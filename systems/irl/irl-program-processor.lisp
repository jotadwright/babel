(in-package :irl)
                                     
;; ############################################################################
;; primitive-inventory-processor, pip-node
;; ----------------------------------------------------------------------------

(defclass primitive-inventory-processor (tree)
  ((primitive-inventory
    :type primitive-inventory :initarg :primitive-inventory
    :accessor primitive-inventory
    :initform (error "please provide a :primitive-inventory")
    :documentation "the primitive inventory to apply")
   (irl-program
    :type list :accessor irl-program :initarg :irl-program
    :initform (error "please provide a :irl-program")
    :documentation "The irl-program being processed")
   (queue
    :documentation "The queue that drives the search process"
    :accessor queue :initarg :queue
    :initform nil :type list)
   (node-counter
    :documentation "Counter for the number of nodes"
    :accessor node-counter :initarg :node-counter
    :initform 0 :type number)
   (succeeded-nodes
    :type list :initform nil :accessor succeeded-nodes
    :documentation "all succeeded nodes of the search process")
   (ontology
    :type blackboard :initform (make-blackboard)
    :initarg :ontology :accessor ontology
    :documentation "The ontology used during processing")
   (data
     :type blackboard :initform (make-blackboard)
     :initarg :blackboard :accessor blackboard))
  (:documentation "The PIP handles the evaluation of an irl program"))

(defmethod initialize-instance :after ((pip primitive-inventory-processor)
                                       &key primitive-inventory irl-program ontology)
  (let ((irl-program
         ;; replace all non-variables with variables
         ;; and introduce bind-statements for all of them
         (loop for item in irl-program
               if (eq (first item) 'bind)
               collect item ;; bind statement
               else if (apply #'always (mapcar #'variable-p (cdr item)))
               collect item ;; no non-variables
               else append (loop
                            with new-item = (list (car item))
                            with bind-statements
                            for parameter in (cdr item)
                            if (variable-p parameter)
                            do (push parameter new-item)
                            else
                            do (let ((var (make-var 'var))
                                     (value (if (symbolp parameter)
                                              (find-entity-by-id ontology parameter)
                                              parameter)))
                                 (push var new-item)
                                 (push
                                  `(bind ,(type-of value) ,var ,value)
                                  bind-statements))
                            finally
                            (return (cons (reverse new-item) bind-statements))))))
    ;; when set, check the irl program for mistakes before evaluating it
    (when (get-configuration pip :check-irl-program-before-evaluation)
      (check-irl-program irl-program ontology primitive-inventory))
    ;; create the initial node, add it to the tree and enqueue it
    (let* ((all-variables
            (remove-duplicates (all-variables irl-program)))
           (bind-statements (all-bind-statements irl-program))
           (irl-program-w/o-bind-statements 
            (set-difference irl-program bind-statements))
           (bindings-through-bind-statements
            (evaluate-bind-statements bind-statements ontology))
           (bindings-for-unbound-variables 
            (loop for var in (set-difference all-variables 
                                             (mapcar #'var bindings-through-bind-statements))
                  collect (make-instance 'binding :var var)))
           (bindings (append bindings-for-unbound-variables
                             bindings-through-bind-statements))
           (initial-node
            (make-instance 'pip-node
                           :primitive-inventory primitive-inventory
                           :pip pip :statuses '(initial)
                           :bindings bindings
                           :per (make-primitive-evaluation-result
                                 :status 'initial
                                 :source-bindings bindings
                                 :evaluated-primitive nil
                                 :resulting-bindings bindings)
                           :primitive-under-evaluation nil
                           :primitives-evaluated nil
                           :primitives-remaining irl-program-w/o-bind-statements
                           :created-at 0 :depth 0)))
      (add-node pip initial-node)
      (setf (queue pip) (list initial-node)))))

(defmethod create-primitive-inventory-processor ((irl-program list) (ontology blackboard)
                                                 (primitive-inventory primitive-inventory)
                                                 &key &allow-other-keys)
  (make-instance 'primitive-inventory-processor
                 :primitive-inventory primitive-inventory
                 :irl-program irl-program
                 :ontology ontology))

(defmethod copy-object ((pip primitive-inventory-processor))
  pip)

(defmethod get-configuration ((pip primitive-inventory-processor) key &key)
  (get-configuration (primitive-inventory pip) key))





(defclass pip-node (tree-node)
  ((primitive-inventory
    :type primitive-inventory :initarg :primitive-inventory
    :accessor primitive-inventory
    :initform (error "please provide a :primitive-inventory")
    :documentation "the primitive inventory to apply")
   (pip
    :documentation "A pointer to the processor"
    :accessor pip :initarg :pip)
   (statuses
    :documentation "Status of the node."
    :accessor statuses :initarg :statuses
    :initform nil :type symbol)
   (primitive-supplier
    :documentation "an object that can be asked to return the next primitive"
    :type t :initform nil :accessor primitive-supplier)
   (primitive-evaluated
    :type symbol :accessor primitive-evaluated
    :initarg :primitive-evaluated :initform t
    :documentation "t when primitive under evaluation has been evaluated")
   (primitive-under-evaluation
    :documentation "The primitive being evaluated in this node"
    :accessor primitive-under-evaluation :type list
    :initarg :primitive-under-evaluation :initform nil)
   (primitives-evaluated
    :documentation "List of evaluated primitives"
    :accessor primitives-evaluated :type list
    :initarg :primitives-evaluated :initform nil)
   (primitives-remaining
    :documentation "List of remaining primitives"
    :accessor primitives-remaining :type list
    :initarg :primitives-remaining :initform nil)
   (bindings
    :documentation "Available bindings at this point in processing"
    :accessor bindings :initarg :bindings :type list)
   (per  ; primitive evaluation result
    :documentation "the result of primitive evaluated"
    :type primitive-evaluation-result :initarg :per
    :accessor pipn-per)
   (priority
    :type number :initarg :priority :initform 1.0 :accessor priority
    :documentation "the higher, the more in front in the queue")
   (goal-test-data
    :type blackboard :accessor goal-test-data :initform (make-blackboard)
    :documentation "goal tests can store any information here")
   (fully-expanded?
    :type t :initarg :fully-expanded? :initform nil :accessor fully-expanded?
    :documentation "whether a node has been fully expanded")
   (duplicate
    :type (or null pip-node) :initform nil :accessor duplicate
    :documentation "the node that this node is a duplicate of")
   (created-at
    :type number :initarg :created-at :accessor created-at
    :documentation "the number of previous search tree expansions
                    when this node was created")
   (depth
    :type number :initarg :depth :accessor depth
    :documentation "the depth of the node in the tree"))
  (:documentation "represents a node in the search tree"))

(defmethod all-parents ((node pip-node))
  "Returns all parents of a pip-node, with the
   most recent parent first in the list. The intial
   node will be last."
  (reverse (parents node)))

(defmethod upward-branch ((node pip-node) &key (include-initial t))
  (cons node (if include-initial
               (all-parents node)
               (butlast (all-parents node)))))

(defun initial-node-p (node)
  "Checks if a node is the initial node"
  (= (created-at node) 0))

(defmethod get-configuration ((node pip-node) key &key)
  (get-configuration (pip node) key))

(defmethod print-object ((node pip-node) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<pipn ~a, ~a, ~a,~:_ statuses: ~(~a~),~:_ evaluated: ~a,~:_ per: ~:w>"
              (created-at node) (priority node) (fully-expanded? node)
              (statuses node) (primitive-under-evaluation node)
              (pipn-per node)))
    (format stream "<pipn ~a, ~a, ~a: ~{~(~a~)~^, ~}>"
            (created-at node) (priority node)
            (fully-expanded? node) (statuses node))))

(defmethod copy-object ((node pip-node))
  node)

(defgeneric traverse-depth-first (tree-or-node &key collect-fn do-fn &allow-other-keys)
  (:documentation "Traverses the given tree (or starting form a given
  node) top down in a depth first fashion. The user has to supply
  either a collect-fn or a do-fn which will be called on each
  individual node."))

(defmethod traverse-depth-first :before ((node primitive-inventory-processor) 
					 &key (collect-fn nil) (do-fn nil)
                                         &allow-other-keys)
  (unless (or do-fn collect-fn)
    (error "You need to supply a collect-fn or a do-fn")))

(defmethod traverse-depth-first ((node pip-node)
                                 &key (collect-fn nil)
                                 (do-fn nil)
                                 &allow-other-keys)
  (if do-fn
    (progn
      (funcall do-fn node)
      (loop for child in (children node)
            do (traverse-depth-first child :collect-fn collect-fn :do-fn do-fn)))
    ;; else there is a collect-fn
    (cons (funcall collect-fn node)
          (loop for child in (children node)
                append (traverse-depth-first child :collect-fn collect-fn :do-fn do-fn)))))

(defmethod traverse-depth-first ((tree primitive-inventory-processor) 
				 &key (collect-fn nil) (do-fn nil)
                                 &allow-other-keys)
  (traverse-depth-first (top tree) :collect-fn collect-fn :do-fn do-fn))


;; #############################################################################
;; hooks for search customization
;; -----------------------------------------------------------------------------

(defgeneric create-primitive-supplier (node mode)
  (:documentation "Creates and returns a list of primtitives for a new node"))

(defgeneric next-primitive (primitive-supplier node)
  (:documentation "Returns the next primtitive to try from a pool"))

(defgeneric expand-pip-node (node mode)
  (:documentation "Returns children to be queued"))

(defgeneric pip-node-test (node mode)
  (:documentation "Tests whether a node should be further explored"))

(defgeneric pip-goal-test (node mode)
  (:documentation "Tests whether a pip node is a solution"))

(defgeneric pip-enqueue (node pip mode)
  (:documentation "Puts a node into the queue"))

(defgeneric pip-heuristic-value (node mode)
  (:documentation "Computes the heuristic value of a pip-node."))

(defgeneric apply-pip-heuristic (node mode)
  (:documentation "Applies a single heuristic to a node and returns a score."))

;; #############################################################################
;; pip-add-child
;; -----------------------------------------------------------------------------

(defgeneric pip-add-child (pip-node primitive-evaluation-result &key)
  (:documentation "Creates and adds a new children based on the given node
                   and pers to the given node"))

(defmethod pip-add-child ((node pip-node) (per primitive-evaluation-result)
                          &key (primitive-evaluated t))
  ;; a per can have multiple results, leading to multiple children
  (let ((child
         (make-instance 'pip-node
                        :primitive-evaluated primitive-evaluated
                        :primitive-inventory (primitive-inventory node)
                        :statuses (list (per-status per))
                        :per per :pip (pip node)
                        :bindings (per-resulting-bindings per)
                        :created-at (incf (node-counter (pip node)))
                        :depth (1+ (depth node))
                        :primitive-under-evaluation (per-evaluated-primitive per)
                        :primitives-evaluated (when (primitive-under-evaluation node)
                                                (cons (primitive-under-evaluation node)
                                                      (primitives-evaluated node)))
                        :primitives-remaining (remove (per-evaluated-primitive per)
                                                      (primitives-remaining node)
                                                      :test #'equal))))
    (add-node (pip node) child :parent node)
    child))

;; #############################################################################
;; pip-heuristic-value
;; -----------------------------------------------------------------------------

(defmethod pip-heuristic-value ((node pip-node) (mode (eql :sum-heuristics-and-parent)))
  "Heuristic value of a pip-node is the sums of the results of all heuristic functions
   applied to the node and the heuristic value of the parent."
  (loop for heuristic in (get-configuration (pip node) :heuristics)
        sum (apply-pip-heuristic node heuristic) into heuristic-value
        finally
        (return (+ (priority (parent node))
                   heuristic-value))))

(defmethod pip-heuristic-value ((node pip-node) (mode (eql :sum-heuristics)))
  "Heuristic value of a pip-node is the sums of the results of all heuristic functions
   applied to the node."
  (loop for heuristic in (get-configuration (pip node) :heuristics)
        sum (apply-pip-heuristic node heuristic)))


;; #############################################################################
;; apply-pip-heuristic
;; -----------------------------------------------------------------------------

(defmethod apply-pip-heuristic ((node pip-node) (mode (eql :nr-of-evaluated-primitives)))
  1)

(defmethod apply-pip-heuristic ((node pip-node) (mode (eql :sum-of-new-binding-scores)))
  "Take the sum of the scores of the newly created bindings in this node."
  (loop for resulting-binding in (per-resulting-bindings (pipn-per node))
        for source-binding in (per-source-bindings (pipn-per node))
        when (and (null (value source-binding))
                  (value resulting-binding))
        sum (score resulting-binding)))
                         
(defmethod apply-pip-heuristic ((node pip-node) (mode (eql :sum-of-all-binding-scores)))
  (loop for binding in (bindings node)
        when (score binding)
        sum (score binding)))

(defmethod apply-pip-heuristic ((node pip-node) (mode (eql :average-of-all-binding-scores)))
  (loop for binding in (bindings node)
        when (score binding)
        collect (score binding) into score-list
        finally
        (return (average score-list))))
      
;; #############################################################################
;; pip-enqueue
;; -----------------------------------------------------------------------------

(defmethod pip-enqueue ((node pip-node) (pip primitive-inventory-processor)
                        (mode (eql :depth-first)))
  "Depth first search: children always added in front of queue."
  (push node (queue pip)))

(defmethod pip-enqueue ((node pip-node) (pip primitive-inventory-processor)
                        (mode (eql :breadth-first)))
  "Breadth first search: children always added at the back of the queue."
  (pushend node (queue pip)))

(defmethod pip-enqueue ((node pip-node) (pip primitive-inventory-processor)
                        (mode (eql :best-first)))
  "Adds new nodes to the queue based on their heuristic value (best-first)."
  (setf (priority node)
        (if (initial-node-p node)
          1.0
          (pip-heuristic-value node (get-configuration pip :heuristic-value-mode))))
  (unless (priority node)
    (error "The heuristic value of the new node is NIL. You used heuristic value
            mode ~a. Please check why it did not calculate a score."
           (get-configuration pip :heuristic-value-mode)))
  (setf (queue pip) (sorted-insert (queue pip) node :key #'priority :test #'>))) 


;; #############################################################################
;; expand-pip-node
;; -----------------------------------------------------------------------------

(defmethod expand-pip-node ((node pip-node) (mode (eql :single-expansion)))
  "Take the next primitive for evaluation, evaluate it and
   return the successful result(s)."
  (loop
   with nodes-to-queue = nil
   with failed-nodes = nil
   with primitive-inventory = (primitive-inventory node)
   for primitive = (next-primitive (primitive-supplier node) node)
   when primitive
   do (multiple-value-bind (succeeded-pers failed-pers)
          (evaluate-primitive-in-program primitive (bindings node)
                                         (ontology (pip node))
                                         primitive-inventory)
        (loop for per in succeeded-pers
              do (push (pip-add-child node per :primitive-evaluated t)
                       nodes-to-queue))
        (loop for per in failed-pers
              do (push (pip-add-child node per :primitive-evaluated nil)
                       failed-nodes)))
   when nodes-to-queue do (return nodes-to-queue)
   while primitive
   finally (setf (fully-expanded? node) t)))

(defmethod expand-pip-node ((node pip-node) (mode (eql :full-expansion)))
  "Take ALL next primitives for evaluation, evaluate them
   and return the successful result(s)."
  (loop
   with nodes-to-queue = nil
   with failed-nodes = nil
   with primitive-inventory = (primitive-inventory node)
   for primitive = (next-primitive (primitive-supplier node) node)
   when primitive
   do (multiple-value-bind (succeeded-pers failed-pers)
          (evaluate-primitive-in-program primitive (bindings node)
                                         (ontology (pip node))
                                         primitive-inventory)
        (loop for per in succeeded-pers
              do (push (pip-add-child node per :primitive-evaluated t)
                       nodes-to-queue))
        (loop for per in failed-pers
              do (push (pip-add-child node per :primitive-evaluated nil)
                       failed-nodes)))
   while primitive
   finally
   (setf (fully-expanded? node) t)
   (return nodes-to-queue)))

;; #############################################################################
;; next-pip-solution
;; -----------------------------------------------------------------------------

(define-event pip-started (pip primitive-inventory-processor))
(define-event pip-next-node (pipn pip-node))
(define-event pip-node-expanded (pipn pip-node))
(define-event pip-finished (solution t) (pip primitive-inventory-processor))

(defun get-last-pip-node (pip) 
  "Helper function: extract last node that is consulted from a
   pip. Useful when there is no solution."
  (the-biggest #'created-at (nodes pip)))

(defgeneric next-pip-solution (pip &key notify)
  (:documentation "Runs the primitive inventory processor search until
   the next solution is found."))

(defmethod next-pip-solution ((pip primitive-inventory-processor) &key (notify t))
  "Get the next solution"
  (when notify (notify pip-started pip))
  (loop
   with solution = nil
   with search-algorithm = (get-configuration pip :search-algorithm)
   for node = (pop (queue pip))
   when node
   ;; check if the node has a primitive supplier
   do (unless (primitive-supplier node)
        (setf (primitive-supplier node)
              (create-primitive-supplier
               node (get-configuration pip :primitive-supplier-mode))))
   (when notify (notify pip-next-node node))

   ;; expand the node
   ;; run node tests on each child
   ;; and enqueue when it passes
   (loop for child in (expand-pip-node
                       node (get-configuration pip :node-expansion-mode))
         when (pip-run-node-tests child pip)
         do (pip-enqueue child pip search-algorithm))

   ;; run the goal tests
   (let ((goal-test-succeeded? (pip-run-goal-tests node pip)))
     ;; when succeeded, set the solution and stop
     (when goal-test-succeeded?
       (setf solution node))
     ;; when node is not fully expanded or the goal
     ;; test failed, enqueue it again
     (unless (or (fully-expanded? node)
                 goal-test-succeeded?)
       (pip-enqueue node pip search-algorithm)))

   (when notify (notify pip-node-expanded node))
   until (or solution (not (queue pip)))
   finally
   ;; when no solution was found, get the last node,
   ;; run the goal test on it and return it
   (unless (or solution (succeeded-nodes pip))
     (setf solution (get-last-pip-node pip))
     (progn
       (pip-run-goal-tests solution pip)
       (push 'goal-test-failed (statuses solution))))
   (when notify (notify pip-finished solution pip))
   (return (values solution pip))))


;; #############################################################################
;; primitive-apply
;; -----------------------------------------------------------------------------

(defmethod primitive-apply ((irl-program list) (ontology blackboard)
                            (primitive-inventory primitive-inventory)
                            &key (notify t))
  "Apply primitives until a solution is found
   (or the end is reached without a solution)."
  (next-pip-solution
   (create-primitive-inventory-processor
    irl-program ontology
    primitive-inventory)
   :notify notify))

(define-event primitive-apply-with-n-solutions-started
  (n t) (irl-program list) (ontology blackboard)
  (primitive-inventory primitive-inventory))

(define-event primitive-apply-with-n-solutions-finished
  (solutions t) (pip primitive-inventory-processor))
           
(defun primitive-apply-with-n-solutions (irl-program ontology primitive-inventory n &key (notify t))
  "Return the first N solutions"
  (assert (or (not n) (> n 0)))
  ;; notify when requested
  (when notify
    (notify primitive-apply-with-n-solutions-started
            n irl-program ontology primitive-inventory))
  ;; the main loop
  (loop
   with solutions = nil
   with pip = nil
   initially (multiple-value-bind (solution new-pip)
                 (primitive-apply irl-program ontology primitive-inventory :notify notify)
               (when solution (push solution solutions))
               ;; when this 'solution' is not an actual solution,
               ;; push it manually onto the succeeded nodes anyway
               ;; otherwise, next-pip-solution will keep applying
               ;; the goal test to this node again and again...
               (when (find 'goal-test-failed (statuses solution))
                 (push solution (succeeded-nodes new-pip)))
               (setf pip new-pip))
   for i from 2 to (or n 32000)  ; apparantly, 32000 is the max allowed number of solutions :-)
   for (solution new-pip) = (multiple-value-list (next-pip-solution pip :notify notify))
   do (setf pip new-pip)
   while solution do (setf solutions (append solutions (list solution)))
   finally
   (when notify (notify primitive-apply-with-n-solutions-finished solutions pip))
   (return (values solutions pip))))

(defun primitive-apply-exhaustively (irl-program ontology primitive-inventory &key (notify t))
  "Return all solutions"
  (primitive-apply-with-n-solutions
   irl-program ontology
   primitive-inventory nil
   :notify notify))