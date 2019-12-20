(in-package :irl-2)
                                     
;; ############################################################################
;; irl-program-processor
;; ----------------------------------------------------------------------------

(defclass irl-program-processor (tree)
  ((irl-program :documentation "The irl-program being processed"
                :accessor irl-program :initarg :irl-program
                :initform nil)
   (primitive-inventory :documentation "A pointer to the inventory"
                        :accessor primitive-inventory
                        :initarg :primitive-inventory
                        :initform nil)
   (solutions :documentation "Solutions, i.e. lists of lists of bindings"
              :accessor solutions :initarg :solutions
              :initform nil)
   (node-counter :documentation "Counter for the number of nodes"
                 :accessor node-counter :initarg :node-counter
                 :initform 0 :type number))
  (:documentation "The IPP handles the evaluation of an irl program"))


(defclass irl-program-processor-node (tree-node)
  ((status :documentation "Status of the node."
           :accessor status :initarg :status :initform 'initial :type symbol)
   (bindings :documentation "Available bindings at this point in processing"
             :accessor bindings :initarg :bindings)
   (primitive-under-evaluation
    :documentation "The primitive being evaluated in this node"
    :accessor primitive-under-evaluation
    :initarg :primitive-under-evaluation :initform nil)
   (primitives-evaluated
    :documentation "List of evaluated primitives"
    :accessor primitives-evaluated
    :initarg :primitives-evaluated :initform nil)
   (primitives-remaining
    :documentation "List of remaining primitives"
    :accessor primitives-remaining
    :initarg :primitives-remaining :initform nil)
   (irl-program-processor
    :documentation "A pointer to the processor"
    :accessor processor :initarg :processor
    :initform nil)
   (created-at
    :documentation "The number of the node"
    :accessor created-at :initarg :created-at
    :type number :initform 0)
   (node-depth
    :documentation "The depth of the node"
    :accessor node-depth :initarg :node-depth
    :type number :initform 0)))


(define-event evaluate-irl-program-started
  (irl-program list) (primitive-inventory primitive-inventory))

(define-event evaluate-irl-program-finished
  (solutions list) (evaluation-tree irl-program-processor)
  (primitive-inventory primitive-inventory))

;;;; The new and improved(?) IRL search process
;; We make use of a priority-queue (PQ). Here, the nodes can be
;; ordered according to the :queue-mode (e.g. depth-first).
;; Each node has the following information: primitive-under-evaluation, primitives-evaluated and primitives-remaining.

;; For every node, we do the following steps:
;; 1. evaluate the primitive-under-evaluation
;; 2. run the node-tests
;; 3. run the goal-tests
;; 4. if valid node and not solution, process the results
;;    -> result = inconsistent; change the status of the node and take the next one in the PQ
;;    -> result = empty; change the status of the node and take the next one in the PQ
;;    -> result = non-empty; expand the node (adding new nodes to the PQ)

(defun make-child-node (parent processor next-primitive &optional result)
  (make-instance 'irl-program-processor-node
                 :status 'to-evaluate
                 :bindings (if result result (bindings parent))
                 :primitive-under-evaluation next-primitive
                 :primitives-evaluated (remove nil (cons (primitive-under-evaluation parent) (primitives-evaluated parent)))
                 :primitives-remaining (remove next-primitive (primitives-remaining parent))
                 :irl-program-processor processor
                 :created-at (incf (node-counter processor))
                 :node-depth (+ (node-depth parent) 1)))

(defmethod enqueue-ippn-nodes (nodes queue (mode (eql :random-depth-first)))
  "Add the nodes to the front of the queue, randomly ordered"
  (loop for node in (shuffle nodes)
        do (push node queue)))

(defun expand-node (parent queue processor primitive-inventory &optional results)
  ;; create the child nodes
  (let ((child-nodes
         (if results
           (loop for p in (primitives-remaining parent)
                 append (loop for r in results
                              collect (make-child-node parent processor p r)))
           (loop for p in (primitives-remaining parent)
                 collect (make-child-node parent processor p)))))
    ;; add them to the tree
    (when child-nodes
      (loop for child in child-nodes
            do (add-node child processor :parent parent))
      ;; add them to the queue, taking into account queue-mode and priority-mode...
      (enqueue-ippn-nodes child-nodes queue (get-configuration primitive-inventory :queue-mode)))))

(defun evaluate-irl-program (irl-program &key (primitive-inventory *irl-primitives*) (silent nil))
    ;; check if there is an ontology to work with
  (unless (ontology primitive-inventory)
    (error "There is no ontology. Provide an ontology in the primitive-inventory"))
  (unless (fields (ontology primitive-inventory))
    (error "The ontology appears to be empty. Cannot evaluate an irl-program with an empty ontology"))
  ;; replace all non-variables with variables
  ;; and introduce bind-statements for all of them
  (let ((irl-program
         (loop for item in irl-program
               if (eq (first item) 'bind)
               collect item ;; bind statement
               else if (apply #'always (mapcar #'variable-p (cdr item)))
               collect item ;; no non-variables
               else
               append (loop with new-item = (list (car item))
                            with bind-statements
                            for parameter in (cdr item)
                            if (variable-p parameter)
                            do (push parameter new-item)
                            else
                            do (let ((var (make-var 'var))
                                     (value (if (symbolp parameter)
                                              (find-entity-by-id (ontology primitive-inventory) parameter)
                                              parameter)))
                                 (push var new-item)
                                 (push
                                  `(bind ,(type-of value) ,var ,value)
                                  bind-statements))
                            finally
                            (return (cons (reverse new-item) bind-statements))))))
    ;; when set, check the irl program for mistakes before evaluating it
    (when (get-configuration primitive-inventory :check-irl-program-before-evaluation)
      (check-irl-program irl-program primitive-inventory))
    (let* ((queue nil)
           (processor
            (make-instance 'irl-program-processor :irl-program irl-program
                           :primitive-inventory primitive-inventory
                           :solutions nil :node-counter 0))
           (all-variables
            (remove-duplicates (find-all-anywhere-if #'variable-p irl-program)))
           (bind-statements (find-all 'bind irl-program :key #'first))
           (irl-program-w/o-bind-statements 
            (set-difference irl-program bind-statements))
           (bindings-through-bind-statements
            (evaluate-bind-statements bind-statements (ontology primitive-inventory)))
           (bindings-for-unbound-variables 
            (loop for var in (set-difference all-variables 
                                             (mapcar #'var bindings-through-bind-statements))
                  collect (make-instance 'binding :var var)))
           (bindings (append bindings-for-unbound-variables
                             bindings-through-bind-statements))
           (initial-node (make-instance 'irl-program-processor-node
                          :status 'initial :bindings bindings :processor processor
                          :primitive-under-evaluation nil
                          :primitives-evaluated nil
                          :primitives-remaining irl-program-w/o-bind-statements
                          :created-at 0 :node-depth 0)))

      ;; notify the start of processing
      (unless silent
        (notify evaluate-irl-program-started irl-program primitive-inventory))
      
      ;; push the initial node on the search tree and on the queue
      (add-node processor initial-node)
      (push initial-node queue)

      ;; run the queue
      (when queue
        (loop
         ;; choose the next primitive and evaluate it
         for current-node = (pop queue)
         for current-primitive = (primitive-under-evaluation current-node)
         for results = (when current-primitive
                         (evaluate-primitive-in-program current-primitive
                                                        (bindings current-node)
                                                        primitive-inventory))
         do (cond (;; run the node tests
                   ;; if the node is not valid, do nothing
                   (not (run-node-tests current-node primitive-inventory))
                   nil)
                  (;; run the goal tests
                   ;; if the node is a solution, store the bindings
                   (run-goal-tests current-node primitive-inventory)
                   (setf (status current-node) 'solution)
                   (push (bindings current-node) (solutions processor)))
                  (;; if the node is valid and not a solution
                   ;; process the results
                   t (cond ((eq result 'inconsistent) ; result is inconsistent
                            (setf (status current-node) 'inconsistent))
                           ((null result) ; no results
                            (if (eql (status current-node) 'initial)
                              (expand-node current-node queue processor primitive-inventory)
                              (setf (status current-node) 'primitive-evaluated-w/o-result)))
                           ((and (listp results) results) ; results to process
                            (setf (status node) 'primitive-evaluated)
                            (expand-node current-node queue processor primitive-inventory results)))))
         while queue))
      ;; clean the solutions
      (setf (solutions processor)
            (loop for solution in (solutions processor)
                  if solution collect solution))

      ;; notify the end of processing
      (unless silent
        (notify evaluate-irl-program-finished (solutions processor)
                processor primitive-inventory))

      ;; return solutions and processor
      (values (solutions processor) processor))))
           
#|
(defun evaluate-irl-program (irl-program &key (primitive-inventory *irl-primitives*) (silent nil))
  ;; check if there is an ontology to work with
  (unless (ontology primitive-inventory)
    (error "There is no ontology. Provide an ontology in the primitive-inventory"))
  (unless (fields (ontology primitive-inventory))
    (error "The ontology appears to be empty. Cannot evaluate an irl-program with an empty ontology"))
  ;; replace all non-variables with variables
  ;; and introduce bind-statements for all of them
  (let ((irl-program
         (loop for item in irl-program
               if (eq (first item) 'bind)
               collect item ;; bind statement
               else if (apply #'always (mapcar #'variable-p (cdr item)))
               collect item ;; no non-variables
               else
               append (loop with new-item = (list (car item))
                            with bind-statements
                            for parameter in (cdr item)
                            if (variable-p parameter)
                            do (push parameter new-item)
                            else
                            do (let ((var (make-var 'var))
                                     (value (if (symbolp parameter)
                                              (find-entity-by-id (ontology primitive-inventory) parameter)
                                              parameter)))
                                 (push var new-item)
                                 (push
                                  `(bind ,(type-of value) ,var ,value)
                                  bind-statements))
                            finally
                            (return (cons (reverse new-item) bind-statements))))))
    ;; when set, check the irl program for mistakes before evaluating it
    (when (get-configuration primitive-inventory :check-irl-program-before-evaluation)
      (check-irl-program irl-program primitive-inventory))
    (let* ((queue nil)
           (processor
            (make-instance 'irl-program-processor :irl-program irl-program
                           :primitive-inventory primitive-inventory
                           :solutions nil :node-counter 0))
           (all-variables
            (remove-duplicates (find-all-anywhere-if #'variable-p irl-program)))
           (bind-statements (find-all 'bind irl-program :key #'first))
           (irl-program-w/o-bind-statements 
            (set-difference irl-program bind-statements))
           (bindings-through-bind-statements
            (evaluate-bind-statements bind-statements (ontology primitive-inventory)))
           (bindings-for-unbound-variables 
            (loop for var in (set-difference all-variables 
                                             (mapcar #'var bindings-through-bind-statements))
                  collect (make-instance 'binding :var var)))
           (bindings (append bindings-for-unbound-variables
                             bindings-through-bind-statements))
           (initial-node (make-instance 'irl-program-processor-node
                          :status 'initial :bindings bindings :processor processor
                          :primitives-evaluated nil
                          :primitives-remaining irl-program-w/o-bind-statements
                          :primitives-evaluated-w/o-result nil
                          :created-at 0 :node-depth 0)))

      ;; notify the start of processing
      (unless silent
        (notify evaluate-irl-program-started irl-program primitive-inventory))
      
      ;; push the initial node
      (add-node processor initial-node)
      
      ;; process the initial node
      (cond
       ;; if a node test fails, stop
       ((not (run-node-tests initial-node primitive-inventory)) nil)
       ;; if the goal tests succeed, solution found
       ((run-goal-tests initial-node primitive-inventory)
        (setf (status initial-node) 'solution)
        (push (bindings initial-node) (solutions processor)))
       ;; if the node is valid, but not a solution
       ;; and there are primitives to evaluate
       ;; push it on the queue
       ((primitives-remaining initial-node)
        (push initial-node queue))
       ;; otherwise, stop
       (t (setf (status initial-node) 'no-primitives-remaining)))
      
      ;; run the queue
      (when queue
        (loop
         
         ;; choose the next primitive and evaluate it
         for current-node = (pop queue)
         for current-primitive = (run-primitive-supplier current-node primitive-inventory)
         for result = (evaluate-primitive-in-program current-primitive
                                                     (bindings current-node)
                                                     primitive-inventory)
         
         ;; process the result
         do (cond ((eq result 'inconsistent) ; result is inconsistent
                   (let ((new-node (make-instance 'irl-program-processor-node
                                                  :status 'inconsistent
                                                  :bindings (bindings current-node)
                                                  :primitives-evaluated 
                                                  (cons current-primitive
                                                        (primitives-evaluated current-node))
                                                  :primitives-remaining
                                                  (remove current-primitive
                                                           (primitives-remaining current-node))
                                                  :primitives-evaluated-w/o-result
                                                  (primitives-evaluated-w/o-result current-node)
                                                  :processor processor
                                                  :created-at (incf (node-counter processor))
                                                  :node-depth (+ (node-depth current-node) 1))))
                     ;; add the inconsistent node to the tree
                     (add-node processor new-node :parent current-node)))
                  
                  ((null result) ;no results
                   (let ((remaining-primitives (remove current-primitive
                                                       (primitives-remaining current-node))))
                     (cond
                      ;; if remaining primitives, re-queue the node
                      (remaining-primitives
                       (when (not (eq (status current-node) 'initial))
                         (setf (status current-node) 'primitives-remaining))
                       (setf (primitives-remaining current-node) remaining-primitives)
                       (setf (primitives-evaluated-w/o-result current-node)
                             (push current-primitive 
                                   (primitives-evaluated-w/o-result current-node)))
                       (pushend current-node queue))
                      ;; else, stop
                      (t (setf (status current-node) 'no-primitives-remaining)
                         (push current-primitive 
                               (primitives-evaluated-w/o-result current-node))
                         (setf (primitives-remaining current-node) nil)))))
                  
                  ((and (listp result) result) ;result to process
                   (let ((remaining-primitives 
                          (append
                           (remove current-primitive
                                   (primitives-remaining current-node))
                           (primitives-evaluated-w/o-result current-node)))
                         (evaluated-primitives (cons current-primitive
                                                     (primitives-evaluated current-node))))
                     ;; create a new node for each result
                     (loop for res in result
                           for new-node = (make-instance
                                           'irl-program-processor-node
                                           :status 'no-primitives-remaining
                                           :bindings res
                                           :primitives-evaluated evaluated-primitives
                                           :primitives-remaining remaining-primitives
                                           :primitives-evaluated-w/o-result nil
                                           :processor processor
                                           :created-at (incf (node-counter processor))
                                           :node-depth (+ (node-depth current-node) 1))
                           do (add-node processor new-node :parent current-node)
                           (cond
                            ;; if a node test fails, stop
                            ((not (run-node-tests new-node primitive-inventory)))
                            ;; if the goal tests succeed, solution found
                            ((run-goal-tests new-node primitive-inventory)
                             (setf (status new-node) 'solution)
                             (push res (solutions processor)))
                            ;; if the node is valid, but not a solution
                            ;; and there are primitives to evaluate
                            ;; push it on the queue
                            (remaining-primitives
                             (setf (status new-node) 'primitives-remaining)
                             (push new-node queue))
                            ;; otherwise, stop
                            (t (setf (status initial-node) 'no-primitives-remaining)))))))
         while queue))

      ;; clean the solutions
      (setf (solutions processor)
            (loop for solution in (solutions processor)
                  if solution collect solution))

      ;; notify the end of processing
      (unless silent
        (notify evaluate-irl-program-finished (solutions processor)
                processor primitive-inventory))

      ;; return solutions and processor
      (values (solutions processor) processor))))
|#

;; ############################################################################
;; helper functions
;; ----------------------------------------------------------------------------

(defun irl-program-connected? (irl-program)
  "Checks whether an irl program is connected. Returns t if so, the
   number of sub graphs and the sub graphs themselves"
  (loop with classes = nil
        with sub-networks = nil
        for x in irl-program
        for variables = (find-all-if #'variable-p x)
        for (cs subs) = (multiple-value-list
                         (loop for class in classes
                               for sub-network in sub-networks
                               when (loop for var in variables
                                          thereis (member var class))
                               collect class into cs
                               and
                               collect sub-network into subs
                               finally (return (values cs subs))))
        if cs
        do
        (loop for class in cs do (setf classes (remove class classes)))
        (push (remove-duplicates (reduce #'append (cons variables cs))) classes)
        (loop for sub in subs do (setf sub-networks (remove sub sub-networks)))
        (push (cons x (reduce #'append subs)) sub-networks)
        else
        do
        (push variables classes)
        (push (list x) sub-networks)
        finally
        (return (values
                 (< (length classes) 2)
                 (length classes)
                 sub-networks))))

(defun irl-program-p (thing &key (primitive-inventory *irl-primitives*))
  "returns t if thing conforms to the basic syntax of irl-programs
   list of bind statements (bind ...)  and irl-primitives (primitive ..)"
  (and (listp thing)
       (loop for s in thing
             always (and (listp s)
                         (or (eq (first s) 'bind)
                             (find-primitive (first s) primitive-inventory))))))
      