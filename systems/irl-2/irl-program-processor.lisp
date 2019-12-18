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
  ((status :documentation "Status of the node. Possible statuses are:
                           initial, primitives-remaining, inconsistent,
                           no-primitives-remaining, solution or duplicate"
           :accessor status :initarg :status :initform 'initial :type symbol)
   (bindings :documentation "Available bindings at this point in processing"
             :accessor bindings :initarg :bindings)
   (primitives-evaluated
    :documentation "List of evaluated primitives"
    :accessor primitives-evaluated
    :initarg :primitives-evaluated :initform nil)
   (primitives-remaining
    :documentation "List of remaining primitives"
    :accessor primitives-remaining
    :initarg :primitives-remaining :initform nil)
   (primitives-evaluated-w/o-result
    :documentation "List of primitives evaluated without result"
    :accessor primitives-evaluated-w/o-result
    :initarg :primitives-evaluated-w/o-result :initform nil)
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


;; main entry point
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
      