(in-package :irl-2)

;; ############################################################################
;; check-irl-program
;; ----------------------------------------------------------------------------

(defun check-irl-program (irl-program primitive-inventory)
  "Checks irl-program for mistakes"
  (let* ((variables (remove-duplicates
                     (find-all-anywhere-if #'variable-p irl-program)))
         (bindings (loop for var in variables
                         collect (make-instance 'binding :var var)))
         (bind-statements (find-all 'bind irl-program :key #'first))
         (primitives-in-program (set-difference irl-program bind-statements)))
    
    ;; first check, everything should be a non-empty list
    (loop for expr in irl-program
          unless (and (listp expr) expr)
          do (error "The expression should be a non-empty list,~%got: ~a." expr))

    ;; next we check all bind statements
    (loop for bind-statement in bind-statements
          do (if (not (= (length bind-statement) 4))
               (error "Expected four element bind statement, ~%got: ~a" bind-statement)
               (destructuring-bind (bind type variable value-expr) bind-statement
                 (declare (ignore bind))
                 (when (not (variable-p variable))
                   (error "Expected variable identifier in ~a,~%got: ~a."
                          bind-statement variable))
                 (let ((value 
                        (or (when (typep value-expr 'entity) value-expr)
                            (when (symbolp value-expr)
                              (or (find-entity-by-id (ontology primitive-inventory) value-expr)
                                  (error "Could not find an entity with id ~a in ontology" value-expr)))
                            (error
                             "Expected symbol or entity as value in ~a:~%got: ~a"
                             bind-statement value-expr))))
                   (unless (typep value 'entity)
                     (if (symbolp value-expr)
                       (error "Value ~a returned by find-entity-by-id is not an entity"
                              value)
                       (error "Value ~a is not an entity"
                              value)))
                   (unless (typep value type)
                     (if (symbolp value-expr)
                       (error "Value ~a returned by find-entity-by-id for ~a is not of type ~a"
                              value value-expr type))
                     (error "Value ~a is not of type ~a"
                            value type))
                   (when (null value)
                     (error "Could not read the value expression~%  ~a." value-expr))
                   (when (not (typep value type))
                     (error "The type of the value does not match the ~
                             defined type:~%- value: ~a (~a)~%- defined type: ~a"
                            value (type-of value) type))
                   (let ((binding (find variable bindings :key #'var)))
                     (when (or (score binding)
                               (value binding))
                       (error "~a is bound multiple times in bind-statements."
                              variable))
                     (setf (score binding) 1.0)
                     (setf (value binding) value))))))
    
    ;; lastly we check all primitives
    (loop for expr in primitives-in-program
          for variables = (cdr expr)
          unless (= (length variables) (length (remove-duplicates variables)))
          do (error "In ~a variables appear at least twice as argument." expr)
          ;; primitive must be found
          unless (find-primitive (first expr) primitive-inventory)
          do (error "Primitive ~a is not defined " (car expr))
          do
          (let ((primitive (find-primitive (first expr) primitive-inventory)))            
            ;; check that the number of variables matches the
            ;; number of slot-specs:
            (unless (= (length (slot-specs primitive))
                       (length variables))
              (error "Error while reading primitive expression~%  ~a.~
                      ~%The number of given variables does not match ~
                      the number of slots."
                     expr))
            ;; check that the given parameters are proper variable identifiers:
            (loop for var in variables
                  unless (variable-p var)
                  do (error "Error while reading primitive expression~%  ~a.~
                             ~%Expected variable identifier, got ~a."
                            expr var))
            ;; check the type of the variable bindings
            (loop with slot-specs = (slot-specs primitive)
                  for slot-spec in slot-specs
                  for var in variables
                  for value = (value (find var bindings :key #'var))
                  for expected-type = (slot-spec-type slot-spec)
                  unless (or (null value)
                             (subtypep (type-of value) expected-type))
                  do (error "Expected value of type ~a in ~a for ~a,~%got: ~a"
                            expected-type expr var value))))
    ;; all test succeeded, return t
    t))

;; ############################################################################
;; node-tests
;; ----------------------------------------------------------------------------

(defgeneric run-node-tests (node primitive-inventory)
  (:documentation "Runs all node tests on the given node. All of them must return t."))

(defmethod run-node-tests ((node irl-program-processor-node)
                           (primitive-inventory primitive-inventory))
  (loop for mode in (get-configuration primitive-inventory :node-tests)
        always (node-test node mode)))

(defgeneric node-test (node mode)
  (:documentation "Runs the node test specified by mode on the node"))
                      

;; ############################################################################
;; goal-tests
;; ----------------------------------------------------------------------------

(defgeneric run-goal-tests (node primitive-inventory)
  (:documentation "Runs all goad tests on the given node. All of them must return t."))

(defmethod run-goal-tests ((node irl-program-processor-node)
                           (primitive-inventory primitive-inventory))
  (loop for mode in (get-configuration primitive-inventory :goal-tests)
        always (goal-test node mode)))

(defgeneric goal-test (node mode)
  (:documentation "Runs the goal test specified by mode on the node"))

(defmethod goal-test ((node irl-program-processor-node)
                      (mode (eql :no-primitives-remaning)))
  (null (primitives-remaining node)))

(defmethod goal-test ((node irl-program-processor-node)
                      (mode (eql :all-variables-bound)))
  (loop for binding in (bindings node)
        never (null (value binding))))

;; ############################################################################
;; next-primitive
;; ----------------------------------------------------------------------------

(defgeneric run-next-primitive (node primitive-inventory)
  (:documentation "Returns the next primitive to try, depending on the configuration"))

(defmethod run-next-primitive ((node irl-program-processor-node)
                               (primitive-inventory primitive-inventory))
  (next-primitive node (get-configuration primitive-inventory :next-primitive)))

(defgeneric next-primitive (node mode)
  (:documentation "Return the next primitive, according to the mode"))

(defmethod next-primitive ((node irl-program-processor-node)
                           (mode (eql :random)))
  (random-elt (primitives-remaining node)))
                    
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
              :initform nil))
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
    :initform nil)))


(define-event evaluate-irl-program-started
  (irl-program list) (primitive-inventory primitive-inventory))

(define-event evaluate-irl-program-finished
  (solutions list) (evaluation-tree irl-program-processor)
  (primitive-inventory primitive-inventory))


(defun duplicate-solution-p (node solutions)
  (loop for solution in solutions
        never (loop for var in (mapcar #'var (bindings node))
                    for solution-value = (value (find var solution :key #'var))
                    for node-binding-value = (value (find var (bindings node) :key #'var))
                    always (or (and (null solution-value)
                                    (null node-binding-value))
                               (and solution-value
                                    node-binding-value
                                    (equal-entity node-binding-value solution-value))))))


;; main entry point
(defun evaluate-irl-program (irl-program &key (primitive-inventory *irl-primitives*) (silent nil))
  ;; check if there is an ontology to work with
  (unless (ontology primitive-inventory)
    (error "There is no useable ontology. Provide an ontology in the primitive-inventory"))
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
    (when (get-configuration primitive-inventory :check-irl-program)
      (check-irl-program irl-program primitive-inventory))
    (let* ((queue nil)
           (processor
            (make-instance 'irl-program-processor :irl-program irl-program
                           :primitive-inventory primitive-inventory
                           :solutions nil))
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
                          :primitives-evaluated-w/o-result nil)))

      ;; notify the start of processing
      (unless silent
        (notify evaluate-irl-program-started irl-program primitive-inventory))
      
      ;; push the initial node
      (add-node processor initial-node)
      
      ;; process the initial node
      (cond
       ((duplicate-solution-p initial-node (solutions processor))
        (setf (status initial-node) 'duplicate))
       ((run-goal-tests initial-node primitive-inventory)
        (setf (status initial-node) 'solution)
        (push (bindings initial-node) (solutions processor)))
       ((primitives-remaining initial-node)
        (push initial-node queue))
       (t (setf (status initial-node) 'no-primitives-remaining)))
      
      ;; run the queue
      (when queue
        (loop
         
         ;; choose the next primitive and evaluate it
         for current-node = (pop queue)
         for current-primitive = (run-next-primitive current-node primitive-inventory)
         for result = (evaluate-primitive-in-program current-primitive
                                                     (bindings current-node)
                                                     (ontology primitive-inventory))
         
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
                                                  :processor processor)))
                     (add-node processor new-node :parent current-node)))
                  
                  (;no results
                   (null result)
                   (let ((remaining-primitives (remove current-primitive
                                                       (primitives-remaining current-node))))
                     (cond
                      (remaining-primitives
                       (when (not (eq (status current-node) 'initial))
                         (setf (status current-node) 'primitives-remaining))
                       (setf (primitives-remaining current-node) remaining-primitives)
                       (setf (primitives-evaluated-w/o-result current-node)
                             (push current-primitive 
                                   (primitives-evaluated-w/o-result current-node)))
                       (pushend current-node queue))
                      (t (setf (status current-node) 'no-primitives-remaining)
                         (push current-primitive 
                               (primitives-evaluated-w/o-result current-node))
                         (setf (primitives-remaining current-node) nil)))))
                  
                  (;result to process
                   (and (listp result) result)
                   (let ((remaining-primitives 
                          (append
                           (remove current-primitive
                                   (primitives-remaining current-node))
                           (primitives-evaluated-w/o-result current-node)))
                         (evaluated-primitives (cons current-primitive
                                                     (primitives-evaluated current-node))))
                     (loop for res in result
                           for new-node = (make-instance
                                           'irl-program-processor-node
                                           :status 'no-primitives-remaining
                                           :bindings res
                                           :primitives-evaluated evaluated-primitives
                                           :primitives-remaining remaining-primitives
                                           :primitives-evaluated-w/o-result nil
                                           :processor processor)
                           do (add-node processor new-node :parent current-node)
                           (cond
                            ((duplicate-solution-p new-node (solutions processor))
                             (setf (status new-node) 'duplicate))
                            (remaining-primitives   
                             (setf (status new-node) 'primitives-remaining)
                             (push new-node queue))
                            ((run-goal-tests new-node primitive-inventory)
                             (setf (status new-node) 'solution)
                             (push res (solutions processor))))))))
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

      