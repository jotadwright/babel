(in-package :irl)

;; ############################################################################
;; check-irl-program
;; ----------------------------------------------------------------------------

(defun check-irl-program (irl-program ontology primitive-inventory)
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
          do (if (and (not (= (length bind-statement) 4)) (not (= (length bind-statement) 5)))
               (error "Expected four or five element bind statement, ~%got: ~a" bind-statement)
               (destructuring-bind (bind type variable value-expr . available-at) bind-statement
                 (declare (ignore bind))
                 (when (not (variable-p variable))
                   (error "Expected variable identifier in ~a,~%got: ~a."
                          bind-statement variable))
                 (let ((value 
                        (or (when (typep value-expr 'entity) value-expr)
                            (when (symbolp value-expr)
                              (or (find-entity-by-id ontology value-expr)
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