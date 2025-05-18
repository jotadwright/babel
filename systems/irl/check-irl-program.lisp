(in-package :irl)

;; ############################################################################
;; check-irl-program
;; ----------------------------------------------------------------------------

(defun check-irl-program (irl-program ontology primitive-inventory)
  "Checks irl-program for mistakes"
  (let* ((variables
          (remove-duplicates
           (find-all-anywhere-if #'variable-p irl-program)))
         (bindings
          (loop for var in variables
                collect (make-instance 'binding :var var)))
         (bind-statements
          (find-all 'bind irl-program :key #'first))
         (primitives-in-program
          (set-difference irl-program bind-statements)))
    
    ;; first check, everything should be a non-empty list
    (dolist (expr irl-program)
      (unless (and (listp expr) expr)
        (error "The expression should be a non-empty list,~%got: ~a." expr)))

    ;; next we check all bind statements
    (dolist (bind-statement bind-statements)
      (if (and (not (= (length bind-statement) 4)) (not (= (length bind-statement) 5)))
        ;; bind statement has to consists of 4 or 5 elements
        (error "Expected four or five element bind statement, ~%got: ~a" bind-statement)
        (destructuring-bind (bind type variable value-expr . available-at) bind-statement
          (declare (ignore bind available-at))
          ;; check that the third element is a variable
          (when (not (variable-p variable))
            (error "Expected variable identifier in ~a,~%got: ~a."
                   bind-statement variable))
          (let ((value
                 (if (and (subtypep type 'entity) (symbolp value-expr))
                   (or (find-entity-by-id ontology value-expr :type type)
                       (error "Could not find an entity with id ~a in ontology" value-expr))
                   value-expr)))
            (when (not (typep value type))
              (error "The type of the value does not match the ~
                             defined type:~%- value: ~a (~a)~%- defined type: ~a ~
                             ~%- bind-statement: ~a"
                     value (type-of value) type bind-statement))
            ;(when (null value)
            ;  (error "Cannot bind a null value in bind statement ~a" bind-statement))
            (let ((binding (find variable bindings :key #'var)))
              (when (or (score binding) (slot-boundp binding 'value))
                (error "~a is bound multiple times in bind-statements."
                       variable))
              (setf (score binding) 1.0)
              (setf (value binding) value))))))
    
    ;; lastly we check all primitives
    (dolist (expr primitives-in-program)
      (let ((variables (cdr expr)))
        (unless (= (length variables) (length (remove-duplicates variables)))
          (error "In ~a variables appear at least twice as argument." expr))
        ;; check that the given parameters are proper variable identifiers:
        (dolist (var variables)
          (unless (variable-p var)
            (error "Error while reading primitive expression~%  ~a.~
                             ~%Expected variable identifier, got ~a."
                   expr var)))
        (let ((primitive (find-primitive (first expr) primitive-inventory)))
          ;; primitive must be found
          (when (null primitive)
            (error "Primitive ~a is not defined " (car expr)))
          ;; check that the number of variables matches the
          ;; number of slot-specs:
          (unless (= (length (slot-specs primitive)) (length variables))
            (error "Error while reading primitive expression~%  ~a.~
                      ~%The number of given variables does not match ~
                      the number of slots."
                   expr))
          ;; check the type of the variable bindings
          (loop with slot-specs = (slot-specs primitive)
                for slot-spec in slot-specs
                for var in variables
                for binding = (find var bindings :key #'var)
                for expected-type = (slot-spec-type slot-spec)
                when (and (slot-boundp binding 'value)
                          (not (subtypep (type-of (value binding)) expected-type)))
                  do (error "Expected value of type ~a in ~a for ~a,~%got: ~a"
                            expected-type expr var (value binding))))))

    ;; all test succeeded, return t
    t))