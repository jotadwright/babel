;;;; irl-program-utils.lisp

(in-package :clevr-evaluation)

(export '(get-all-vars all-linked-predicates linked-bind-statement
          predicate-name output-var input-vars binding-var
          bind-statement-type bind-statement-var bind-statement-value
          answer->str get-target-value get-target-predicate))

(defun get-all-vars (irl-program)
  "Find all variables in a predicate"
  (find-all-anywhere-if #'variable-p irl-program))

(defun all-linked-predicates (predicate var irl-program)
  "Find the next predicate, given a variable"
  (when (member var predicate)
    (loop for p in (remove 'bind irl-program :key #'first)
          for connecting-vars = (cons (output-var p) (input-vars p))
          when (and (not (equalp p predicate))
                    (find var connecting-vars))
          collect p)))

(defun linked-bind-statement (predicate irl-program)
  "Get the bind-predicate linked to the given predicate"
  (let* ((var (binding-var predicate))
         (all-bind-statements (find-all 'bind irl-program :key #'first)))
    (find var all-bind-statements :key #'third)))

(defun predicate-name (predicate)
  (first predicate))

(defun output-var (predicate)
  "Get the output variable of a predicate"
  (unless (eql (first predicate) 'bind)
    (second predicate)))

(defun input-vars (predicate)
  "Get the (possibly multiple) input variable(s) of a predicate"
  (unless (eql (first predicate) 'bind)
    (cond ((member (first predicate) '(relate same))
           (subseq predicate 2 (- (length predicate) 3)))
          ((member (first predicate) '(filter query))
           (subseq predicate 2 (- (length predicate) 2)))
          ((member (first predicate) '(equal?))
           (subseq predicate 2 (- (length predicate) 1)))
          (t (subseq predicate 2)))))

(defun binding-var (predicate)
  "Get the binding variable of a predicate"
  (unless (eql (first predicate) 'bind)
    (when (member (first predicate) '(filter query same equal? relate))
      (last-elt predicate))))

(defun bind-statement-type (bind-statement)
  (when (eql (first bind-statement) 'bind)
    (second bind-statement)))

(defun bind-statement-var (bind-statement)
  (when (eql (first bind-statement) 'bind)
    (third bind-statement)))

(defun bind-statement-value (bind-statement)
  (when (eql (first bind-statement) 'bind)
    (fourth bind-statement)))

(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

(defun get-target-predicate (irl-program)
  (let ((target-variable (get-target-var irl-program)))
    (find target-variable irl-program :key #'second :test #'eql)))
