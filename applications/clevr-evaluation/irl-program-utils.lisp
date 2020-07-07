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
    (remove predicate (find-all var irl-program :test #'member) :test #'equal)))

(defun linked-bind-statement (predicate irl-program)
  "Get the bind-predicate linked to the given predicate"
  (let* ((var (binding-var predicate))
         (all-linked (all-linked-predicates predicate var irl-program))
         (binding-list (remove-if-not #'(lambda (pred)
                                          (eql (first pred) 'bind))
                                      all-linked)))
    (when binding-list
      (first binding-list))))

(defun predicate-name (predicate)
  (first predicate))

(defun output-var (predicate)
  "Get the output variable of a predicate"
  (unless (eql (first predicate) 'bind)
    (second predicate)))

(defun input-vars (predicate)
  "Get the (possibly multiple) input variable(s) of a predicate"
  (unless (eql (first predicate) 'bind)
    (if (member (first predicate) '(filter query same equal? relate))
      (subseq predicate 2 (- (length predicate) 1))    
      (subseq predicate 2))))

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

(defun answer->str (answer-value)
  (case #+lispworks (type-of answer-value)
        #+ccl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
        #+sbcl (if (listp (type-of answer-value))
                  (first (type-of answer-value))
                  (type-of answer-value))
    (number (mkstr answer-value))
    (fixnum (mkstr answer-value))
    (integer (mkstr answer-value))
    (bit (mkstr answer-value))
    (shape-category (mkstr (shape answer-value)))
    (size-category (mkstr (size answer-value)))
    (color-category (mkstr (color answer-value)))
    (material-category (mkstr (material answer-value)))
    (boolean-category (mkstr (id answer-value)))))

(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

(defun get-target-predicate (irl-program)
  (let ((target-variable (get-target-var irl-program)))
    (find target-variable irl-program :key #'second :test #'eql)))
