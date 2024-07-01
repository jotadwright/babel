(in-package :au-benchmark.base)

(export '(+no-bindings+
          +fail+
          fail?
          subs-lookup
          get-binding
          extend-bindings
          reverse-bindings
          substitute-bindings))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fail+ nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fail? (x)
    "Test whether x is eq to the constant +fail+"
    (eq +fail+ x)))

(defparameter +no-bindings+ '((t . t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun no-bindings? (x)
    "Test whether x is eq to the constant +no-bindings+"
    (eq +no-bindings+ x)))

;; 'binding' ADT definition

(deftype binding ()
  `(cons))

(defun make-binding (var val)
  (cons var val))

(defun binding-val (binding)
  "Returns the value of binding (the cdr)"
  (declare (type binding binding))
  (cdr binding))

(defsetf binding-val (binding) (val)
  `(setf (cdr ,binding) ,val))

(defun binding-var (binding)
  "Returns the variable of binding (the car)"
  (declare (type binding binding))
  (car binding))

;; 'bindings' ADT definition

(deftype bindings ()
  `(or (satisfies fail?)
       (satisfies no-bindings?)
       cons))

(defun subs-lookup (pattern-bindings source-bindings pattern source)
  "returns the binding for a given pattern and source in the bindings-lists"
  (cond
   ;; Case: no pattern-bindings left
   ((null pattern-bindings)
    +fail+)
   ;; Case: see whether pattern and source are first elements of bindings, if so, return binding
   ((and (equal (car (first pattern-bindings)) pattern)
         (equal (car (first source-bindings)) source)
         (equal (cdr (first pattern-bindings)) (cdr (first source-bindings))))
    (cdr (first pattern-bindings)))
   ;; Case: otherwise compare rests
   (t
    (subs-lookup (cdr pattern-bindings) (cdr source-bindings) pattern source))))

(defun get-binding (var bindings)
  "Returns a binding (a cons cell of the form (var . value)) for the variable
   var as specified in bindings or NIL if var is unbound in bindings."
  (declare (type symbol var) (type bindings bindings))
  (assoc var bindings))

(defun extend-bindings (var val bindings)
  "Adds the binding of var to val to bindings."
  (declare (type bindings bindings))
  #+dbg
  (assert (not (get-binding var bindings)))
  (cons (make-binding var val)
	(if (eq bindings +no-bindings+)
	    nil
	    bindings)))

(defun reverse-bindings (bs)
  (declare (type bindings bs))
  (loop for b in bs collect (make-binding (binding-val b) (binding-var b))))

(defun substitute-bindings (bindings x)
  "Substitute all variables in x with their binding as specified in bindings."
  (declare (type bindings bindings))
  ;; Remi 20/10: Quick solution to remove illegal bindings... but we need to
  ;;             understand where they come-from
  (setf bindings (loop for binding in bindings
                       when (rest binding) collect binding))
  ;; -------------------------------------------------------
  (labels ((aux (x)
             (cond ((variable-p x)
                    (let ((y (assoc x bindings :test #'eq)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
                   ((atom x) x)
                   (t (cons (aux (car x)) (aux (cdr x)))))))
    (cond ((eq bindings +fail+) +fail+)
          ((eq bindings +no-bindings+) x)
          (t (aux x)))))