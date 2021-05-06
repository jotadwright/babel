(in-package :irl)

(export '(all-variables all-bind-statements all-predicates
          get-unconnected-vars get-target-var
          get-open-vars get-type-of-var))

(defun all-variables (irl-program)
  (find-all-anywhere-if #'variable-p irl-program))

(defun all-bind-statements (irl-program)
  (find-all 'bind irl-program :key #'car))

(defun all-predicates (irl-program)
  (find-all-if-not #'(lambda (x) (eql x 'bind))
                   irl-program :key #'car))

(defun get-unconnected-vars (irl-program)
  ;; find all unconnected variables, in other words, all those that
  ;; appear once in the irl-program
  ;; notice that this also includes the target-var
  (loop with variables = (all-variables irl-program)
        for var in (remove-duplicates variables)
        if (= (count var variables) 1)
        collect var))

(defun get-target-var (irl-program)
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (unless (length= 0 irl-program)
    (if (and (length= irl-program 1)
             (eq (car (first irl-program)) 'bind))
      (third (first irl-program))
      (let* ((open-vars (get-unconnected-vars irl-program))
             (target-vars (intersection open-vars (mapcar #'second irl-program)))
             (target-var (when (= 1 (length target-vars))
                           (first target-vars))))
        target-var))))

(defun get-open-vars (irl-program)
  ;; find all unconnected variables which are not target
  (set-difference
   (get-unconnected-vars irl-program)
   (mapcar #'second irl-program)))

(defun get-type-of-var (var program/predicate
                        &key (primitive-inventory *irl-primitives*))
  (let ((predicate (if (listp (first program/predicate))
                     (find var program/predicate :test #'member)
                     program/predicate)))
    (if (eql (first predicate) 'bind)
      (if (and (or (eql var (third predicate))
                   (eql var (fourth predicate)))
               (not (variable-p (second predicate))))
        (second predicate)
        'entity)
      (let* ((primitive (find-primitive (first predicate) primitive-inventory))
             (slot-spec (nth (- (position var predicate) 1)
                             (slot-specs primitive))))
        (slot-spec-type slot-spec)))))
        