(in-package :irl-2)

(export '(target-var open-vars type-of-var))

(defun get-unconnected-vars (irl-program)
  ;; find all unconnected variables, in other words, all those that
  ;; appear once in the irl-program
  ;; notice that this also includes the target-var
  (loop with variables = (find-all-anywhere-if #'variable-p irl-program)
        for var in (remove-duplicates variables)
        if (= (count var variables)  1)
        collect var))

(defun get-target-var (irl-program)
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (let* ((open-vars (get-unconnected-vars irl-program))
         (target-vars (intersection open-vars (append (mapcar #'second irl-program))))
         (target-var (when (= 1 (length target-vars))
                       (first target-vars))))
    target-var))

(defmethod target-var ((irl-program list))
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (unless (length= 0 irl-program)
    (if (and (length= irl-program 1)
             (eq (first (first irl-program)) 'bind))
      (cons (third (first irl-program))
            (second (first irl-program)))
      (get-target-var irl-program))))

(defun get-open-vars (irl-program)
  ;; find all unconnected variables which are not target
  (set-difference
   (get-unconnected-vars irl-program)
   (mapcar #'second irl-program)))

(defmethod open-vars ((irl-program list))
  ;; find all unconnected variables which are not target
  (get-open-vars irl-program))

(defgeneric type-of-var (var thing &key &allow-other-keys)
  (:documentation "Get the type of the var from the thing"))

(defmethod type-of-var (var (irl-program list)
                        &key (primitive-inventory *irl-primitives*))
  (let* ((predicate (find var irl-program :test #'member))
         (primitive (find-primitive (first predicate) primitive-inventory))
         (slot-spec (nth (- (position var predicate) 1)
                         (slot-specs primitive))))
    (slot-spec-type slot-spec)))
        