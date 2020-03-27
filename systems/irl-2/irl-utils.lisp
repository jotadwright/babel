(in-package :irl-2)

(export '(get-target-var))

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
         (target-vars (intersection open-vars
                                    (append (mapcar #'second irl-program))))
         (target-var (when (= 1 (length target-vars))
                       (first target-vars))))
    target-var))