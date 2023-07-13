(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;
;; compute args ;;
;;;;;;;;;;;;;;;;;;

(defun compute-args (anti-unification-result anti-unified-cxn)
  "Loop over all variables in both bindings lists.
   Whenever a variable occurs in either one of the delta's,
   or it is a slot-arg; add it as an arg!"
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((raw-pattern-delta (remove-arg-predicates pattern-delta))
            (raw-source-delta (remove-arg-predicates source-delta))
            (pattern-delta-slot-args (find-all 'slot-arg pattern-delta :key #'first))
            (source-delta-slot-args (find-all 'slot-arg source-delta :key #'first))
            (pattern-delta-top-args (find-all 'top-arg pattern-delta :key #'first))
            (source-delta-top-args (find-all 'top-arg source-delta :key #'first)))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var raw-pattern-delta)
                       (find-anywhere source-var raw-source-delta)
                       (find pattern-var pattern-delta-slot-args :key #'second)
                       (find source-var source-delta-slot-args :key #'second))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         (reverse (mapcar #'second pattern-delta-top-args)))
                        ((item-based-cxn-p anti-unified-cxn)
                         (mapcar #'second pattern-delta-slot-args))))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         (reverse (mapcar #'second source-delta-top-args)))
                        ((item-based-cxn-p anti-unified-cxn)
                         (mapcar #'second source-delta-slot-args))))))
    args))