(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;
;; compute args ;;
;;;;;;;;;;;;;;;;;;

(defgeneric compute-form-args (anti-unification-result thing source-args)
  (:documentation "Compute the form-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defgeneric compute-meaning-args (anti-unification-result thing source-args)
  (:documentation "Compute the meaning-args from the anti-unification result
                   obtained by anti-unifying the observation with 'thing',
                   which can be a cxn or a cipn."))

(defmethod compute-form-args (anti-unification-result (anti-unified-cxn fcg-construction) (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args (extract-slot-form-args anti-unified-cxn))
            (pattern-top-args (extract-top-lvl-form-args anti-unified-cxn))
            (source-slot-args (find-data source-args :slot-form-args))
            (source-top-args (find-data source-args :top-lvl-form-args)))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var pattern-delta)
                       (find-anywhere source-var source-delta)
                       (find pattern-var pattern-slot-args)
                       (find source-var source-slot-args)
                       (> (count pattern-var pattern-bindings :key #'car) 1)
                       (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn)
                         pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         source-top-args)
                        ((item-based-cxn-p anti-unified-cxn)
                         source-slot-args)))))
    args))

(defmethod compute-form-args (anti-unification-result (anti-unified-cipn cip-node) (source-args blackboard))
  )

(defmethod compute-meaning-args (anti-unification-result (anti-unified-cxn fcg-construction) (source-args blackboard))
  (let ((args (make-blackboard)))
    (with-slots (generalisation
                 pattern-bindings
                 source-bindings
                 pattern-delta
                 source-delta) anti-unification-result
      (let ((pattern-slot-args (extract-slot-meaning-args anti-unified-cxn))
            (pattern-top-args (extract-top-lvl-meaning-args anti-unified-cxn))
            (source-slot-args (find-data source-args :slot-meaning-args))
            (source-top-args (find-data source-args :top-lvl-meaning-args)))
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var pattern-delta)
                       (find-anywhere source-var source-delta)
                       (find pattern-var pattern-slot-args)
                       (find source-var source-slot-args)
                       (> (count pattern-var pattern-bindings :key #'car) 1)
                       (> (count source-var source-bindings :key #'car) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        (set-data args :pattern-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         pattern-top-args)
                        ((item-based-cxn-p anti-unified-cxn)
                         pattern-slot-args)))
        (set-data args :source-slot-args
                  (cond ((holistic-cxn-p anti-unified-cxn)
                         source-top-args)
                        ((item-based-cxn-p anti-unified-cxn)
                         source-slot-args)))))
    args))

(defmethod compute-meaning-args (anti-unification-result (anti-unified-cipn cip-node) (source-args blackboard))
  )



#|
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
        ;; determine the args that connect the generalisation to both delta's
        (loop for (pattern-var . generalisation-var) in pattern-bindings
              for (source-var . nil) in source-bindings
              when (or (find-anywhere pattern-var raw-pattern-delta)
                       (find-anywhere source-var raw-source-delta)
                       (find pattern-var pattern-delta-slot-args :key #'second)
                       (find source-var source-delta-slot-args :key #'second)
                       (> (count pattern-var pattern-bindings :key #'first) 1)
                       (> (count source-var source-bindings :key #'first) 1))
              do (push-data args :pattern-top-lvl-args pattern-var)
                 (push-data args :source-top-lvl-args source-var)
                 (push-data args :generalisation-slot-args generalisation-var))
        ;; determine the args that connect the cxns from the delta's to other cxns
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
|#
