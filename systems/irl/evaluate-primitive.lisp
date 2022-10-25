(in-package :irl)

(defstruct (primitive-evaluation-result (:conc-name per-))
  (status 'initial)
  ;; status can be initial, inconsistent, evaluated-w/o-result or evaluated
  (source-bindings nil)
  (evaluated-primitive nil)
  (resulting-bindings nil))

(defmethod copy-object ((per primitive-evaluation-result))
  (copy-structure per))


(defun evaluate-primitive (primitive bindings-list &optional (ontology (make-blackboard)))
  "primitive -- primitive-object
   bindings -- list of bindings in the right order of the slots of the primitives
   Returns either a list of list of new bindings, nil or 'inconsistent"
  ;; sanity check: as many bindings as slots
  (assert (= (length bindings-list)
             (length (slot-specs primitive))))
  (let* ((bindings (if (subtypep (type-of (first bindings-list))
                                 'binding)
                     bindings-list
                     (loop for b in bindings-list
                           collect (make-instance 'binding :var (first b)
                                                  :score (second b) :value (third b)
                                                  :available-at (fourth b)))))
         (bound-slots-pattern (loop for binding in bindings
                                    if (value binding) collect t
                                    else collect nil))
         (applicable-evaluation-spec (find bound-slots-pattern
                                           (evaluation-specs primitive)
                                           :key #'bound-slots-pattern
                                           :test #'equalp))
         (applicable-slot-spec (loop for slot-spec in (slot-specs primitive)
                                     for binding in bindings
                                     always (or (null (value binding))
                                                (subtypep (type-of (value binding))
                                                          (slot-spec-type slot-spec)))))
         (primitive-is-applicable (and applicable-slot-spec applicable-evaluation-spec))
         (results (when primitive-is-applicable
                    (apply (evaluation-spec-function applicable-evaluation-spec)
                           ontology bindings (mapcar #'value bindings)))))
    ;;  given the bound-slots-pattern there are the following cases
    ;;  no bound-slots-pattern found
    ;;  bound-slots-pattern all t -> t (success), nil (failure)
    ;;  bound-slots-pattern \exists nil ->
    ;;          results - could bind,
    ;;          results == nil - could execute, but not bind
    ;;  errors during application -> not handled
    (if primitive-is-applicable
      (if (null results)
        'inconsistent
        (if (position nil bound-slots-pattern)
          ;; bound slot patterns exist
          (loop for result in results
                collect (loop for binding in bindings
                              for res in result
                              if (value binding) collect binding
                              else collect (make-instance 'binding :var (var binding)
                                                          :score (first res) :value (second res)
                                                          :available-at (fourth res))))
          ;; only checked the bindings -> return the bindings
          (list bindings)))
      nil)))


(defun evaluate-primitive-in-program (primitive-in-program bindings
                                      ontology primitive-inventory)
  "primitive-in-program -- primitive from irl program e.g.
                           (pick-apples ?var-1 ?var3 ?var-123)
   bindings -- ((?var-1 1.0 value-1)(?var-2 nil nil)...)
   primitive-inventory
   Returns new sets of bindings -- (((?var-1 1.0 value-1)(?var-2 1.0 value-2))
                                    ((?var-1 1.0 value-1)(?var-2 1.0 value-3))...)"
  (multiple-value-bind (primitive-bindings-indices primitive-bindings)
      (loop for var in (cdr primitive-in-program)
            for index = (position var bindings :key #'var)
            collect index into indices
            collect (nth index bindings) into primitive-bindings
            finally (return (values indices primitive-bindings)))

    (let* ((primitive-type (first primitive-in-program))
           (primitive (find-primitive primitive-type primitive-inventory))
           ;; returns a list of lists, nil or 'inconsistent
           (eval-primitive-result
            (evaluate-primitive primitive primitive-bindings ontology))
           (succeeded-pers nil)
           (failed-pers nil))
      (cond
       ;; evaluated without results
       ((null eval-primitive-result)
        (push (make-primitive-evaluation-result
               :status 'evaluated-w/o-results
               :source-bindings bindings
               :evaluated-primitive primitive-in-program
               :resulting-bindings bindings)
              failed-pers))
       ;; bindings are inconsistent
       ((eql eval-primitive-result 'inconsistent)
        (push (make-primitive-evaluation-result
               :status 'inconsistent
               :source-bindings bindings
               :evaluated-primitive primitive-in-program
               :resulting-bindings bindings)
              failed-pers))
       ;; new bindings were found
       (t
        (loop for new-primitive-bindings in eval-primitive-result
              do (assert (= (length new-primitive-bindings)
                            (length primitive-bindings-indices)))
              (let ((new-bindings-list
                     (loop with new-bindings = (copy-list bindings)
                           for new-primitive-binding in new-primitive-bindings
                           for index in primitive-bindings-indices
                           do (setf (nth index new-bindings)
                                    new-primitive-binding)
                           finally (return new-bindings))))
                  (push (make-primitive-evaluation-result
                         :status 'evaluated
                         :source-bindings bindings
                         :evaluated-primitive primitive-in-program
                         :resulting-bindings new-bindings-list)
                        succeeded-pers)))))
      (values succeeded-pers failed-pers))))