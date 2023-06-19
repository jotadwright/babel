(in-package :fcg)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting form and meaning from fcg-constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(extract-meaning-predicates
          extract-form-predicates))

(defgeneric extract-meaning-predicates (object))

(defmethod extract-meaning-predicates ((cxn fcg-construction))
  (append (mappend #'extract-meaning-predicates (conditional-part cxn))
          (mappend #'extract-meaning-predicates (contributing-part cxn))))

(defmethod extract-meaning-predicates ((unit conditional-unit))
  (append (extract-meaning-predicates (comprehension-lock unit))
          (extract-meaning-predicates (formulation-lock unit))))

(defmethod extract-meaning-predicates ((unit contributing-unit))
  (extract-meaning-predicates (unit-structure unit)))

(defmethod extract-meaning-predicates ((unit-body list))
  (append (find-feature-value 'meaning unit-body)
          (find-hashed-feature-value 'meaning unit-body)))

;; (extract-meaning-predicates (first (constructions *fcg-constructions*)))

(defgeneric extract-form-predicates (object))

(defmethod extract-form-predicates ((cxn fcg-construction))
  (append (mappend #'extract-form-predicates (conditional-part cxn))
          (mappend #'extract-form-predicates (contributing-part cxn))))

(defmethod extract-form-predicates ((unit conditional-unit))
  (append (extract-form-predicates (comprehension-lock unit))
          (extract-form-predicates (formulation-lock unit))))

(defmethod extract-form-predicates ((unit contributing-unit))
  (extract-form-predicates (unit-structure unit)))

(defmethod extract-form-predicates ((unit-body list))
  (append (find-feature-value 'form unit-body)
          (find-hashed-feature-value 'form unit-body)))

;; (extract-form-predicates (first (constructions *fcg-constructions*)))

(defun find-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (equal (feature-name feature-value) feature)
        return  (second feature-value)))

(defun find-hashed-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (and (equal (first feature-value) 'HASH)
                  (equal (second feature-value) feature))
        return (third feature-value)))

#|
(defmethod comprehend (utterance &key
                                 (cxn-inventory *fcg-constructions*)
                                 (gold-standard-meaning nil)
                                 (silent nil))
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterances (listify utterance))
    (set-data initial-cfs :meanings (if (atom (caar gold-standard-meaning)) (list gold-standard-meaning) gold-standard-meaning))
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))

(defmethod formulate (meaning &key
                            (cxn-inventory *fcg-constructions*)
                            (gold-standard-utterance nil)
                            (silent nil))
  (let ((initial-cfs (create-initial-structure meaning
                                               (get-configuration cxn-inventory :create-initial-structure-mode)))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :meanings (if (atom (caar meaning)) (list meaning) meaning))
    (set-data initial-cfs :utterances (listify gold-standard-utterance))
    ;; Notification
    (unless silent (notify produce-started meaning cxn-inventory initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '-> :notify (not silent))
      (let ((utterance
             (and solution
                  (or (find-data (goal-test-data solution) 'utterance)
                      (render 
                       (car-resulting-cfs (cipn-car solution)) 
                       (get-configuration cxn-inventory :render-mode)
                      :node solution)))))
        ;; Notification
        (unless silent (notify produce-finished utterance))
        ;; Return value
        (values utterance solution cip)))))
|#

(defmethod render ((node cip-node) (mode (eql :generate-and-test)) &key &allow-other-keys)
  (render (car-resulting-cfs (cipn-car node)) :generate-and-test))

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :add-to-front)))
  (push node (queue cip)))