(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; Understanding an utterance given a PDM                                   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod understand ((utterance string)
                       (cxn-inventory fcg-construction-set)
                       (world-state world-state)
                       &key silent)
  "Comprehends an utternace given a PDM."
  (let* ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :world-state world-state
                                 :cxn-inventory cxn-inventory)))
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply (processing-cxn-inventory cxn-inventory) initial-cfs '<- :notify (not silent))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning (processing-cxn-inventory cxn-inventory)))
        (values meaning solution cip)))))

