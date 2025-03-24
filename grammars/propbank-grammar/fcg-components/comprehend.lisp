(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       ;;
;; Hash Mode             ;;
;;                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun comprehend-and-extract-frames (utterance &key (cxn-inventory *fcg-constructions*)
                                                (silent nil)
                                                (syntactic-analysis nil)
                                                (selected-rolesets nil)
                                                (timeout 60))
  "Comprehends an utterance and visualises the extracted frames."
  (set-data (blackboard cxn-inventory) :matched-categorial-links nil)
  (multiple-value-bind (solution cipn)
      (comprehend utterance :cxn-inventory cxn-inventory :silent silent
                  :syntactic-analysis syntactic-analysis :selected-rolesets selected-rolesets :timeout timeout)
    (if (eql solution 'time-out)
      (values 'time-out 'time-out 'time-out)
      (let ((frames (propbank-grammar::extract-frames (car-resulting-cfs (cipn-car cipn)))))
        (unless silent
          (wi:add-element `((h3 :style "margin-bottom:3px;") "Frame representation:"))
          (wi:add-element (make-html frames  :expand-initially t)))
        (values solution cipn frames)))))

(defmethod comprehend ((utterance conll-sentence) &key (cxn-inventory *fcg-constructions*) (silent nil) (selected-rolesets nil) (timeout 60) &allow-other-keys)
  (let ((initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode) :cxn-inventory cxn-inventory)))
    (set-data initial-cfs :annotation (propbank-frames utterance))
    (unless silent (monitors:notify parse-started (listify (sentence-string utterance)) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                                                    (comprehend-with-rolesets initial-cfs cxn-inventory selected-rolesets (sentence-string utterance) silent))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))

(defmethod comprehend ((utterance string) 
                       &key (syntactic-analysis nil) 
                       (cxn-inventory *fcg-constructions*)  (silent nil) (selected-rolesets nil) (timeout 60))
  (let* ((syntactic-analysis (nlp-tools:get-penelope-syntactic-analysis utterance
                                                                       :model (or (get-configuration cxn-inventory :model)
                                                                                  "en_benepar")))
         (initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :model (or (get-configuration cxn-inventory :model) "en_benepar")
                                 :cxn-inventory cxn-inventory :syntactic-analysis syntactic-analysis)))
    (unless silent (monitors:notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                                                    (comprehend-with-rolesets initial-cfs cxn-inventory selected-rolesets utterance silent))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))

(defmethod comprehend-all ((utterance string) 
                           &key (syntactic-analysis nil) 
                           (cxn-inventory *fcg-constructions*) (silent nil) (selected-rolesets nil) (timeout 60) (n nil))
  (let* ((syntactic-analysis (nlp-tools:get-penelope-syntactic-analysis utterance
                                                                        :model (or (get-configuration cxn-inventory :model)
                                                                                   "en_benepar")))
         (initial-cfs (de-render utterance (get-configuration cxn-inventory :de-render-mode)
                                 :model (or (get-configuration cxn-inventory :model) "en_benepar")
                                 :cxn-inventory cxn-inventory :syntactic-analysis syntactic-analysis)))
    (unless silent (monitors:notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind (meaning cip-node cip)
        (handler-case (trivial-timeout:with-timeout (timeout)
                        (comprehend-all-with-rolesets initial-cfs cxn-inventory selected-rolesets utterance silent n))
          (trivial-timeout:timeout-error (error)
            (values 'time-out 'time-out 'time-out)))
      (values meaning cip-node cip))))

(defun comprehend-with-rolesets (initial-cfs cxn-inventory selected-rolesets utterance silent)
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (set-data initial-cfs :selected-rolesets selected-rolesets)
    (set-data initial-cfs :utterance utterance)
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (when solution
                       (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        
        ;; Notification
        (unless silent (monitors:notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(defun comprehend-all-with-rolesets (initial-cfs cxn-inventory selected-rolesets utterance silent n)
  (let ((processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    (set-data (blackboard initial-cfs) :selected-rolesets selected-rolesets)
    (set-data (blackboard initial-cfs) :utterance utterance)
   (fcg-apply-with-n-solutions  processing-cxn-inventory initial-cfs '<- n :notify (not silent))
      ))