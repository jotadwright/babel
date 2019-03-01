(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

(define-event new-cxn-added (cxn fcg-construction))

(defun adopt-unknown-words (agent topic words)
  (let ((meaning
         (loop with initial-certainty = (get-configuration agent :initial-certainty)
               for attr in (get-configuration agent :attributes)
               collect (list attr
                             (get-attr-val topic attr)
                             initial-certainty))))
    (loop for word in words
          for new-cxn = (add-lex-cxn agent word meaning)
          do (notify new-cxn-added new-cxn))))

(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

;(defun similarity->delta (similarity)
;  (- similarity 0.5))

(defun similarity->delta (similarity)
  (float
   (if (>= similarity 0.8)
     (- (* (/ 5 2) similarity) 2)
     (- (* (/ 5 8) similarity) (/ 1 2)))))

(defun align-known-words (agent topic words)
  (loop for word in words
        for cxn = (find-cxn-with-form agent word)
        for meaning = (attr-val cxn :meaning)
        do (loop with rewarded
                 with punished
                 for (attr value certainty) in meaning
                 for sim = (attribute-similarity topic value attr)
                 for delta = (similarity->delta sim)
                 unless (= delta 0)
                 do (adjust-certainty agent cxn attr delta)
                 if (>= delta 0)
                 do (progn (push attr rewarded)
                      (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
                 else
                 do (push attr punished)
                 finally
                 (notify scores-updated cxn rewarded punished))))
          
        
(defgeneric align-agent (agent topic)
  (:documentation
   "The alignment procedure can be split in 2 cases:
   1. Adopting unknown words. The meaning of these unknown words
      will be a cloud containg all attributes of the object. The
      value for each attribute will be the exact value of the
      topic (pointed to by the tutor). These values are linked with
      the new word using an initial certainty score.
   2. Alinging known words. For each word in the utterance, the
      agent will reward and punish certain attributes, based on
      the similarity measure. This is based on the function
      f(x) = x - 0.5. So, if the similarity is 0, the certainty will
      be punished with -0.5. If the similarity is 1, the certainty
      will be rewarded with 0.5. The attributes with positive similarity
      will not only be rewarded; their value will also be shifted
      towards the topic, using rate alpha."))

(define-event alignment-started (agent mwm-agent))
(define-event adopting-words (words list))
(define-event aligning-words (words list))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let* ((known-words
          (loop for form in (utterance agent)
                when (find-cxn-with-form agent form)
                collect form))
         (unknown-words (set-difference (utterance agent)
                                        known-words
                                        :test #'string=)))
    (notify alignment-started agent)
    (when unknown-words
      (notify adopting-words unknown-words)
      (adopt-unknown-words agent topic unknown-words))
    (when known-words
      (notify aligning-words known-words)
      (align-known-words agent topic known-words))))
  