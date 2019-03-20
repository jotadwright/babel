(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

(define-event new-cxn-added (cxn fcg-construction))

(defun adopt-unknown-words (agent topic words)
  (let ((meaning
         (loop with initial-certainty = (get-configuration agent :initial-certainty)
               for attr in (get-configuration agent :attributes)
               collect (cons (make-category-from-object topic attr) initial-certainty))))
    (loop for word in words
          for new-cxn = (add-lex-cxn agent word meaning)
          do (notify new-cxn-added new-cxn))))

(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

(define-event re-introduced-meaning (cxn fcg-construction)
  (attrs list))

;(defun similarity->delta (similarity)
;  (- similarity 0.5))

;(defun similarity->delta (similarity)
;  (float
;   (if (>= similarity 0.8)
;     (- (* (/ 5 2) similarity) 2)
;     (- (* (/ 5 8) similarity) (/ 1 2)))))

;(defun similarity->delta (similarity)
;  (float
;   (if (>= similarity 0.9)
;     (- (* 5 similarity) 4.5)
;     (- (* 0.55556 similarity) 0.5))))

;(defun distance->delta (distance)
;  (if (> distance 2.0)
;    -0.1
;    (+ (* (- 0.15) distance) 0.1)))

(defun align-known-words (agent topic words)
  (loop for word in words
        for cxn = (find-cxn-with-form agent word)
        for meaning = (attr-val cxn :meaning)
        for all-attributes = (get-configuration agent :attributes)
        for unused-attributes = (set-difference all-attributes
                                                (mapcar #'attribute (mapcar #'car meaning)))
        do (loop with rewarded
                 with punished
                 for (category . certainty) in meaning
                 for attr = (attribute category)
                 for sim = (similarity topic category)
                 ;; HOW TO KNOW WHEN TO UPDATE THE CATEGORY AND WHEN NOT TO??
                 ;; FIRST UPDATE CATEGORY AND THAN CERTAINTY? OR VICE-VERSA?

                 ;; CHECK IF THE CATEGORY HAS A SINGLE ENTRY. IF IT HAS, USE THE PROTOTYPE DISTANCE.
                 ;; IF THIS DISTANCE IS BELOW A THRESHOLD, DO THE UPDATE. OTHERWISE, DON'T.
                 ;; FOR THIS, I NEED SENSORY/CONTEXT SCALING
                 for delta = (if (plusp sim)
                               (get-configuration agent :certainty-incf)
                               (get-configuration agent :certainty-decf))
                 if (> delta 0)
                 do (progn (push attr rewarded)
                      (case (get-configuration agent :shift-prototype)
                        (:on-success (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
                        (:always (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))))
                 else
                 do (progn (push attr punished)
                      (case (get-configuration agent :shift-prototype)
                        (:on-failure (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
                        (:always (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))))
                 end
                 unless (= delta 0)
                 do (adjust-certainty agent cxn attr delta
                                      :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound))
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
   2. Aligning known words. For each word in the utterance, the
      agent will reward and punish certain attributes, based on
      the similarity measure. The attributes with positive similarity
      will not only be rewarded; their value will also be shifted
      towards the topic, using rate alpha. When the game failed, the
      agent will again extend the meaning of the word, but with a
      low initial certainty."))

(define-event alignment-started (agent mwm-agent))
(define-event adopting-words (words list))
(define-event aligning-words (words list))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let* ((known-words
          (loop for form in (utterance agent)
                when (find-cxn-with-form agent form)
                collect form))
         (unknown-words (set-difference (utterance agent) known-words
                                        :test #'string=)))
    (notify alignment-started agent)
    (when unknown-words
      (notify adopting-words unknown-words)
      (adopt-unknown-words agent topic unknown-words))
    (when known-words
      (notify aligning-words known-words)
      (align-known-words agent topic known-words))))
  