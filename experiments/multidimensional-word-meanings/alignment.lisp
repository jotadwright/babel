(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-words (agent topic words strategy)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-words ((agent mwm-agent) (topic mwm-object) words strategy)
  (let ((meaning
         (loop with initial-certainty = (get-configuration agent :initial-certainty)
               for attr in (get-configuration agent :attributes)
               collect (cons (make-category-from-object topic attr (get-configuration agent :category-representation))
                             initial-certainty))))
    (loop for word in words
          for new-cxn = (add-lex-cxn agent word meaning)
          do (notify new-cxn-added new-cxn))))

(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

(define-event re-introduced-meaning (cxn fcg-construction)
  (attrs list))

(defgeneric align-known-words (agent topic words strategy)
  (:documentation "Align known words"))

(defmethod align-known-words ((agent mwm-agent) (topic mwm-object) words strategy)
  (declare (ignorable words))
  ;(loop for word in words
  ;      for cxn = (find-cxn-with-form agent word)
  ;      for meaning = (attr-val cxn :meaning)
  ;      for all-attributes = (get-configuration agent :attributes)
  ;      for unused-attributes = (set-difference all-attributes
  ;                                              (mapcar #'attribute (mapcar #'car meaning)))
  ;      do
  (loop with rewarded
        with punished
        for (category . certainty) in (parsed-meaning agent) ;meaning
        for cxn = (find category (applied-cxns agent)
                        :key #'(lambda (cxn)
                                 (mapcar #'car (attr-val cxn :meaning)))
                        :test #'member)
        for attr = (attribute category)
        ;; shift the prototype
        do (case (get-configuration agent :shift-prototype)
             (:on-success (when (communicated-successfully agent)
                            (shift-value cxn attr topic :alpha (get-configuration agent :alpha))))
             (:on-failure (unless (communicated-successfully agent)
                            (shift-value cxn attr topic :alpha (get-configuration agent :alpha))))
             (:always (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
             (:never nil))
        ;; adjust the certainty
        when (get-configuration agent :update-certainty)
        do (let ((sim (similarity topic category)))
             (if (plusp sim)
               (progn (push attr rewarded)
                 (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf)))
               (progn (push attr punished)
                 (adjust-certainty agent cxn attr (- (get-configuration agent :certainty-decf))))))
        ;; notify
        finally
        (notify scores-updated cxn rewarded punished)))        
        
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
                                        :test #'string=))
         (strategy (get-configuration agent :strategy)))
    (notify alignment-started agent)
    (when unknown-words
      (notify adopting-words unknown-words)
      (adopt-unknown-words agent topic unknown-words strategy))
    (when known-words
      (notify aligning-words known-words)
      (align-known-words agent topic known-words strategy))))
  