(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-words (agent topic words)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-words ((agent mwm-agent) (topic mwm-object) words)
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

(defgeneric align-known-words (agent topic words)
  (:documentation "Align known words"))

#|
(defmethod align-known-words ((agent mwm-agent) (topic mwm-object) words)
  (declare (ignorable words))
  (loop with rewarded
        with punished
        for (category . certainty) in (parsed-meaning agent)
        for cxn = (find category (applied-cxns agent)
                        :key #'(lambda (cxn)
                                 (mapcar #'car (attr-val cxn :meaning)))
                        :test #'member)
        for attr = (attribute category)
        ;; update the prototype (mostly set to always)
        do (case (get-configuration agent :shift-prototype)
             (:on-success (when (communicated-successfully agent)
                            (shift-value cxn attr topic :alpha (get-configuration agent :alpha))))
             (:on-failure (unless (communicated-successfully agent)
                            (shift-value cxn attr topic :alpha (get-configuration agent :alpha))))
             (:always (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
             (:never nil))
        ;; adjust the certainty, when enabled
        when (get-configuration agent :update-certainty)
        ;; if the features were sampled, update the certainty
        ;; based on success. Otherwise, update the certainty
        ;; based on similarity.
        do (let ((sim (similarity topic category)))
             (if features-sampled-p
               (if (communicated-successfully agent)
                 (progn (push attr rewarded)
                   (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf)))
                 (progn (push attr punished)
                   (adjust-certainty agent cxn attr (- (get-configuration agent :certainty-decf)))))
               (if (>= sim 0)
                 (progn (push attr rewarded)
                   (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf)))
                 (progn (push attr punished)
                   (adjust-certainty agent cxn attr (- (get-configuration agent :certainty-decf)))))))
        ;; notify
        finally
        (notify scores-updated cxn rewarded punished)))
|#




(defun get-cxn-from-category (agent category)
  (find category (applied-cxns agent)
        :key #'(lambda (cxn)
                 (mapcar #'car (attr-val cxn :meaning)))
        :test #'member))

(defun discriminating-p (agent category topic)
  (let* ((context (remove topic (objects (context agent))))
         (topic-sim (similarity topic category))
         (best-object-sim (apply #'max
                                 (loop for obj in context
                                       collect (similarity obj category)))))
    (>= topic-sim best-object-sim)))

(defmethod align-known-words ((agent mwm-agent) (topic mwm-object) words)
  (let ((categories-w-cxns (loop for (category . certainty) in (parsed-meaning agent)
                                 for cxn = (get-cxn-from-category agent category)
                                 collect (cons category cxn)))
        rewarded punished)
    ;; maybe the prototype should be expanded more often!
    (if (communicated-successfully agent)
      (loop for (category . cxn) in categories-w-cxns
            for attr = (attribute category)
            if (discriminating-p agent category topic)
            do (progn (push attr rewarded)
                 (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf))
                 (shift-value cxn attr topic :alpha (get-configuration agent :alpha)))
            else
            do (progn (push attr punished)
                 (adjust-certainty agent cxn attr (- (get-configuration agent :certainty-incf)))))
      (loop for (category . cxn) in categories-w-cxns
            for attr = (attribute category)
            do (progn (push attr punished)
                 (adjust-certainty agent cxn attr (- (get-configuration agent :certainty-incf))))))
    ;; shortcut!
    (notify scores-updated (cdr (first categories-w-cxns)) rewarded punished)))
      
        
  
        
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
         (feature-selection (get-configuration agent :feature-selection)))
    (notify alignment-started agent)
    (when unknown-words
      (notify adopting-words unknown-words)
      (adopt-unknown-words agent topic unknown-words))
    (when known-words
      (notify aligning-words known-words)
      (align-known-words agent topic known-words
                         ;(eql feature-selection :sampling)))))
                         ))))
  