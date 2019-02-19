(in-package :mwm)

;;;; ----------------------
;;;; + Interaction script +
;;;; ----------------------

(defun initialize-agent (agent context)
  "Initialize the agent"
  (setf (context agent) context
        (topic agent) (first (entities context))
        (utterance agent) nil
        (communicated-successfully agent) nil
        (discriminating-categories agent) nil
        (applied-lex agent) nil))

(define-event context-generated (context mwm-object-set))

(defmethod interact :before ((experiment mwm-experiment) interaction &key)
  "Generate a context and intialize the agents"
  (let* ((min-context-size (get-configuration experiment :min-context-size))
         (max-context-size (get-configuration experiment :max-context-size))
         (context-size (random-from-range min-context-size max-context-size))
         (context (generate-context context-size)))
    (notify context-generated context)
    (loop for agent in (interacting-agents interaction)
          do (initialize-agent agent context))))

(defmethod interact ((experiment mwm-experiment) interaction &key)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    (conceptualise speaker (topic speaker))
    (produce speaker)
    (unless (re-enter speaker)
      (invent speaker)
      (produce speaker))
    (setf (utterance hearer) (utterance speaker))
    (let ((interpreted-topic (interpret hearer (utterance hearer))))
      (setf (topic hearer) interpreted-topic)
      (if interpreted-topic
        (if (determine-success speaker hearer)
          (loop for agent in (interacting-agents interaction)
                do (setf (communicated-successfully agent) t))
          (adopt hearer (topic speaker)))
        (adopt hearer (topic speaker))))))
    
(defmethod interact :after ((experiment mwm-experiment) interaction &key)
  (let ((speaker-topic (topic (speaker interaction))))
    (loop for agent in (interacting-agents interaction)
          do (align-agent agent speaker-topic
                          (get-configuration experiment :alignment-strategy)))))

;; -------------
;; + Alignment +
;; -------------

;; Alignment consists of several steps:
;; 0. Punish form competitors
;; 1. Entrenchement of the shared attributes between meaning and topic
;;    Also, the categories are shifted towards the topic.
;; 2. Erosion of the disjoint attributes between meaning and topic
;; 3. In case of failed game and no unknown words, the hearer extends the meaning
;;    of the used words.

(defmethod align-agent :around ((agent mwm-agent) (topic mwm-object) strategy)
  (case (get-configuration agent :who-aligns)
    (:speaker (when (speakerp agent) (call-next-method)))
    (:hearer (when (hearerp agent) (call-next-method)))
    (:both (call-next-method))))

(defun unknown-forms (agent)
  (loop for form in (utterance agent)
        unless (find form (lexicon agent) :key #'form :test #'string=)
        collect form))

(define-event alignment-started (agent mwm-agent))
(define-event competitor-punished (competitor mwm-lex))
(define-event lex-channels-entrenched (lex mwm-lex) (channels list))
(define-event lex-channels-eroded (lex mwm-lex) (channels list))
(define-event meaning-extended (lex mwm-lex) (categories list))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object) (strategy (eql :lateral-inhibition)))
  (let* ((topic-categories (categorise-object agent topic))
         (utterance-meaning (utterance-meaning agent (utterance agent)))
         (shared-categories (intersection utterance-meaning topic-categories :key #'car))
         (disjoint-categories (set-difference utterance-meaning topic-categories :key #'car)))
    (notify alignment-started agent)
    ;; punish form competitors across channels
    (loop for lex in (applied-lex agent)
          do (loop for competitor in (get-form-competitors agent lex)
                   do (notify competitor-punished competitor)
                   do (loop for channel in (get-configuration agent :channels)
                            do (dec-lex-score agent competitor channel
                                              :delta (get-configuration agent :decf-lex-score)))))
    ;; entrenchment and erosion
    (loop for lex in (applied-lex agent)
          do (loop with entrenched-channels = nil
                   with eroded-channels = nil
                   for (category . certainty) in (meaning lex)
                   if (member category shared-categories :key #'car)
                   do (progn (shift-category category (topic agent))
                        (inc-lex-score lex (channel category)
                                       :delta (get-configuration agent :incf-lex-score))
                        (push (channel category) entrenched-channels)) 
                   else
                   do (progn (push (channel category) eroded-channels)
                        (dec-lex-score agent lex (channel category)
                                       :delta (get-configuration agent :decf-lex-score)))
                   finally
                   (progn (notify lex-channels-entrenched lex entrenched-channels)
                     (notify lex-channels-eroded lex eroded-channels))))
    ;; extend meaning
    (when (and (not (communicated-successfully agent))
               (hearerp agent)
               (null (unknown-forms agent)))
      (loop for lex in (applied-lex agent)
            do (extend-meaning lex disjoint-categories)
            do (notify meaning-extended lex disjoint-categories)))))