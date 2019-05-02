(in-package :mwm)

(defmethod initialize-agent ((agent mwm-agent) clevr-context mwm-context)
  "Set the context and possibly also the topic of the agent.
   Clear some slots to prepare for the interaction."
  (setf (context agent)
        (cond ((learnerp agent) mwm-context)
              ((and (tutorp agent)
                    (eql (get-configuration agent :tutor-lexicon)
                         :continuous))
               mwm-context)
              (t clevr-context)))
  (setf (topic agent)
        (when (speakerp agent)
          (random-elt (objects (context agent)))))
  (setf (applied-cxns agent) nil
        (discriminative-set agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

(define-event context-determined (clevr-context clevr-object-set) (mwm-context mwm-object-set))

(defmethod initialize-interaction ((experiment mwm-experiment)
                                   interaction &key)
  "Initialize the interaction by choosing a random context"
  ;; integrate perceptual deviation here; do both agents get the same noise
  (let* ((clevr-context (random-elt (world experiment)))
         (mwm-context (clevr->mwm clevr-context
                                  :noise-amount (get-configuration experiment :noise-amount)
                                  :scale (get-configuration experiment :scale-world)
                                  :noise-prob (get-configuration experiment :noise-prob))))
    (notify context-determined clevr-context mwm-context)
    (loop for agent in (interacting-agents interaction)
          do (initialize-agent agent clevr-context mwm-context))))

(defmethod tutor-speaks ((experiment mwm-experiment)
                         interaction &key)
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    (loop while t
          for success = (conceptualise speaker (id speaker))
          if success
          return success
          else
          do (initialize-interaction experiment interaction))
    (produce-word speaker (id speaker))
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer (id hearer))
                 (interpret hearer (id hearer))
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

(defmethod learner-speaks ((experiment mwm-experiment)
                           interaction &key)
  "The learner tries to conceptualise the topic. If this
   fails, the game ends. Next, it tries to produce an
   utterance. If this fails, the game ends. When successful,
   the tutor parsed and interprets the utterance and success
   is determined. Adoption is handled together with
   alignment."
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    (when (conceptualise speaker (id speaker))
      (when (produce-word speaker (id speaker))
        (setf (utterance hearer) (utterance speaker))
        (when (and (parse-word hearer (id hearer))
                   (interpret hearer (id hearer))
                   (determine-success speaker hearer))
          (setf (communicated-successfully speaker) t
                (communicated-successfully hearer) t))))))
    
(defmethod interact ((experiment mwm-experiment)
                     interaction &key)
  "Call the appropriate interaction script, depending
   on who is speaker and hearer"
  (initialize-interaction experiment interaction)
  (let ((speaker (speaker interaction)))
    (cond
     ((tutorp speaker) (tutor-speaks experiment interaction))
     ((learnerp speaker) (learner-speaks experiment interaction)))))

(defmethod interact :after ((experiment mwm-experiment)
                            interaction &key)
  (let* ((tutor (find 'tutor (interacting-agents interaction) :key #'id))
         (learner (find 'learner (interacting-agents interaction) :key #'id))
         (topic (find (id (topic tutor)) (objects (context learner)) :key #'id)))
    (when (or (discriminative-set tutor)
              (applied-cxns tutor))
      (align-agent learner topic))))