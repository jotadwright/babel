(in-package :mwm)

(defmethod initialize-agent ((agent mwm-agent) clevr-context)
  "Set the context and possibly also the topic of the agent.
   Clear some slots to prepare for the interaction."
  (setf (context agent)
        (if (learnerp agent)
          (clevr->mwm clevr-context)
          clevr-context))
  (setf (topic agent)
        (when (speakerp agent)
          (random-elt (objects (context agent)))))
  (setf (applied-cxns agent) nil
        (discriminative-set agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

(define-event context-determined (context clevr-object-set))

(defmethod interact :before ((experiment mwm-experiment)
                             interaction &key)
  "Initialize the interaction by choosing a random context"
  (let ((context (random-elt (world experiment))))
    (notify context-determined context)
    (loop for agent in (interacting-agents interaction)
          do (initialize-agent agent context))))

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
    (conceptualise speaker (id speaker))
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
  (let ((speaker (speaker interaction)))
    (cond
     ((tutorp speaker) (tutor-speaks experiment interaction))
     ((learnerp speaker) (learner-speaks experiment interaction)))))

(defmethod interact :after ((experiment mwm-experiment)
                            interaction &key)
  (let* ((tutor (find 'tutor (interacting-agents interaction) :key #'id))
         (learner (find 'learner (interacting-agents interaction) :key #'id))
         (topic (find (id (topic tutor)) (objects (context learner)) :key #'id)))
    (align-agent learner topic)))