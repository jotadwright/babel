(in-package :grounded-color-naming-game)

(defun initialize-agent (agent)
  "Reset the slots of the agent for this interaction"
  (setf (topic agent) nil
        (utterance agent) nil
        (context agent) nil
        (applied-cxn agent) nil
        (applied-category agent) nil
        (communicated-successfully agent) t
        (irl-program agent) nil))

(defun connect-agents-to-robots (interaction)
  "Connect the agents to the robots"
  (let ((robots (robots (experiment interaction))))
    ;;;; hardcoded for a single robot!!!!
    (loop with robot = (first robots)
          for agent in (shuffle (interacting-agents interaction))
          do (setf (robot agent) robot)
          unless (nao-connected-p robot)
          do (make-new-connection robot :test-connection
                                  (not (get-configuration (experiment interaction) :silent))))))

(defun prepare-interaction (interaction)
  "Prepare the interaction"
  (loop for agent in (interacting-agents interaction)
        do (initialize-agent agent))
  (connect-agents-to-robots interaction)
  (unless (get-configuration (experiment interaction) :silent)
    (speak (robot (speaker interaction))
           (format nil "Interaction ~a starts"
                   (interaction-number interaction)))))

(defun interaction-fails (interaction)
  (loop for agent in (interacting-agents interaction)
        do (setf (communicated-successfully agent) nil)))
  
(defmethod interact ((experiment grounded-color-naming-game-experiment)
                     interaction &key)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    ;;;; 0. Preparing the interaction, e.g.
    ;;;;    opening the connection to the robot
    (prepare-interaction interaction)
    ;;;; 1. Sensorimotor level
    ;;;;    The agents observe the world around them
    ;;;;    and store this in their 'context' slot
    (agents-observe-world interaction)
    ;;;; 2. Conceptual level
    ;;;;    The agent first chooses a topic and stores
    ;;;;    this in its 'topic' slot. Next, the speaker
    ;;;;    tries to conceptualise the topic. If a
    ;;;;    discriminating color category is found, it
    ;;;;    is stored in the agent's 'applied-category'
    ;;;;    slot. If no such category is found, a new one
    ;;;;    is created based on the topic and the agent
    ;;;;    again tries to conceptualise. This can still
    ;;;;    fail!
    (choose-topic speaker (context speaker))
    (unless (conceptualise speaker (topic speaker))
      (create-new-category speaker (topic speaker))
      (conceptualise speaker (topic speaker)))
    ;;;; 3. Language level
    ;;;;    Given that conceptualisation succeeded,
    ;;;;    the agent looks in its lexicon for the word
    ;;;;    that best describes the discriminating color
    ;;;;    category. If no such word is found, a new one
    ;;;;    is invented and production is tried again.
    ;;;;    The found or invented word is
    ;;;;    stored in the 'utterance' slot of the agent.
    ;;;;    The form-meaning mapping that was used is
    ;;;;    stored in the 'applied-cxn' slot of the agent.
    (if (applied-category speaker)
      (progn (unless (produce-word speaker (applied-category speaker))
               (invent speaker (applied-category speaker))
               (produce-word speaker (applied-category speaker)))
        ;;;; 4. The utterance is passed from speaker to hearer
        (pass-utterance speaker hearer (utterance speaker))
        ;;;; 5. Language level
        ;;;;    The hearer looks in its lexicon for the word
        ;;;;    said by the speaker. The form-meaning mapping
        ;;;;    that is found is stored in the 'applied-cxn'
        ;;;;    slot of the agent.
        ;;;; 6. Conceptual level
        ;;;;    The hearer looks for the object in the world
        ;;;;    that best fits with the color category associated
        ;;;;    to the utterance. This category can be accessed
        ;;;;    through the 'applied-cxn' of the hearer. If an
        ;;;;    object is found, it is stored in the 'topic' slot
        ;;;;    of the agent
        (if (and (comprehend-utterance hearer (utterance hearer))
                 (interpret hearer (applied-cxn hearer)))
          ;;;; 7. The speaker checks if the topic pointed to by
          ;;;;    the hearer is indeed the topic he intended.
          ;;;;    This determines the success of the interaction.
          ;;;;    If unsuccessful, the hearer has a learning
          ;;;;    oppurtuniy. The speaker points to the correct
          ;;;;    object. The hearer finds the category that best
          ;;;;    fits the topic and stores a new mapping between
          ;;;;    the utterance and this category.
          (unless (determine-success speaker hearer)
            (adopt hearer (topic speaker))
            (interaction-fails interaction))
          ;;;; 8. If either parsing or interpretation already failed,
          ;;;;    the hearer also needs to learn. This works in the
          ;;;;    same way as in 7.
          (progn (adopt hearer (topic speaker))
            (interaction-fails interaction))))
      (interaction-fails interaction))
      ;;;; 9. After the interaction, both agents align their lexicon
    ;;;;    to be more succesful in the future. Alignment depends
    ;;;;    on the success of the interaction.
    (align-agents interaction)
    ;;;; 10. Finish up the interaction, e.g. disconnect from
    ;;;;     the robot.
    (finish-interaction interaction)))

(defun align-agents (interaction)
  "Align both agents based on the success of the interaction.
   Pass alogn the correct topic, for shifting the prototype"
  (loop for agent in (interacting-agents interaction)
        do (align agent (topic (speaker interaction))
                  (get-configuration (experiment interaction) :alignment-strategy))))

(define-event alignment-started (agent grounded-color-naming-game-agent))

(defmethod align :around ((agent grounded-color-naming-game-agent)
                          correct-topic strategy)
  (case (get-configuration agent :who-aligns)
    (:speaker (when (speakerp agent) (call-next-method)))
    (:hearer (when (hearerp agent) (call-next-method)))
    (:both (call-next-method))))

(defmethod align ((agent grounded-color-naming-game-agent)
                  correct-topic (strategy (eql :li)))
  ;;;; If the interaction was a success, the agents shift their
  ;;;; applied color category slightly towards the topic. Also, they
  ;;;; reward the applied form/meaning mapping and punish its competitors

  ;;;; If the interaction was not a success, the agents punish the applied
  ;;;; form/meaning mapping, if there is any. Remember that the interaction
  ;;;; can fail before there is an applied form/meaning mapping.
  (if (communicated-successfully agent)
    (progn (notify alignment-started agent)
      (shift-color-prototype agent (applied-category agent) correct-topic)
      (reward-applied-cxn-and-punish-competitors agent (applied-cxn agent)))
    (when (applied-cxn agent)
      (notify alignment-started agent)
      (punish-applied-cxn agent (applied-cxn agent)))))

(defun finish-interaction (interaction)
  (unless (get-configuration (experiment interaction) :silent)
    (speak (robot (speaker interaction))
           (format nil "Interaction ~a ends"
                   (interaction-number interaction))))
  ;(loop for agent in (interacting-agents interaction)
  ;      do (setf (robot agent) nil))
  )