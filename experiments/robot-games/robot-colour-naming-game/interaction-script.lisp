(in-package :roconaga)

(defun initialize-agent (agent)
  "Reset the slots of the agent for this interaction"
  (setf (topic agent) nil
        (utterance agent) nil
        (context agent) nil
        (applied-cxn agent) nil
        (applied-category agent) nil
        (communicated-successfully agent) t
        (irl-program agent) nil
        (observed-object agent) nil))

(defmethod interact :before ((experiment grounded-color-naming-game-experiment)
                             interaction &key)
  "Prepare the interaction"
  (let* ((min-context-size (get-configuration (experiment interaction) :min-context-size))
         (max-context-size (get-configuration (experiment interaction) :max-context-size))
         (context-size (random-from-range min-context-size max-context-size)))
    (set-data interaction 'context-size context-size)
    (loop for agent in (interacting-agents interaction)
          do (initialize-agent agent))))

(defmethod interact ((experiment grounded-color-naming-game-experiment)
                     interaction &key)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    ;; 1
    (embody speaker (first (robots experiment)))
    (embody hearer (second (robots experiment)))
    ;; 2
    (agent-observe-world speaker)
    (agent-observe-world hearer)
    ;; 3
    (choose-topic speaker (world speaker))
    ;; 4
    (conceptualise speaker (topic speaker) (world speaker))
    ;; 5
    (produce-utterance speaker (meaning-representation speaker))
    ;; 6
    (pass-utterance speaker hearer (utterance speaker))
    ;; 7
    (comprehend-utterance hearer (observed-utterance hearer))
    ;; 8
    (interpret hearer (meaning-representation hearer))
    ;; 9
    (point-and-observe hearer (hypothesized-topic hearer) speaker)
    ;; 10
    (if (determine-success speaker hearer)
      (nod speaker)
      (point-and-observe speaker (topic speaker) hearer))
    ;; 11
    (align-agent speaker (topic speaker))
    (align-agent hearer (topic speaker))))

#|
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
|#