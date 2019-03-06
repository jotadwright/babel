(in-package :gcng)

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
      (agent-nod speaker)
      (point-and-observe speaker (topic speaker) hearer))
    ;; 11
    (align-agent speaker (topic speaker))
    (align-agent hearer (topic speaker))))