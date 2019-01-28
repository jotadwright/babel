;;;; ./interaction.lisp

(in-package :robot-colour-game)

;; ---------------
;; + Interaction +
;; ---------------

(defun initialize-agent (agent)
  "Reset the slots of the agent for this interaction"
  (setf (topic agent) nil
        (utterance agent) nil
        (context agent) nil
        (applied-cxn agent) nil
        (applied-cat agent) nil
        (communicated-successfully agent) nil
        (irl-program agent) nil))

(defmethod interact :before ((experiment rcg-experiment) interaction &key)
  "Connect the agents to the robots"
  (let ((available-ips (get-configuration experiment :robot-ips))
        (available-ports (get-configuration experiment :robot-ports))
        (perceptual-deviation (get-configuration experiment :perceptual-deviation)))
    (if perceptual-deviation
      (loop for agent in (interacting-agents interaction)
            for ip in available-ips
            for port in available-ports
            do (connect-to-robot agent ip port)
            do (initialize-agent agent))
      (loop for agent in (interacting-agents interaction)
            for ip in available-ips
            for port in available-ports
            do (connect-to-robot agent ip port)
            do (initialize-agent agent)))))

(defmethod interact ((experiment rcg-experiment) interaction &key)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction))
        (perceptual-deviation (get-configuration experiment :perceptual-deviation)))
    (speak speaker
           (format nil "Interaction ~a starts"
                   (interaction-number interaction)))
    ;; run the speaker task
    (run-speaker-task speaker)
    ;; when successful
    (when (utterance speaker)
      ;; if same perception, pass on the context
      (unless perceptual-deviation
        (setf (context hearer) (context speaker))
        (set-data (ontology hearer) 'context (context speaker)))
      ;; pass on the utterance
      (setf (utterance hearer) (utterance speaker))
      ;; run the hearer task
      (run-hearer-task hearer))
    ;; determine success
    ;; and run the consolidation task
    (loop with success = (determine-success speaker hearer)
          for agent in (interacting-agents interaction)
          do (setf (communicated-successfully agent) success)
          do (run-consolidation-task agent (topic speaker)))))

(defmethod interact :after ((experiment rcg-experiment) interaction &key)
  "Disconnect the agents from the robots"
  (speak (speaker interaction)
         (format nil "Interaction ~a ends"
                 (interaction-number interaction)))
  (loop for agent in (interacting-agents interaction)
        do (disconnect-from-robot agent)))