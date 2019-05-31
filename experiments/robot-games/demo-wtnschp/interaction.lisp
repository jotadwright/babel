;;;; /interaction.lisp

(in-package :demo-wtnschp)

;; ---------------
;; + Interaction +
;; ---------------

(defmethod interact :before ((experiment demo-experiment) interaction &key)
  "Reset the agent, set the context-size and nr-of-topics and
   generate a scene before every interaction"
  (let ((agent (first (interacting-agents interaction))))
    (reset agent)))

(defmethod interact ((experiment demo-experiment) interaction &key)
  "Run a single interaction"
  (let ((agent (first (interacting-agents interaction)))
        success)
    (cond
     ((speaker? agent)
      (setf success (run-lexical-speaker-task agent)))
     ((hearer? agent)
      (setf success (run-lexical-hearer-task agent))))
    
    (setf (communicated-successfully agent) success)
    (setf (communicated-successfully interaction) success)))