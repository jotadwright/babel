;;;; /interaction.lisp

(in-package :demo-wtnschp)

;; ---------------
;; + Interaction +
;; ---------------

(defmethod interact :before ((experiment demo-experiment) interaction &key)
  "Reset the agent, set the context-size and nr-of-topics and
   generate a scene before every interaction"
  (let ((agent (first (interacting-agents interaction))))
    (reset agent)
    ;(when (find-data (ontology agent) 'color-categories)
    ;  (display-category-set (get-data (ontology agent) 'color-categories)))
    ))

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

;(defmethod interact :after ((experiment demo-experiment) interaction &key)
;  "Do some things after every interaction"
;  nil)