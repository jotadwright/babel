;;;; /interaction.lisp

(in-package :roos)

;; ---------------
;; + Interaction +
;; ---------------

(defmethod interact :before ((experiment roos-experiment) interaction &key)
  "Reset the agent, set the context-size and nr-of-topics and
   generate a scene before every interaction"
  (let ((agent (first (interacting-agents interaction)))
        (game-stage (get-configuration experiment :game-stage)))
    (reset agent)
    (case game-stage
      (:lexical (progn
                  (set-configuration experiment
                                     :context-size
                                     (get-configuration experiment :lexical-context-size)
                                     :replace t)
                  (set-configuration experiment
                                     :nr-of-topics
                                     (get-configuration experiment :lexical-nr-of-topics)
                                     :replace t)))
      (:grammatical (progn
                      (set-configuration experiment
                                         :context-size
                                         (random-from-range (get-configuration experiment :min-context-size)
                                                            (get-configuration experiment :max-context-size))
                                         :replace t)
                      (set-configuration experiment
                                         :nr-of-topics
                                         (random-from-range (get-configuration experiment :min-nr-of-topics)
                                                            (get-configuration experiment :max-nr-of-topics))
                                         :replace t))))
    (generate-unique-scene (get-configuration experiment :context-size))))

(defmethod interact ((experiment roos-experiment) interaction &key)
  "Run a single interaction"
  (let ((agent (first (interacting-agents interaction)))
        (game-stage (get-configuration experiment :game-stage))
        success)
    (cond
     ((and (speaker? agent)
           (eql game-stage :lexical))
      (setf success (run-lexical-speaker-task agent)))
     ((and (speaker? agent)
           (eql game-stage :grammatical))
      (setf success (run-grammatical-speaker-task agent)))
     
     ((and (hearer? agent)
           (eql game-stage :lexical))
      (setf success (run-lexical-hearer-task agent)))
     ((and (hearer? agent)
           (eql game-stage :grammatical))
      (setf success (run-grammatical-hearer-task agent))))
    
    (setf (communicated-successfully agent) success)
    (setf (communicated-successfully interaction) success)))

(defmethod interact :after ((experiment roos-experiment) interaction &key)
  "Do some things after every interaction"
  nil)