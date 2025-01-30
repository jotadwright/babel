(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; Code implementing interact/interaction ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod run-interaction ((experiment crs-conventionality-experiment)
                            &key &allow-other-keys)
  "Runs an interaction in the experiment."
  (let (;; Make an instance of 'interaction and add it to the experiment
        (interaction (make-instance 'crs-conventionality-interaction
                                    :experiment experiment
                                    :interaction-number (if (interactions experiment)
                                                          (+ 1 (interaction-number (current-interaction experiment)))
                                                          1))))
    (push interaction (interactions experiment))

    ;; Determine the speaker and hearer agents as well as the scene and topic, notify that interaction can start.
    (determine-interacting-agents experiment interaction (get-configuration experiment :determine-interacting-agents-mode))
    (determine-scene-entities experiment interaction (get-configuration experiment :determine-scene-entities-mode))
    (determine-topic experiment interaction (get-configuration experiment :determine-topic-mode))
    (notify interaction-started experiment interaction (interaction-number interaction))
    
    ;; Run the actual interaction
    (interact experiment interaction)

    ;; Round up the interaction
    (setf (communicated-successfully interaction) (loop for agent in (interacting-agents interaction)
                                                        always (communicated-successfully agent)))
    (notify interaction-finished experiment interaction (interaction-number interaction))
    (values interaction experiment)))



;; Determine interacting agents, scene and topic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod determine-interacting-agents (experiment (interaction interaction)
                                                    (mode (eql :random-from-population))
                                                    &key &allow-other-keys)
  "Randomly chooses two interacting agents and adds the discourse roles speaker and hearer to them."
  (let ((agents (agents (population experiment))))
    (setf (interacting-agents interaction)
          (if (> (length agents) 1)
            (random-elts agents 2)
            agents))
    (loop for a in (interacting-agents interaction)
          for d in '(speaker hearer)
          do (setf (discourse-role a) d)
             (setf (utterance a) nil)
             (setf (communicated-successfully a) nil))
    (notify interacting-agents-determined experiment interaction)))


(defgeneric determine-scene-entities (experiment interaction mode)
  (:documentation "Creates a scene for an interaction."))

(defmethod determine-scene-entities (experiment interaction (mode (eql :random-subset-of-world)))
  "Creates a scene and sets it in the interaction."
  (setf (scene interaction) (make-instance 'crs-conventionality-scene
                                           :interaction interaction
                                           :entities (random-elts (entities (world experiment))
                                                                  (get-configuration experiment :nr-of-entities-in-scene)))))


(defgeneric determine-topic (experiment interaction mode)
  (:documentation "Sets the topic for an interaction."))

(defmethod determine-topic (experiment interaction (mode (eql :random-entity-from-scene)))
  "Sets the topic for an interaction."
  (setf (topic interaction) (make-instance 'crs-conventionality-entity-set
                                           :entities (list (random-elt (entities (scene interaction)))))))



;; Interact ;;
;;;;;;;;;;;;;;


(defmethod interact ((experiment naming-game-experiment)
                     (interaction interaction) &key)
  "Defines a single interaction/game in the naming game experiment."
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction))
        (scene (scene interaction))
        (topic (topic interaction)))
    
    ;; The speaker conceptualizes the topic into a meaning representation and formulates it.
    (conceptualise-and-produce speaker scene topic)

    ;; Speaker says utterance to hearer. 
    (utter speaker hearer)

    ;; Hearer comprehends and interprets the meaning. 
    (comprehend-and-interpret hearer scene)
    
    ;; Determine success
    (determine-success speaker hearer interaction)

    ;; Feedback
    (provide-feedback speaker hearer)

    ;; Adoption and alignment
    (align speaker hearer interaction (get-configuration experiment :alignment-strategy))
    
    #|
    ;; Finishing interaction
    (finish-interaction experiment interaction)
    |#
    ))


(defmethod utter ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent))
  "The utterer utters the utterance to the utteree."
  (setf (utterance hearer) (utterance speaker)))

(defmethod provide-feedback ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent))
  "Speaker provides feedback by pointing to the topic."
  (setf (topic hearer) (topic speaker)))
