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
    
    ;; The speaker conceptualizes the topic into a meaning representation
    (conceptualise-and-produce speaker scene topic)
    #|
    ;; 3) The speaker formulates the meaning
    (run-formulation speaker (get-data speaker :meaning))
    ;; --- The speaker passes on the formulated utterance to the hearer
    (set-data hearer :utterance (get-data speaker :utterance))
    ;(notify utterance-passed (get-data speaker :utterance))
    ;; 4) The hearer comprehends the utterance
    (run-comprehension hearer (get-data hearer :utterance))
    ;; 5) The hearer interprets the meaning in the scene
    (interpret hearer (get-data hearer :meaning) (get-data hearer :scene))
    ;; 6) Compare intended topic of speaker and interpreted topic of hearer
    ;;    and set communicated-succesfully for speaker, hearer and interaction
    (compare-topics speaker hearer interaction)
    ;; 7) If not succesful, then learning phase for hearer
    (unless (communicated-successfully hearer)
      (run-learning hearer (get-data speaker :topic) (get-data hearer :cipn)))
    ;; 8) Speaker and hearer update the scores of the constructions used and their competitors
    (align-agent speaker (get-configuration experiment :alignment))
    (align-agent hearer (get-configuration experiment :alignment))
    ;; Finishing interaction
    (finish-interaction experiment interaction)
    |#
    ))


