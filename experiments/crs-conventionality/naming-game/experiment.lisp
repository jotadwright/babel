(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;; Naming game setting ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod interact ((experiment naming-game-experiment)
                     (interaction interaction) &key)
  "Defines a single interaction/game"
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    ;; 1) Perceive new scene and choose topic
    (initialize-interaction experiment interaction speaker hearer)
    ;; 2) The speaker conceptualizes the topic into a meaning
    (conceptualize speaker (get-data speaker :topic) (get-data speaker :scene))
    ;; 3) The speaker formulates the meaning
    (run-formulation speaker (get-data speaker :meaning))
    ;; --- The speaker passes on the formulated utterance to the hearer
    (set-data hearer :utterance (get-data speaker :utterance))
    (notify utterance-passed (get-data speaker :utterance))
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
    (finish-interaction experiment interaction)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Initialize interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-interaction (experiment interaction speaker hearer)
  "Speaker and hearer get a scene and the speaker chooses a topic."
  (let* ((scene (perceive-new-scene (world experiment)
                                   :min (get-configuration experiment :min-nr-of-objects-in-scene)
                                   :max (get-configuration experiment :max-nr-of-objects-in-scene)))
        (topic (select-topic scene
                             :min (get-configuration experiment :min-nr-of-objects-in-topic)
                             :max (get-configuration experiment :max-nr-of-objects-in-topic))))
    (set-data speaker :scene scene)
    (set-data (blackboard (grammar speaker)) :scene scene) ;; For accesssing it from FCG in goal tests
    (set-data speaker :topic topic)
    (set-data hearer :scene scene)
    (set-data (blackboard (grammar hearer)) :scene scene) ;; For accesssing it from FCG in goal tests
    (setf (problems speaker) nil)
    (setf (problems hearer) nil)
    (when (and (get-configuration experiment :trace-every-nth-interaction)
               (or (= (interaction-number interaction) 1) (= 0 (mod (interaction-number interaction)
                                                                    (get-configuration experiment :trace-every-nth-interaction)))))
      (activate-monitor trace-interaction)
      (activate-monitor trace-fcg))
    (notify starting-interaction experiment interaction)
    (notify scene-perceived scene)
    (notify topic-selected topic)))

interact