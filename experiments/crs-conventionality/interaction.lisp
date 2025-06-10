(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                          ;;
;;  Code implementing interact/interaction  ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
    (switch-conditions experiment)
    
    ;; Determine the speaker and hearer agents as well as the scene and topic, notify that interaction can start.
    (determine-interacting-agents experiment interaction (get-configuration experiment :determine-interacting-agents-mode))
    (determine-scene-entities experiment interaction (get-configuration experiment :determine-scene-entities-mode))
    (determine-topic experiment interaction (get-configuration experiment :determine-topic-mode))
    (notify interaction-started experiment interaction (interaction-number interaction))
    
    ;; Run the actual interaction
    (interact experiment interaction) ;

    ;; Round up the interaction
    (notify interaction-finished experiment interaction (interaction-number interaction))
    (clear-interaction interaction)
    (values interaction experiment)))

(defun switch-conditions (experiment)
  (when (eq (get-configuration experiment :introduce-new-agents-after-interaction)
            (- (interaction-number (current-interaction experiment)) 1))
    (introduce-new-agents experiment :number-of-agents (get-configuration experiment :nr-of-agents-to-introduce))
    (set-configuration experiment :determine-interacting-agents-mode :random-listener-from-younger-generation))
  
  (when (eq (get-configuration experiment :replace-agents-after-interaction)
            (- (interaction-number (current-interaction experiment)) 1))
    (replace-agents experiment (get-configuration experiment :proportion-of-agents-to-replace))))


;; Determine interacting agents, scene and topic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clear-interaction ((interaction crs-conventionality-interaction))
  "Resets the necessary slots of the interaction and the interacting-agents."
  (loop for agent in (interacting-agents interaction)
        do (progn (setf (topic agent) nil
                 (computed-topic agent) nil
                 (utterance agent) nil
                 (conceptualised-utterance agent) nil
                 (applied-constructions agent) nil
                 (solution-nodes agent) nil)
            (set-data (blackboard (grammar agent)) :topic nil)
            (set-data (blackboard (grammar agent)) :cipn nil)
            (set-data (blackboard (grammar agent)) :scene nil)
            (set-data (blackboard (grammar agent)) :agent nil)
            (when (find-data (blackboard (grammar agent)) :concept-similarity)
              (set-data (blackboard (grammar agent)) :concept-similarity nil))
            (when (find-data (blackboard (grammar agent)) :discriminative-power)
              (set-data (blackboard (grammar agent)) :discriminative-power nil))
            ))
  (when (equalp (type-of (experiment interaction)) 'concept-emergence-game-experiment)
    (set-data (get-data (blackboard *concept-emergence-game-primitives*) :ontology) :concepts nil))
  (setf (communicated-successfully interaction) nil
        (interacting-agents interaction) nil))

(defun has-old-neighbor (agent)
  (some (lambda (neighbor)
          (= (introduced-in-game neighbor) 0))
        (social-network agent)))

(defmethod determine-interacting-agents (experiment (interaction interaction)
                                                    (mode (eql :random-listener-from-younger-generation))
                                                    &key &allow-other-keys)
  "Randomly chooses an agent from the older generation as the speaker and randomly chooses an agent from a new generation as listener."  
  (let* ((agents (agents (population experiment)))
         (new-generations (loop for agent in agents
                                if (> (introduced-in-game agent) 0)
                                collect agent))
         (hearer (random-elt-if #'has-old-neighbor new-generations))
         (speaker (random-elt (social-network hearer)))
         (interacting-agents (list speaker hearer)))

    ;; set the discourse-role
    (setf (discourse-role speaker) 'speaker
          (discourse-role hearer) 'hearer
          (interacting-agents interaction) interacting-agents)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents ((experiment crs-conventionality-experiment)
                                         (interaction interaction) 
                                         (mode (eql :random-from-social-network))
                                         &key &allow-other-keys)
  "This method randomly picks one agent, and then randomly picks an agent 
   from its social network. In a fully connected network, this is the same 
   as picking two random agents."
  (let* ((agents (if (listp (agents experiment))
                   (agents experiment)
                   (agents (population experiment))))
         (speaker (random-elt agents))
         (hearer (random-elt (social-network speaker)))
         (interacting-agents (list speaker hearer)))

    ;; set the discourse-role
    (setf (discourse-role speaker) 'speaker
          (discourse-role hearer) 'hearer
          (interacting-agents interaction) interacting-agents)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :boltzmann-partner-selection))
                                                    &key &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  (let* (;; select a random agent
         (agent (random-elt (agents (population experiment))))
         ;; select its partner based on its preferences
         (preferred-partner (choose-partner agent
                                            (agents (population experiment))
                                            (get-configuration experiment :boltzmann-tau)))
         ;; shuffle the two agents around so that speaker/hearer role assignment is random
         (interacting-agents (shuffle (list agent preferred-partner))))
    ;; set interacting-agents
    (setf (interacting-agents interaction) interacting-agents))
  
  ;; set the discourse-role
  (loop for a in (interacting-agents interaction)
        for d in '(speaker hearer)
        do (setf (discourse-role a) d))
  (notify interacting-agents-determined experiment interaction))



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

(defmethod interact ((experiment crs-conventionality-experiment)
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
    
    ;; Determine success and coherence
    (determine-success speaker hearer interaction)
    (determine-coherence speaker hearer) ;; check coherence before alignment!

    ;; Feedback
    (provide-feedback speaker hearer)

    ;; Alignment
    (unless (invention interaction)
      (align speaker hearer interaction (get-configuration experiment :alignment-strategy)))

    ;; Adoption
    (unless (communicated-successfully interaction)
      (adopt (topic interaction) hearer))

    ;; Finishing interaction
    (finish-interaction experiment interaction)))

;; helper functions (best placed somewhere else?)

(defmethod utter ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent))
  "The utterance is copied from the speaker to the hearer, noise may be added."
  (setf (utterance speaker) (conceptualised-utterance speaker))
  (if (get-configuration (experiment speaker) :introduce-noise-after-interaction)
    (if (< (get-configuration (experiment speaker) :introduce-noise-after-interaction)
           (interaction-number (current-interaction (experiment speaker))))
      (setf (utterance hearer) (list (add-noise (copy-object (first (utterance speaker))) (get-configuration (experiment speaker) :noise-level)))) 
      (setf (utterance hearer) (utterance speaker)))
    (setf (utterance hearer) (utterance speaker))))


(defmethod provide-feedback ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent))
  "Speaker provides feedback by pointing to the topic."
  (setf (topic hearer) (topic speaker)))

(defmethod determine-success ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent) (interaction crs-conventionality-interaction))
  "Determines and sets success. There is success if the computed-topic of the hearer is the same as the intended topic of the speaker."
  (if (equalp (computed-topic hearer) (first (entities (topic speaker)))) ;; pointing
    (setf (communicated-successfully interaction) t)
    (setf (communicated-successfully interaction) nil)))


(defmethod determine-coherence ((speaker crs-conventionality-agent) (hearer crs-conventionality-agent))
  "Determines and sets the coherences. Tests whether the hearer would have used the same word as the speaker, should the hearer have been the speaker."
  (let* ((interaction (current-interaction (experiment speaker)))
         (scene (scene interaction))
         (topic (topic interaction)))
    (conceptualise-and-produce hearer scene topic :use-meta-layer nil)
    (if (equalp (utterance speaker) (conceptualised-utterance hearer))
      (setf (coherence interaction) t)
      (setf (coherence interaction) nil))
    (notify determine-coherence-finished speaker hearer)))


(defmethod finish-interaction ((experiment crs-conventionality-experiment) (interaction crs-conventionality-interaction))
  ;; update neighbor-q-values
  (let ((reward (if (communicated-successfully interaction) 1 0))
        (lr (get-configuration experiment :neighbor-q-value-lr)))
    (update-neighbor-q-value (speaker experiment) (hearer experiment) reward (if lr lr 0.01))
    (update-neighbor-q-value (hearer experiment) (speaker experiment) reward (if lr lr 0.01))))


;; ---------------------
;; + Partner selection +
;; ---------------------

(defun calculate-new-q-value (q-value reward lr)
  (+ q-value (* lr (- reward q-value))))

(defun boltzmann-exploration (q-values tau)
  "Boltzmann exploration for partner selection.

   tau corresponds to an inverse temperature:
     if tau = 0: no preference, random sampling
     if tau > 0: preference to select partners you understand well
     if tau < 0: curiosity-driven partner selection

    Inspired by Leung's et al. paper on curiosity-driven partner selection (2025)."
  (let* ((exp-values (mapcar (lambda (q) (exp (* tau q))) q-values))
         (total-sum (reduce #'+ exp-values)))
    (mapcar (lambda (exp-value) (/ exp-value total-sum)) exp-values)))

(defun sample-partner (neighbors probabilities)
  "Randomly sample a partner from the neighbors using the given probabilities."
  (loop with r = (random 1.0)
        with cumulative = 0.0
        for neighbor in neighbors
        for probability in probabilities
        do (setf cumulative (+ cumulative probability))
        when (<= r cumulative)
          return neighbor
          ;; if the sum of probabilities is not equal to 1 (due to some numerical stability)
          ;; always make a decision and pick the last agent
        finally (return (car (last neighbors)))))

(define-event event-partner-selection
  (agent crs-conventionality-agent)
  (neighbors list)
  (q-values list)
  (probabilities list)
  (partner-id symbol))

(defmethod choose-partner ((agent crs-conventionality-agent) (other-agents list) (tau number))
  "Choose a partner for a given agent using Boltzmann exploration."

  (let* ((neighbors (hash-keys (neighbor-q-values agent)))
         (q-values (hash-values (neighbor-q-values agent)))
         (probabilities (boltzmann-exploration q-values tau))
         ;; sample a partner (by id)
         (partner-id (sample-partner neighbors probabilities))
         ;; match id to the other-agents
         (partner (find partner-id other-agents :test (lambda (x y) (eq x (id y))))))

    (notify event-partner-selection agent neighbors q-values probabilities partner-id)
    partner))


(defmethod initialise-neighbor-q-values ((agent crs-conventionality-agent) &key (initial-q 0.5))
  "Let agent store a Q-value for all of its neighbors, initialised at initial-q."
  (loop for neighbor in (social-network agent)
        do (insert-neighbor-q-value agent neighbor :initial-q initial-q)))

(defmethod insert-neighbor-q-value ((agent crs-conventionality-agent) (neighbor crs-conventionality-agent) &key (initial-q 0.5))
  "Let agent store a Q-value for neighbor, initialised at initial-q."
  (setf (gethash (id neighbor) (neighbor-q-values agent))
        initial-q))

(defmethod remove-neighbor-q-value ((agent crs-conventionality-agent) (neighbor crs-conventionality-agent))
  (remhash (id neighbor) (neighbor-q-values agent)))

(defmethod update-neighbor-q-value ((agent crs-conventionality-agent) (neighbor crs-conventionality-agent) (reward number)  (lr float))
  "Update the q-value that agent stores for neighbor."
  (let* ((q-values (neighbor-q-values agent))
         (q-old (gethash (id neighbor) q-values))
         (q-new (calculate-new-q-value q-old reward lr)))
    (setf (gethash (id neighbor) q-values) q-new)))
