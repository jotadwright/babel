(in-package :cle)

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------

(defmethod set-agents ((experiment cle-experiment) agents)
  "Set the agents of the experiment."
  (setf (interacting-agents interaction) agents)

  (loop for a in (interacting-agents interaction)
        for d in '(speaker hearer)
        do (setf (discourse-role a) d))

  (notify interacting-agents-determined experiment interaction))


(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :standard))
                                                    &key &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  ;; set two random interacting agents
  (let* ((agents (agents experiment))
         (agent-1 (random-elt agents))
         (agent-2 (random-elt (social-network agent-1)))
         (interacting-agents (list agent-1 agent-2)))
    (setf (interacting-agents interaction) interacting-agents))
  
  ;; set discourse-role
  (loop for a in (interacting-agents interaction)
        for d in '(speaker hearer)
        do (setf (discourse-role a) d))
  (notify interacting-agents-determined experiment interaction))

(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :boltzmann-partner-selection))
                                                    &key &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  
  (let* (;; select a random agent
         (agent-1 (random-elt (agents experiment)))
         ;; select its partner based on its preferences
         (agent-2 (choose-partner agent-1
                                  (social-network agent-1)
                                  (get-configuration experiment :boltzmann-tau)))
         ;; shuffle the two agents around so that speaker/hearer role assignment is random
         (interacting-agents (shuffle (list agent-1 agent-2))))
    (setf (interacting-agents interaction) interacting-agents))
  
  ;; set discourse-role
  (loop for a in (interacting-agents interaction)
        for d in '(speaker hearer)
        do (setf (discourse-role a) d))
  (notify interacting-agents-determined experiment interaction))

;; -------------------
;; + Determine views +
;; -------------------

(defmethod determine-views (experiment (mode (eql :exclusive-views)))
  "Every agent is assigned to a specific view. The group is split up depending on the group."
  (loop with population-size = (get-configuration experiment :population-size)
        with view-names = (get-configuration experiment :dataset)
        for view-name in view-names
        for group = (loop for i from 1 to (/ population-size (length view-names))
                          collect (list view-name))
        collect group into groups
        finally (return (mapcan #'identity groups))))

(defmethod determine-views (experiment (mode (eql :shared-views)))
  "Every agent has access to every view."
  (loop for i from 1 to (get-configuration experiment :population-size)
        collect (get-configuration experiment :dataset)))
