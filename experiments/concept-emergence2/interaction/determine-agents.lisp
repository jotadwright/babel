(in-package :cle)

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------

(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :standard))
                                                    &key (agents nil)
                                                    &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  (declare (ignore mode))
  ;; random or not?
  (if (not agents)
    ;; set two random interacting agents
    (setf (interacting-agents interaction) (random-elts (agents experiment) 2))
    ;; set the specified agents
    (setf (interacting-agents interaction) agents))
  
  ;; set discourse-role
  (loop for a in (interacting-agents interaction)
        for d in '(speaker hearer)
        do (setf (discourse-role a) d))
  (notify interacting-agents-determined experiment interaction))

;; -------------------------------
;; + Determine disabled channels +
;; -------------------------------

(defmethod determine-disable-channels (experiment (mode (eql :none)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop for i from 1 to (get-configuration experiment :population-size)
        collect nil))

(defmethod determine-disable-channels (experiment (mode (eql :random)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop for i from 1 to (get-configuration experiment :population-size)
        for disabled = (random-elts (get-configuration experiment :available-channels)
                                    (get-configuration experiment :amount-disabled-channels))
        collect disabled))

(defmethod determine-disable-channels (experiment (mode (eql :2-groups)))
  "Split the population in two groups, for every group, chooses randomly n channels to be disabled.
   The disabled channels cannot overlap."
  (let* ((population-size (get-configuration experiment :population-size))
         (available-channels (get-configuration experiment :available-channels))
         (amount-channels (get-configuration experiment :amount-disabled-channels))
         (group1 (random-elts available-channels amount-channels))
         (group2 (random-elts (set-difference available-channels group1) amount-channels))
         (repeated-g1 (loop for i from 1 to (/ population-size 2)
                            collect group1))
         (repeated-g2 (loop for i from 1 to (if (eq (mod population-size 2) 0)
                                              (/ population-size 2)
                                              (/ (+ population-size 1) 2))
                            collect group2)))
    (append repeated-g1 repeated-g2)))
