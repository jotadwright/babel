(in-package :cle)

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------

(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :emergence))
                                                    &key (agents nil)
                                                    &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them.

   Corresponds to the emergent scenario."
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

(defmethod determine-interacting-agents (experiment (interaction interaction) (mode (eql :tutor-learner))
                                                    &key (agents nil)
                                                    &allow-other-keys)
  "Tutor-learner setting."
  (declare (ignore mode))
  ;; random or not?
  (if (not agents)
    ;; set two random interacting agents
    (setf (interacting-agents interaction) (list (first (agents experiment))
                                                 (second (agents experiment))))
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

(defmethod determine-disable-channels (experiment amount (mode (eql :none)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop for i from 1 to amount
        collect nil))

(defmethod determine-disable-channels (experiment amount (mode (eql :random)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop for i from 1 to amount
        for disabled = (random-elts (get-configuration experiment :available-channels)
                                    (get-configuration experiment :amount-disabled-channels))
        collect disabled))

(defmethod determine-disable-channels (experiment amount (mode (eql :fixed)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop with disabled = (random-elts (get-configuration experiment :available-channels)
                                     (get-configuration experiment :amount-disabled-channels))
        for i from 1 to amount
        collect disabled))

(defmethod determine-disable-channels (experiment amount (mode (eql :2-groups)))
  "Split the population in two groups, for every group, chooses randomly n channels to be disabled.
   The disabled channels cannot overlap."
  (let* ((population-size amount)
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

(defmethod determine-disable-channels (experiment amount (mode (eql :split-by-color)))
  "Split the population in two groups, for every group, 
    1. disable lab channels for one group
    2. disable rgb channels for the other group."
  (let* ((population-size amount)
         (available-channels (get-configuration experiment :available-channels))
         (amount-channels (get-configuration experiment :amount-disabled-channels))
         (group1 (random-elts available-channels amount-channels))
         (group2 (random-elts (set-difference available-channels group1) amount-channels))
         (repeated-g1 (loop for i from 1 to (/ population-size 2)
                            collect (list 'lab-mean-l 'lab-mean-a 'lab-mean-b 'lab-std-l 'lab-std-a 'lab-std-b)))
         (repeated-g2 (loop for i from 1 to (if (eq (mod population-size 2) 0)
                                              (/ population-size 2)
                                              (/ (+ population-size 1) 2))
                            collect (list 'rgb-mean-r 'rgb-mean-g 'rgb-mean-b 'rgb-std-r 'rgb-std-g 'rgb-std-b))))
    (append repeated-g1 repeated-g2)))
