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
