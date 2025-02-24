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

;; -------------------------------
;; + Determine disabled channels +
;; -------------------------------

(defmethod determine-disable-channels (experiment views amount (mode (eql :none)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop for i from 0 to (- amount 1)
        collect nil))

(defmethod determine-disable-channels (experiment views amount (mode (eql :random)))
  "For every agent, chooses randomly n channels to be disabled.

   Used in experiment: 'Applicability to heteromorphic agents'"
  (loop for i from 0 to (- amount 1)
        for view-name = (first (nth i views)) ;; assumes that agent has one view
        for disabled = (random-elts (get-feature-set (world experiment) view-name)
                                    (get-configuration experiment :amount-disabled-channels))
        collect disabled))


(defmethod determine-disable-channels (experiment views amount (mode (eql :fixed)))
  "For every agent, chooses randomly n channels to be disabled."
  (loop with view-name = (first (nth i views)) ;; assumes that agent has one view
        with disabled = (random-elts (get-feature-set (world experiment) view-name)
                                     (get-configuration experiment :amount-disabled-channels))
        for i from 0 to (- amount 1)
        collect disabled))
