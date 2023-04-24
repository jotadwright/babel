(in-package :duckie-language-learning)

;; ------------------------------------------
;; + Primitives for a simulated environment +
;; ------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; SCAN-WORLD ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive scan-world ((world object-set))
  ;;Case 1 world is unbound
  ((=> world)
   (bind (world 1.0 (get-data *ontology* 'world))))
  
  ;;Case 2: world is bound
  ((world =>)
   (equal-entity world (get-data *ontology* 'world)))
  :primitive-inventory *duckie-simulation-primitives*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; MOVE-TO ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive move-to ((zone zone-category))
  ;;first case: object is bound
  ((zone => )
   (setf (zone (get-data *ontology* 'agent-car)) zone)
   ;; return t to make IRL succeed the 'inconsistent' goal-test
   (equal t t))
  :primitive-inventory *duckie-simulation-primitives*)
