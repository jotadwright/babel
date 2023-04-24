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

(defprimitive move-to ((car duckie-car)
                       (zone zone-category))
  ;;first case: object is bound
  ((zone => car)
   ;;set location duckie-car from world to zone
   (setf (zone (get-data *ontology* 'agent-car)) (zone zone))
   (bind (car 1.0 (get-data *ontology* 'agent-car))))


  ((car zone => )
   (setf (zone (get-data *ontology* 'agent-car)) (zone zone))
   ;; return t to make IRL succeed the 'inconsistent' goal-test
   (equal (zone car) (zone zone)))
  :primitive-inventory *duckie-simulation-primitives*)
