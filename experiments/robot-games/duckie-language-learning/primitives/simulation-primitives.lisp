(in-package :duckie-language-learning)

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
  ((zone =>)
   (equal 'teste 'teste))
  :primitive-inventory *duckie-simulation-primitives*)