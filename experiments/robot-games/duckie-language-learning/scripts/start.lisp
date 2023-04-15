(ql:quickload :duckie-language-learning)
(in-package :duckie-language-learning)

(monitors::activate-monitor irl::trace-irl)
(activate-monitor trace-fcg)

;;;; START DEMO
(setf *ontology* (build-initial-ontology))

;;; demo in duckie world

(defparameter *demo*
  (make-instance 'duckie-language-learning-experiment))

(run-interaction *demo*)$$

;; demo in simulation
(defparameter *demo*
  (make-instance 'duckie-language-learning-simulation-experiment))

(setf *duckie-world* (make-instance 'object-set
                                    :objects (list 
                                                   (make-instance 'duckie-building
                                                                  :zone 2
                                                                  :building-function 'house
                                                                  :color 'red
                                                                  :rfid 2)
                                                   (make-instance 'duckie-building
                                                                  :zone 1
                                                                  :building-function 'house
                                                                  :color 'green
                                                                  :rfid 1))))

(set-data *ontology* 'world *duckie-world*)

(run-interaction *demo*)

;;;;TESTING IRL PROGRAMS
(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?red)
                        (bind color-category ?red red)
                        (unique ?unique ?out)
                        (query ?answer ?unique ?attribute)
                        (bind attribute-category ?attribute color))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?red)
                        (bind color-category ?red red)
                        (count ?number ?out))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)


(evaluate-irl-program '((scan-world ?world)
                        (count ?number ?world))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?red)
                        (bind color-category ?red red)
                        (unique ?unique ?out)
                        (get-zone ?location ?unique)
                        (move-to ?location))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)


