(ql:quickload :duckie-language-learning)
(in-package :duckie-language-learning)

(monitors::activate-monitor irl::trace-irl)
(activate-monitor trace-fcg)

;;;; START DEMO

(setf *ontology* (build-initial-ontology))

(setf *server-address* "http://192.168.2.5:7000/")

(defparameter *demo*
  (make-instance 'duckie-language-learning-experiment))

(run-interaction *demo*)

;; symbolic evaluation
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


;;;;TESTING IRL PROGRAMS
(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?red)
                        (bind color-category ?red red)
                        (unique ?unique ?out)
                        (query ?answer ?unique ?attribute)
                        (bind attribute-category ?attribute color))
                      *ontology*
                      :primitive-inventory *duckie-primitives*)

(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?red)
                        (bind color-category ?red red)
                        (count ?number ?out))
                      *ontology*
                      :primitive-inventory *duckie-primitives*)


(evaluate-irl-program '((scan-world ?world)
                        (count ?number ?world))
                      *ontology*
                      :primitive-inventory *duckie-primitives*)


