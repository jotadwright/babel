(ql:quickload :duckie-language-learning)
(in-package :duckie-language-learning)

(monitors::activate-monitor trace-irl)
(activate-monitor trace-fcg)

;;;; START DEMO

(setf *ontology* (build-initial-ontology))

(setf *server-address* "http://192.168.2.5:7000/")

(defparameter *demo*
  (make-instance 'duckie-language-learning-experiment))

(run-interaction *demo*)

;; symbolic evaluation
(setf *duckie-world* (make-instance 'object-set
                                    :objects (list (make-instance 'duckie-building
                                                                  :zone 1
                                                                  :building-function 'house
                                                                  :color 'green
                                                                  :rfid 1)
                                                   (make-instance 'duckie-building
                                                                  :zone 2
                                                                  :building-function 'house
                                                                  :color 'red
                                                                  :rfid 2))))

(set-data *ontology* 'world *duckie-world*)
