(ql:quickload :duckie-language-learning)
(in-package :duckie-language-learning)

(progn
  ;;;; START DEMO
  (deactivate-all-monitors)
  (monitors::activate-monitor irl::trace-irl)
  ;(monitors::activate-monitor irl::trace-irl-verbose)
  (monitors::activate-monitor fcg::trace-fcg)
  (setf *ontology* (build-initial-ontology))
  (add-element (make-html *ontology*))

  ;;; demo in duckie world
  ;(defparameter *demo*
  ;  (make-instance 'duckie-language-learning-world-experiment))
  ;(run-interaction *demo*)

  ;; demo in simulation
  (defparameter *demo*
    (make-instance 'duckie-language-learning-simulation-experiment))

  (defparameter *duckie-world*
    (make-instance 'object-set
                   :objects (list
                             (make-instance 'duckie-building
                                            :zone 'zone-1
                                            :building-function 'bakery
                                            :color 'yellow
                                            :rfid 1)
                             (make-instance 'duckie-building
                                            :zone 'zone-2
                                            :building-function 'restaurant
                                            :color 'blue
                                            :rfid 2)
                             (make-instance 'duckie-building
                                            :zone 'zone-3
                                            :building-function 'bakery
                                            :color 'green
                                            :rfid 3)
                             (make-instance 'duckie-car
                                            :zone 'zone-4
                                            :color 'pink
                                            :rfid 4)
                             )))

  (setf *duckie-agent-car* (make-instance 'duckie-agent-car :zone 'zone-1))


  ;; duckie-car also in ontology for move-to primitive
  (set-data *ontology* 'world *duckie-world*)
  (set-data *ontology* 'agent-car *duckie-agent-car*))

(run-interaction *demo*)

;; quick reset
(progn
  (wi::reset)
  (defparameter *demo*
    (make-instance 'duckie-language-learning-simulation-experiment))
  (run-interaction *demo*))

what is the color of the house
what is the color of the car
where is the car
where is the bakery
what is the color of the bakery

(run-interaction *demo*)

;;;;
(progn
  (setf *duckie-world*
        (make-instance 'object-set
                       :objects (list
                                 (make-instance 'duckie-building
                                                :zone 'zone-1
                                                :building-function 'bakery
                                                :color 'yellow
                                                :rfid 1)
                                 (make-instance 'duckie-car
                                                :zone 'zone-2
                                                :color 'green
                                                :rfid 5)
                                 (make-instance 'duckie-car
                                                :zone 'zone-3
                                                :color 'pink
                                                :rfid 4)
                                 )))
  ;; duckie-car also in ontology for move-to primitive
  (set-data *ontology* 'world *duckie-world*))

(run-interaction *demo*)


;;;;;;;

(first (agents *demo*))

;;;;;
(evaluate-irl-program '(
                        (scan-world ?world)
                        (filter ?out ?world ?cat)
                        (bind object-type-category ?cat duckie-car)
                        (unique ?unique ?out)
                        (get-zone ?zone ?unique)
                        (move-to ?duckie-car ?zone)
                        )
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

(evaluate-irl-program '(
                        (scan-world ?world)
                        (filter ?out ?world ?color)
                        (bind color-category ?color yellow)
                        (unique ?unique ?out)
                        (get-zone ?zone ?unique)
                        (move-to ?zone)
                        )
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

;;;

(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?color)
                        (bind color-category ?color yellow)
                        (unique ?unique ?out)
                        (query ?answer ?unique ?attribute)
                        (bind attribute-category ?attribute color))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)



;;;;;
what is the color of the green house
what is the color of the car
what is the building-function of the house
where is the car
where is the house
what is the color of the house

(run-interaction *demo*)

(setf cn (categorial-network (grammar (first (agents *demo*)))))
(add-element (make-html cn))

(wi:add-element (make-html (categorial-network *fcg-constructions*) :weights? t :colored-edges-0-1 t))

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
                        (filter ?out ?world ?zone)
                        (bind zone-category ?zone zone-1)
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
                        (move-to ?car ?location))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

;;;; TESTING IN WORLD

(evaluate-irl-program '((scan-world ?world)
                        (filter ?out ?world ?zone)
                        (bind zone-category ?zone zone-1)
                        (unique ?unique ?out)
                        (get-zone ?location ?unique)
                        (move-to ?car ?location))
                      *ontology*
                      :primitive-inventory *duckie-simulation-primitives*)

(evaluate-irl-program '((scan-world ?world))
                      *ontology*
                      :primitive-inventory *duckie-world-primitives*)

(set-data *ontology* 'world nil)

;;;; these are the possible answers:
("10" "9" "8" "7" "6" "5" "4" "3" "2" "1" "0" "ZONE-4" "ZONE-3" "ZONE-2" "ZONE-1" "BAKERY" "HOUSE" "RESTAURANT" "FALSE" "TRUE" "PURPLE" "YELLOW" "GREEN" "BLUE" "RED")
("car-in zone-1" "car-in zone-2" "car-in zone-3" "car-in zone-4")

;(loop for a in (possible-answers)
;        collect (symbol-name a))
          
;  (capi:prompt-for-items-from-list (reverse (possible-answers)) "choose" :interaction :single-selection)


;; addition and deleten examples

(make-instance 'object-set
                       :objects (list
                                 (make-instance 'duckie-building
                                                :zone 'zone-1
                                                :building-function 'house
                                                :color 'purple
                                                :rfid 1)
                                 (make-instance 'duckie-building
                                                :zone 'zone-2
                                                :building-function 'house
                                                :color 'red
                                                :rfid 2)
                                 (make-instance 'duckie-building
                                                :zone 'zone-3
                                                :building-function 'house
                                                :color 'red
                                                :rfid 3)
                                 (make-instance 'duckie-car
                                                :zone 'zone-3
                                                :color 'red
                                                :rfid 4)
                                 (make-instance 'duckie-car
                                                :zone 'zone-6
                                                :color 'purple
                                                :rfid 5)
                                 (make-instance 'duckie-car
                                                :zone 'zone-10
                                                :color 'purple
                                                :rfid 6)
                                 (make-instance 'duckie-car
                                                :zone 'zone-3
                                                :color 'purple
                                                :rfid 7)
                                 ))
