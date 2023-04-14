(in-package :duckie-language-learning)

(defun build-initial-ontology ()
  (let ((colors '(red blue green yellow purple))
        (building-functions '(restaurant house bakery))
        (bools '(true false))
        (object-types '(duckie-car duckie-building duckie-agent-car duckie-home))
        (rfids '(a b c d e f))
        (operators '(bigger smaller equal))
        (attributes '(color building-function rfid))
        (zones '(1 2 3 4))
        (initial-ontology (make-blackboard)))
    (loop for color in colors
          for instance = (make-instance 'color-category
                                        :id color :category color)
          do (push-data initial-ontology 'colors instance))
    (loop for object-type in object-types
          for instance = (make-instance 'object-type-category
                                        :id object-type :category object-type)
          do (push-data initial-ontology 'object-types instance))
    (loop for bool in bools
          for instance = (make-instance 'boolean-category
                                        :id bool :category bool)
          do (push-data initial-ontology 'bools instance))
    (loop for building-function in building-functions
          for instance = (make-instance 'building-function-category
                                        :id building-function :category building-function)
          do (push-data initial-ontology 'building-functions instance))
    (loop for rfid in rfids
          for instance = (make-instance 'rfid-category
                                        :id rfid :category rfid)
          do (push-data initial-ontology 'rfids instance))
    (loop for op in operators
          for instance = (make-instance 'compare-category
                                        :id op :category op)
          do (push-data initial-ontology 'operators instance))
    (loop for attr in attributes
          for instance = (make-instance 'attribute-category
                                        :id attr :category attr)
          do (push-data initial-ontology 'attributes instance))
    (loop for zone in zones
          for instance = (make-instance 'zone-category
                                        :id zone :category zone)
          do (push-data initial-ontology 'zones instance))
    (set-data initial-ontology 'world nil)
   ; (push-data initial-ontology 'coordinates (make-instance 'coordinates-category :x-coordinate 0 :y-coordinate 0))
   ; (push-data initial-ontology 'coordinates (make-instance 'coordinates-category :x-coordinate 1 :y-coordinate 0))
   ; (push-data initial-ontology 'coordinates (make-instance 'coordinates-category :x-coordinate 1 :y-coordinate 1))
   ; (push-data initial-ontology 'coordinates (make-instance 'coordinates-category :x-coordinate 1 :y-coordinate 2))
    
    initial-ontology))