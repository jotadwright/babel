(in-package :duckie-language-learning)

;; --------------
;; + Duckie-car +
;; --------------

(defclass duckie-car (duckie-object)
  ( )
  (:documentation "a duckie car in the world"))

(defclass duckie-agent-car (duckie-car)
  ( )
  (:documentation "a duckie agent car in the world"))

(defmethod equal-entity ((obj-1 duckie-car) (obj-2 duckie-car))
  "Objects are equal when their attributes are"
  (and 
       (eql (color obj-1) (color obj-2))
       (eql (zone obj-1) (zone obj-2))
       (eql (rfid obj-1) (rfid obj-2))))

(defmethod make-html-for-entity-details ((obj duckie-car) &key)
  "Draw the objects in the web interface"
  `(((div :class "entity-detail")
     ((table)
      ((tr) ((td) "color") ((td) ,(format nil "~(~a~)" (color obj))))
      ((tr) ((td) "zone") ((td) ,(format nil "~(~a~)" (zone obj))))))))
