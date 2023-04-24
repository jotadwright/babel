(in-package :duckie-language-learning)

;; objects in the world
(defclass duckie-object (entity)
  ((zone
    :initarg :zone :accessor zone :initform nil :type cons
    :documentation "the zone of the duckie object")
   (color
    :initarg :color :accessor color :initform nil :type symbol
    :documentation "the color of the duckie object")
   (rfid
    :initarg :rfid :accessor rfid :initform nil :type symbol
    :documentation "the rfid of the duckie object") )
  (:documentation "a duckie object in the world"))

(defmethod equal-entity ((obj-1 duckie-object) (obj-2 duckie-object))
  "Objects are equal when their attributes are"
  (and 
       (eql (rfid obj-1) (rfid obj-2))
       (eql (zone obj-1) (zone obj-2))
       (eql (color obj-1) (color obj-2))))

(defmethod make-html-for-entity-details ((obj duckie-object) &key)
  "Draw the objects in the web interface"
  `(((div :class "entity-detail")
     ((table)
      ((tr) ((td) "zone:") ((td) ,(format nil "~(~a~)" (zone obj))))
      ((tr) ((td) "color") ((td) ,(format nil "~(~a~)" (color obj))))
      ))))