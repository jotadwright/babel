(in-package :duckie-language-learning)

;; -------------------
;; + Duckie-building +
;; -------------------

(defclass duckie-building (duckie-object)
  ((building-function
    :initarg :building-function :accessor building-function :initform nil :type symbol
    :documentation "the function of the duckie building"))
  (:documentation "a duckie building in the world"))

(defmethod equal-entity ((obj-1 duckie-building) (obj-2 duckie-building))
  "Objects are equal when their attributes are"
  (and 
       (eql (building-function obj-1) (building-function obj-2))
       (eql (zone obj-1) (zone obj-2))
       (eql (rfid obj-1) (rfid obj-2))
       (eql (color obj-1) (color obj-2))))

(defmethod make-html-for-entity-details ((obj duckie-building) &key)
  "Draw the objects in the web interface"
  `(((div :class "entity-detail")
     ((table)
      ((tr) ((td) "color") ((td) ,(format nil "~(~a~)" (color obj))))
      ((tr) ((td) "building-function") ((td) ,(format nil "~(~a~)" (building-function obj))))
      ((tr) ((td) "zone") ((td) ,(format nil "~(~a~)" (zone obj))))))))
