(in-package :duckie-language-learning)

;; --------------
;; + Duckie-home +
;; --------------

(defclass duckie-home (duckie-object)
  ((building
    :initarg :building :accessor building :initform nil :type duckie-building
    :documentation "the building of the duckie home"))
  (:documentation "a duckie home in the world"))

(defmethod equal-entity ((obj-1 duckie-home) (obj-2 duckie-home))
  "Objects are equal when their attributes are"
  (and 
       (equal-entity (building obj-1) (building obj-2))
       (eql (rfid obj-1) (rfid obj-2))
       (eql (zone obj-1) (zone obj-2))
       (eql (color obj-1) (color obj-2))))

(defmethod make-html-for-entity-details ((obj duckie-home) &key)
  "Draw the objects in the web interface"
  (when (rfid obj)
  `(((div :class "entity-detail")
     ((table)
      ((tr) ((td) "building:") ((td) ,(format nil "~(~a~)" (id (building obj)))))
      ((tr) ((td) "rfid:") ((td) ,(format nil "~(~a~)" (rfid obj))))
      ((tr) ((td) "color") ((td) ,(format nil "~(~a~)" (color obj)))))))))
