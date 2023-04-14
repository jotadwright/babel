(in-package :duckie-language-learning)

;;categories in the world

;; General category
(defclass duckie-category (entity)
  ((category
    :initarg :category :accessor category :initform nil :type symbol
    :documentation "the category")))

(defmethod make-html-for-entity-details ((cat duckie-category) &key)
  `(((div :class "entity-detail")
     ,(format nil "~(~a~)" (category cat)))))

(defmethod equal-entity ((cat-1 duckie-category) (cat-2 duckie-category))
  (eql (category cat-1) (category cat-2)))

;; Subcategories
(defclass color-category (duckie-category) ())

(defclass object-type-category (duckie-category) ())

(defclass building-function-category (duckie-category) ())

(defclass boolean-category (duckie-category) ())

(defclass rfid-category (duckie-category) ())

(defclass compare-category (duckie-category) ())

(defclass attribute-category (duckie-category) ())



(defclass zone-category (duckie-category)
  ((zone
    :initarg :zone :accessor zone :initform nil :type number
    :documentation "the zone")))