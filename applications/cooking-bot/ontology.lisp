(in-package :cooking-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; This file contains the ontology for the cooking-bot environment. ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *kitchen* (make-instance 'kitchen))

;; General ;;
;;;;;;;;;;;;;

(defclass quantity ()
  ((amount :type number :accessor amount :initarg :amount :initform 0)
   (unit :type symbol :accessor unit :initarg :unit :initform nil)))

(defclass physical-entity ()
  ((quantity :type quantity :accessor quantity :initarg :quantity
             :initform (make-instance 'quantity :amount 1 :unit (make-instance 'piece)))))

(defclass countable-object (physical-entity)
  ())

(defclass uncountable-object (physical-entity)
  ())

(defclass unit ()
  ())

(defclass ml (unit)
  ())

(defclass g (unit)
  ())

(defclass piece (unit)
  ())


;; Kitchen ;;
;;;;;;;;;;;;;

(defclass kitchen (physical-entity)
  ((fridge :type fridge :accessor fridge :initarg :fridge :initform (make-instance 'fridge))
   (pantry :type pantry :accessor pantry :initarg :pantry :initform (make-instance 'pantry))
   (countertop :type countertop :accessor countertop :initarg :countertop :initform (make-instance 'countertop))))

(defclass storage-place (physical-entity)
  ((contents :type list
             :initarg :contents
             :initform '()
             :accessor contents)))

(defclass fridge (storage-place)
  ())

(defclass pantry (storage-place)
  ())

(defclass countertop (storage-place)
  ())

(defclass ingredient (physical-entity)
  ())


;; Ingredients ;;
;;;;;;;;;;;;;;;;;

(defclass milk (ingredient uncountable-object)
  ())

(defclass butter (ingredient uncountable-object)
  ())

(defclass flour (ingredient uncountable-object)
  ())

(defclass sugar (ingredient uncountable-object)
  ())

(defclass egg (ingredient countable-object)
  ())

