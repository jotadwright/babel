(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Class definitions for the fcg-learn package ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass speech-act (blackboard)
  ((form
    :type string
    :accessor form
    :initform nil
    :initarg :form)
   (meaning
    :type list
    :accessor meaning
    :initform nil
    :initarg :meaning)
   (situation
    :accessor situation
    :initform nil
    :initarg :situation))
  (:documentation "Data type for linguistic observations."))

(defmethod copy-object-content ((original speech-act) (copy speech-act))
  (setf (form copy) (form original))
  (setf (meaning copy) (meaning original))
  (setf (situation copy) (situation original)))


(defclass holophrastic-cxn (fcg-construction)
  ())

(defclass filler-cxn (fcg-construction)
  ())

(defclass linking-cxn (fcg-construction)
  ())