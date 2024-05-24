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

