(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Representing for manipulating Propbank data.                            ;;
;;                                                                         ;;                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;

(defclass propbank-data ()
  ((predicates 
    :type hash-table :initarg :frames 
    :accessor frames
    :initform nil 
    :documentation "The predicates of PropBank."))
  (:documentation "Holds all the PropBank predicates in the form of Lisp objects."))

