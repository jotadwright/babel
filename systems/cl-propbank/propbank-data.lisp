(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Representing for manipulating Propbank data.                            ;;
;;                                                                         ;;                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;

(defclass propbank-data ()
  ((predicates 
    :type list :initarg :predicates 
    :accessor predicates
    :initform nil 
    :documentation "The predicates of PropBank."))
  (:documentation "Holds all the PropBank predicates in the form of a list of Lisp objects."))

