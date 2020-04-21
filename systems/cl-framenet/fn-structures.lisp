(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Classes for representing and methods for manipulating Framenet objects. ;;
;;                                                                         ;;                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;

(defclass framenet-data ()
  ((frames 
    :type list :initarg :frames 
    :accessor frames
    :initform nil 
    :documentation "The frames of Framenet.")
   (lexical-units 
    :type list :initarg :lexical-units 
    :accessor lexical-units
    :initform nil 
    :documentation "The lexical units of Framenet.")
   (example-sentences 
    :type list :initarg :example-sentences 
    :accessor example-sentences
    :initform nil 
    :documentation "The annotated example sentences in Framenet."))
  (:documentation "Holds all the Framenet data in the form of Lisp objects."))


