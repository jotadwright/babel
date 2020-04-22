(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                         ;;
;; Classes for representing and methods for manipulating Framenet objects. ;;
;;                                                                         ;;                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;

(defclass framenet-data ()
  ((frames 
    :type hash-table :initarg :frames 
    :accessor frames
    :initform nil 
    :documentation "The frames of Framenet.")
   (lexical-units 
    :type hash-table :initarg :lexical-units 
    :accessor lexical-units
    :initform nil 
    :documentation "The lexical units of Framenet.")
   (example-sentences 
    :type list :initarg :example-sentences 
    :accessor example-sentences
    :initform nil 
    :documentation "The annotated example sentences in Framenet."))
  (:documentation "Holds all the Framenet data in the form of Lisp objects."))


(defclass frame ()
  ((name 
    :type symbol :initarg :name 
    :accessor name
    :initform nil 
    :documentation "The name of the frame.")
   (frame-elements 
    :type list :initarg :frame-elements 
    :accessor frame-elements
    :initform nil 
    :documentation "The frame elements.")
   (lexical-units 
    :type list :initarg :lexical-units 
    :accessor lexical-units
    :initform nil 
    :documentation "The names of de lexical units that evoke this frame."))
  (:documentation "The representation of a Framenet frame."))


(defclass frame-element ()
  ((name 
    :type symbol :initarg :name 
    :accessor name
    :initform nil 
    :documentation "The name of the frame.")
   (abbrev
    :type symbol :initarg :abbrev 
    :accessor abbrev
    :initform nil 
    :documentation "An abbreviation of the FE name as used in the annotations.")
   (core-type 
    :type list :initarg :core-type 
    :accessor core-type
    :initform nil 
    :documentation "Type if frame-element."))
  (:documentation "The representation of a Framenet frame-element."))
