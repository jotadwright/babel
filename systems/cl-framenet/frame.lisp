(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; Representing and processing Frames         ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

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
    :documentation "A list of frame elements.")
   (lexical-units 
    :type list :initarg :lexical-units 
    :accessor lexical-units
    :initform nil 
    :documentation "A list with the names of de lexical units that evoke this frame."))
  (:documentation "The representation of a Framenet frame."))


;; Parsing frames from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-frame-name (xml-frame)
  "Returns the name of the frame as a symbol."
  (make-symbol (string-upcase (xmls:xmlrep-attrib-value "name" xml-frame))))

;; (xml-frame-name (read-frame-from-xml 'opinion))


(defun xml-frame-elements (xml-frame)
  "Returns the frame elements of the frame as a list of frame-element objects."
  (make-list-of-frame-elements xml-frame))

;; (setf *A* (xml-frame-elements (read-frame-from-xml 'opinion)))


(defun xml-lexical-units (xml-frame)
  "Returns the lexical units that the frame evokes."
  (make-list-of-lexical-units xml-frame))

;; (xml-lexical-units (read-frame-from-xml 'opinion))

