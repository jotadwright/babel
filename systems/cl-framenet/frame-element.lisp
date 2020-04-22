(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; Representing and processing Frame Elements ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass frame-element ()
  ((name 
    :type symbol :initarg :name 
    :accessor name
    :initform nil 
    :documentation "The name of the frame element.")
   (abbrev
    :type symbol :initarg :abbrev 
    :accessor abbrev
    :initform nil 
    :documentation "An abbreviation of the frame element name as used in the annotations.")
   (core-type 
    :type list :initarg :core-type 
    :accessor core-type
    :initform nil 
    :documentation "Type of frame-element."))
  (:documentation "The representation of a Framenet frame-element."))


;; Frame Elements ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-list-of-frame-elements (xml-frame)
  "Returns a list of frame-element objects for the given frame."
  (loop for FE in (xmls:xmlrep-find-child-tags "FE" xml-frame)
        collect (make-instance 'frame-element
                               :name (xml-frame-element-name FE)
                               :abbrev (xml-frame-element-abbrev FE)
                               :core-type (xml-frame-element-core-type FE))))

;; (setf *A* (make-frame-elements (read-frame-from-xml 'opinion)))


(defun xml-frame-element-name (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "name" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))

;; (xml-frame-element-name (fourth (xml-frame-elements (read-frame-from-xml 'opinion))))


(defun xml-frame-element-abbrev (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "abbrev" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))


(defun xml-frame-element-core-type (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string  (xmls:xmlrep-attrib-value "coreType" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (make-symbol (string-upcase value-as-string)))))

;; (xml-frame-element-core-type (fourth (xml-frame-elements (read-frame-from-xml 'opinion))))

