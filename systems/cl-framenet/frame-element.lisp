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
    :type symbol :initarg :core-type 
    :accessor core-type
    :initform nil 
    :documentation "Type of frame-element.")
   (sem-types
    :type list :initarg :sem-types 
    :accessor sem-types
    :initform nil 
    :documentation "Possible sem-types of frame-element.")
   (requires-frame-elements
    :type list :initarg :requires-frame-elements 
    :accessor requires-frame-elements
    :initform nil 
    :documentation "Required frame elements.")
   (excludes-frame-elements
    :type list :initarg :excludes-frame-elements 
    :accessor excludes-frame-elements
    :initform nil 
    :documentation "Excluded frame elements."))
  (:documentation "The representation of a Framenet frame-element."))


;; Frame Elements ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-list-of-frame-elements (xml-frame)
  "Returns a list of frame-element objects for the given frame."
  (loop for FE in (xmls:xmlrep-find-child-tags "FE" xml-frame)
        collect (make-instance 'frame-element
                               :name (xml-frame-element-name FE)
                               :abbrev (xml-frame-element-abbrev FE)
                               :core-type (xml-frame-element-core-type FE)
                               :sem-types (xml-frame-element-sem-types FE)
                               :requires-frame-elements (xml-frame-element-requires FE)
                               :excludes-frame-elements (xml-frame-element-excludes FE))))

;; (make-list-of-frame-elements  (read-frame-from-xml 'opinion))


(defun xml-frame-element-name (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "name" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (framenet-string->symbol value-as-string))))

;; (mapcar #'xml-frame-element-name (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'opinion)))


(defun xml-frame-element-abbrev (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string (xmls:xmlrep-attrib-value "abbrev" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (framenet-string->symbol value-as-string))))

;; (mapcar #'xml-frame-element-abbrev (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'opinion)))


(defun xml-frame-element-core-type (xml-frame-element)
  "Returns the name of the frame-element as a symbol."
  (let ((value-as-string  (xmls:xmlrep-attrib-value "coreType" xml-frame-element)))
    (unless (equalp "" value-as-string)
      (framenet-string->symbol value-as-string))))

;; (mapcar #'xml-frame-element-core-type (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'opinion)))


(defun xml-frame-element-sem-types (xml-frame-element)
  "Returns a possibly empty list of SemTypes."
  (let ((sem-types-xml  (xmls:xmlrep-find-child-tags "semType" xml-frame-element)))
    (loop for el in sem-types-xml
          collect (framenet-string->symbol (xmls:xmlrep-attrib-value "name" el)))))

;; (mapcar #'xml-frame-element-sem-types (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'opinion)))


(defun xml-frame-element-requires (xml-frame-element)
  "Returns a possibly empty list of SemTypes."
  (let ((requires-xml  (xmls:xmlrep-find-child-tags "requiresFE" xml-frame-element)))
    (loop for el in requires-xml
          collect (framenet-string->symbol (xmls:xmlrep-attrib-value "name" el)))))

;; (mapcar #'xml-frame-element-requires  (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'apply_heat)))


(defun xml-frame-element-excludes (xml-frame-element)
  "Returns a possibly empty list of SemTypes."
  (let ((excludes-xml  (xmls:xmlrep-find-child-tags "excludesFE" xml-frame-element)))
    (loop for el in excludes-xml
          collect (framenet-string->symbol (xmls:xmlrep-attrib-value "name" el)))))

;; (mapcar #'xml-frame-element-excludes  (xmls:xmlrep-find-child-tags "FE" (read-frame-from-xml 'age)))

