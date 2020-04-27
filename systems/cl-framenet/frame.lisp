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
   (sem-types
    :type list :initarg :sem-types
    :accessor sem-types
    :initform nil 
    :documentation "The possible empty list of semantic types of the frame.")
   (frame-elements 
    :type list :initarg :frame-elements 
    :accessor frame-elements
    :initform nil 
    :documentation "A list of frame elements.")
   (frame-elements-core-sets
    :type list :initarg :frame-element-core-sets 
    :accessor frame-element-core-sets
    :initform nil 
    :documentation "A list of core sets of frame elements.")
   (frame-relations
    :type list :initarg :frame-relations 
    :accessor frame-relations
    :initform nil 
    :documentation "A list of frame relations.")
   (lexical-units 
    :type list :initarg :lexical-units 
    :accessor lexical-units
    :initform nil 
    :documentation "A list with the names of de lexical units that evoke this frame."))
  (:documentation "The representation of a Framenet frame."))

(defmethod print-object ((frame frame) (stream t))
  (format stream "<frame: ~(~a~)>" (name frame)))


;; Parsing frames from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-frame-object (xml-frame)
  "Returns a frame object based on an xml definition."
  (make-instance 'frame
                 :name (xml-frame-name xml-frame)
                 :sem-types (xml-frame-sem-types xml-frame)
                 :frame-elements (xml-frame-elements xml-frame)
                 :frame-element-core-sets (xml-frame-element-core-sets xml-frame)
                 :frame-relations (xml-frame-relations xml-frame)))

;; (xml-frame-object (read-frame-from-xml 'transitive_action))


(defun xml-frame-name (xml-frame)
  "Returns the name of the frame as a symbol."
  (framenet-string->symbol (xmls:xmlrep-attrib-value "name" xml-frame)))

;; (xml-frame-name (read-frame-from-xml 'operate_vehicle))


(defun xml-frame-sem-types (xml-frame)
  "Returns the sementic types of the frame as a list of symbols."
  (let ((sem-types-xml (xmls:xmlrep-find-child-tags "semType" xml-frame)))
    (loop for el in sem-types-xml
          collect (framenet-string->symbol (xmls:xmlrep-attrib-value "name" el)))))

;; (xml-frame-element-sem-types (read-frame-from-xml 'transitive_action))


(defun xml-frame-element-core-sets (xml-frame)
  (loop for core-set in (xmls:xmlrep-find-child-tags "FEcoreSet" xml-frame)
      collect (mapcar (lambda (member-fe)
                        (framenet-string->symbol (xmls:xmlrep-attrib-value "name" member-fe)))
                      (xmls:xmlrep-find-child-tags "memberFE" core-set))))

;; (xml-frame-element-core-sets (read-frame-from-xml 'manipulation))


(defun xml-frame-relations (xml-frame)
  "Returns the frame relations."
  (loop for frame-relation in (xmls:xmlrep-find-child-tags "frameRelation" xml-frame)
        for frame-relation-type = (framenet-string->symbol (xmls:xmlrep-attrib-value "type" frame-relation))
        for frame-relation-elements = (xmls:xmlrep-children frame-relation)
        collect (cons frame-relation-type
                      (loop for el in frame-relation-elements
                            collect (framenet-string->symbol (xmls:xmlrep-string-child el))))))

;; (xml-frame-relations (read-frame-from-xml 'opinion))

(defun xml-lexical-units (xml-frame)
  "Returns the lexical units that the frame evokes."
  (make-list-of-lexical-units xml-frame))

;; (xml-lexical-units (read-frame-from-xml 'opinion))

