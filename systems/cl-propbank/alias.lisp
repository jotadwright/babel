(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;; Representing and processing Propbank aliases     ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass alias ()
  ((name
    :type symbol :initarg :name 
    :accessor name
    :initform nil 
    :documentation "The name of the alias.")
   (pos
    :type symbol :initarg :pos 
    :accessor pos
    :initform nil 
    :documentation "The part of speech (v,n,j or l) of the alias.")
   (framenet
    :type list :initarg :framenet 
    :accessor framenet
    :initform nil 
    :documentation "The framenet references for this alias.")
   (verbnet
    :type symbol :initarg :verbnet 
    :accessor verbnet
    :initform nil 
    :documentation "The verbnet references for this alias."))
  (:documentation "The representation of a PropBank alias."))

(defmethod print-object ((alias alias) (stream t))
  (format stream "<alias: ~(~a~) (~(~a~))>" (name alias) (pos alias)))

;; Parsing predicates from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-aliases (aliases-xml)
  "Returns an alias object for each alias found."
  (loop for child in (xmls:xmlrep-children aliases-xml)
        if (xmls:xmlrep-tagmatch "alias" child)
        collect (xml-alias child)))

;; (setf *A* (mapcar #'aliases (rolesets (first (load-pb-file 'feel)))))

(defun xml-alias (alias-xml)
  "Returns an alias object."
  (make-instance 'alias
                 :name (xml-alias-name alias-xml)
                 :pos (xml-alias-pos alias-xml)
                 :framenet (xml-alias-framenet alias-xml)
                 :verbnet (xml-alias-verbnet alias-xml)))


(defun xml-alias-name (alias-xml)
  "Returns the name of an alias as a symbol."
  (propbank-string->symbol (xmls:xmlrep-string-child alias-xml)))


(defun xml-alias-pos (alias-xml)
  "Returns the pos of an alias as a symbol."
  (propbank-string->symbol (xmls:xmlrep-attrib-value "pos" alias-xml)))


(defun xml-alias-framenet (alias-xml)
  "Returns the framenet ref of an alias."
  (let ((frames (cl-ppcre::split " " (xmls:xmlrep-attrib-value "framenet" alias-xml))))
    (mapcar #'propbank-string->symbol frames)))
           
(defun xml-alias-verbnet (alias-xml)
  "Returns the verbnet ref of an alias."
  (let ((refs (cl-ppcre::split ", " (xmls:xmlrep-attrib-value "verbnet" alias-xml))))
    (mapcar #'propbank-string->symbol refs)))
