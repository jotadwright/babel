(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;; Representing and processing Propbank rolesets.   ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass roleset ()
  ((id 
    :type symbol :initarg :id 
    :accessor id
    :initform nil 
    :documentation "The id of the roleset, eg. find.01.")
   (name 
    :type list :initarg :name 
    :accessor name
    :initform nil 
    :documentation "The name of the roleset, i.e. as string describing the word sense.")
   (aliases 
    :type list :initarg :aliases 
    :accessor aliases
    :initform nil 
    :documentation "Relations to FrameNet and VerbNet.")
   (roles 
    :type list :initarg :roles 
    :accessor roles
    :initform nil 
    :documentation "The participant roles.")
   (examples 
    :type list :initarg :examples 
    :accessor examples
    :initform nil 
    :documentation "A list of annotated examples."))
  (:documentation "The representation of a PropBank roleset."))

(defmethod print-object ((roleset roleset) (stream t))
  (format stream "<roleset: ~(~a~)>" (id roleset)))


;; Parsing predicates from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-roleset (roleset-xml)
  "Returns a roleset object based on an xml definition."
  (make-instance 'roleset
                 :id (xml-roleset-id roleset-xml)
                 :name (xml-roleset-name roleset-xml)
                 :aliases (xml-roleset-aliases roleset-xml)
                 :roles (xml-roleset-roles roleset-xml)))

;; (mapcar #'rolesets  (load-pb-file 'feel))


(defun xml-roleset-id (roleset-xml)
  "Returns the id of a roleset as a symbol."
  (propbank-string->symbol (xmls:xmlrep-attrib-value "id" roleset-xml)))

;; (mapcar #'id (rolesets (first (load-pb-file 'feel))))


(defun xml-roleset-name (roleset-xml)
  "Returns the name of a roleset as a string."
  (xmls:xmlrep-attrib-value "name" roleset-xml))

;; (mapcar #'name (rolesets (first (load-pb-file 'feel))))


(defun xml-roleset-aliases (roleset-xml)
  "Returns the aliases of a roleset as list."
  (loop for child in (xmls:xmlrep-children roleset-xml)
        if (xmls:xmlrep-tagmatch "aliases" child)
        return (xml-aliases child)))

;; (mapcar #'aliases (rolesets (first (load-pb-file 'feel))))


(defun xml-roleset-roles (roleset-xml)
  "Returns the aliases of a roleset as list."
  (loop for child in (xmls:xmlrep-children roleset-xml)
        if (xmls:xmlrep-tagmatch "roles" child)
        return (xml-roles child)))

;; (mapcar #'roles (rolesets (first (load-pb-file 'feel))))

