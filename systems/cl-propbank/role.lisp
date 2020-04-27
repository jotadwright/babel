(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;; Representing and processing Propbank roles       ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass role ()
  ((argument-number
    :type (or number symbol) :initarg :argument-number 
    :accessor argument-number
    :initform nil 
    :documentation "The number of the role, or M.")
   (argument-function
    :type symbol :initarg :argument-function 
    :accessor argument-function
    :initform nil 
    :documentation "The function of the role.")
   (description
    :type symbol :initarg :description 
    :accessor description
    :initform nil 
    :documentation "The description of the role as a string.")
   (framenet-roles
    :type list :initarg :framenet-roles
    :accessor framenet-roles
    :initform nil 
    :documentation "The framenet roles for this role.")
   (verbnet-roles
    :type symbol :initarg :verbnet-roles
    :accessor verbnet-roles
    :initform nil 
    :documentation "The verbnet roles for this role."))
  (:documentation "The representation of a PropBank role."))

(defmethod print-object ((role role) (stream t))
  (format stream "<role: Arg~a-~(~a~)>" (argument-number role) (argument-function role)))

;; Parsing predicates from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-roles (roles-xml)
  "Returns an role object for each role found."
  (loop for child in (xmls:xmlrep-children roles-xml)
        if (xmls:xmlrep-tagmatch "role" child)
        collect (xml-role child)))


(defun xml-role (role-xml)
  "Returns an role object."
  (make-instance 'role
                 :argument-number (xml-role-number role-xml)
                 :argument-function (xml-role-function role-xml)
                 :description (xml-role-description role-xml)
                 :framenet-roles (xml-role-framenet role-xml)
                 :verbnet-roles (xml-role-verbnet role-xml)))


(defun xml-role-number (role-xml)
  "Returns the number of a role as a symbol."
  (propbank-string->symbol (xmls:xmlrep-attrib-value "n" role-xml)))


(defun xml-role-function (role-xml)
  "Returns the function of a role as a symbol."
  (propbank-string->symbol (xmls:xmlrep-attrib-value "f" role-xml)))


(defun xml-role-description (role-xml)
  "Returns the function of a role as a symbol."
  (xmls:xmlrep-attrib-value "descr" role-xml))


(defun xml-role-framenet (role-xml)
  "Returns the framenet ref of an role."
  (loop for child in (xmls:xmlrep-children role-xml)
        if (xmls:xmlrep-tagmatch "fnrole" child)
        collect (list (cons :frame (propbank-string->symbol (xmls:xmlrep-attrib-value "frame" child)))
                      (cons :fe (propbank-string->symbol (xmls:xmlrep-attrib-value "fe" child))))))

           
(defun xml-role-verbnet (role-xml)
  "Returns the verbnet ref of an role."
  (loop for child in (xmls:xmlrep-children role-xml)
        if (xmls:xmlrep-tagmatch "vnrole" child)
        collect (list (cons :vncls (propbank-string->symbol (xmls:xmlrep-attrib-value "vncls" child)))
                      (cons :vntheta (propbank-string->symbol (xmls:xmlrep-attrib-value "vntheta" child))))))
