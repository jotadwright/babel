(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                  ;;
;; Representing and processing Propbank predicates. ;;
;;                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Class Definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass predicate ()
  ((lemma 
    :type symbol :initarg :lemma 
    :accessor lemma
    :initform nil 
    :documentation "The lemma of the predicate")
   (rolesets 
    :type list :initarg :rolesets 
    :accessor rolesets
    :initform nil 
    :documentation "A list of rolesets associated to the predicate."))
  (:documentation "The representation of a PropBank predicate."))

(defmethod print-object ((predicate predicate) (stream t))
  (format stream "<predicate: ~(~a~)>" (lemma predicate)))


;; Parsing predicates from XML ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xml-predicate (predicate-xml)
  "Returns a frame object based on an xml definition."
  (make-instance 'predicate
                 :lemma (xml-predicate-lemma predicate-xml)
                 :rolesets (xml-predicate-rolesets predicate-xml)))

;; (load-pb-file 'feel)


(defun xml-predicate-lemma (predicate-xml)
  "Returns the lemma of the predicate as a symbol."
  (propbank-string->symbol (xmls:xmlrep-attrib-value "lemma" predicate-xml)))

;; (mapcar #'lemma (load-pb-file 'feel))


(defun xml-predicate-rolesets (predicate-xml)
  "Returns the rolesets of the predicate as a list."
  (loop for roleset-xml in (xmls:xmlrep-find-child-tags "roleset" predicate-xml)
        collect (xml-roleset roleset-xml)))

;; (mapcar #'rolesets (load-pb-file 'feel))
