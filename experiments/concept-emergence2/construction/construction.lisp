(in-package :cle)

;; ----------------
;; + Construction +
;; ----------------
(defclass cxn ()
  ((id
    :initarg :id :accessor id :initform (make-id "CXN") :type symbol
    :documentation "Id of the construction.")
   (form
    :initarg :form :accessor form :initform nil :type string
    :documentation "Form of the construction.")
   (meaning
    :initarg :meaning :accessor meaning :initform nil :type concept
    :documentation "Meaning of the construction.")
   (score
    :initarg :score :accessor score :initform nil :type number))
  (:documentation "A construction is a mapping between a form and a meaning."))

(defgeneric make-cxn (agent object form)
  (:documentation "Creates a new construction."))
  
(defmethod make-cxn (agent object form)
  (make-instance 'cxn
                 :form form
                 :meaning (make-concept agent object (get-configuration agent :concept-representation))
                 :score (get-configuration agent :initial-cxn-entrenchement)))

(defmethod copy-object ((cxn cxn))
  (make-instance 'cxn
                 :id (id cxn)
                 :form (copy-object (form cxn))
                 :meaning (copy-object (meaning cxn))
                 :score (copy-object (score cxn))))

(defmethod print-object ((cxn cxn) stream)
  (pprint-logical-block (stream nil)
    (format stream "<CXN:~
                        ~:_ form: ~a,~:_ score: ~a~:_"
            (form cxn) (score cxn))
    (format stream ">")))
