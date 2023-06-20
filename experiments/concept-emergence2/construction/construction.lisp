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
    :initarg :score :accessor score :initform nil :type number)
   (history
    :initarg :history :accessor history :initform '() :type list))
  (:documentation "A construction is a mapping between a form and a meaning."))
  
(defmethod make-cxn (agent object form)
  "Creates a new construction."
  (let ((scene-idx (index (current-scene (world (experiment agent)))))
        (interaction-number (interaction-number (current-interaction (experiment agent)))))
    (make-instance 'cxn
                   :form form
                   :meaning (make-concept agent object (get-configuration agent :concept-representation))
                   :score (get-configuration agent :initial-cxn-entrenchement)
                   :history (list (cons interaction-number scene-idx)))))

(defmethod copy-object ((cxn cxn))
  (make-instance 'cxn
                 :id (id cxn)
                 :form (copy-object (form cxn))
                 :meaning (copy-object (meaning cxn))
                 :score (copy-object (score cxn))
                 :history (copy-object (history cxn))))

(defmethod print-object ((cxn cxn) stream)
  (pprint-logical-block (stream nil)
    (format stream "<CXN:~
                        ~:_ form: ~a,~:_ score: ~a~:_"
            (form cxn) (score cxn))
    (format stream ">")))
