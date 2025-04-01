(in-package :cle)

;; -----------
;; + Concept +
;; -----------

(defclass concept ()
  ((id
    :initarg :id :accessor id :initform (make-id "CONCEPT") :type symbol
    :documentation "Id of the concept."))
  (:documentation "Abstract class for concepts."))

(defclass concept-naming-game (concept)
  ((obj-id
    :initarg :obj-id :accessor obj-id :initform nil :type symbol))
  (:documentation "Concept for the naming game"))
 
(defmethod make-concept ((agent cle-agent) (object cle-object))
  "Creates a concept (based on combinations of distributions) for the given object."
  (make-instance 'concept-naming-game :obj-id (get-topic-id object)))

;; --------------------
;; + helper-functions +
;; --------------------

(defmethod copy-object ((concept concept-naming-game))
  "Returns a copy of the concept."
  (make-instance 'concept-naming-game
                 :id (id concept)
                 :obj-id (obj-id concept)))

(defmethod print-object ((concept concept-naming-game) stream)
  "Prints the concept."
  (pprint-logical-block (stream nil)
    (format stream "<Concept : ~a>" (obj-id concept))
    (format stream ">")))
