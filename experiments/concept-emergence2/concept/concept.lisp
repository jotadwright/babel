(in-package :cle)

;; -----------
;; + Concept +
;; -----------

(defclass concept ()
  ((id
    :initarg :id :accessor id :initform (make-id "CONCEPT") :type symbol
    :documentation "Id of the concept."))
  (:documentation "Abstract class for concepts."))

(defclass concept-distribution (concept)
  ((prototypes
    :initarg :prototypes :accessor prototypes :initform nil :type list))
  (:documentation "Concept representation using prototypes."))
 
(defmethod make-concept ((agent cle-agent) (object cle-object) (mode (eql :distribution)))
  (make-instance 'concept-distribution
                 :prototypes (loop for (channel . exemplar) in (attributes object)
                                   for initial-weight = (get-configuration agent :initial-weight)
                                   for distribution = (make-distribution agent
                                                                         exemplar
                                                                         (get-configuration agent :distribution))
                                   for new-prototype = (make-instance 'prototype
                                                                      :channel channel
                                                                      :weight initial-weight
                                                                      :weight-mode (get-configuration agent :weight-update-strategy)
                                                                      :distribution distribution
                                                                      )
                                   collect new-prototype)))

(defmethod copy-object ((concept concept-distribution))
  (make-instance 'concept-distribution
                 :id (id concept)
                 :prototypes (copy-object (prototypes concept))))
