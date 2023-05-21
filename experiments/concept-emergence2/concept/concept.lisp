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
 
(defmethod make-concept (agent object (mode (eql :distribution)))
  (make-instance 'concept-distribution
                 :prototypes (loop for (channel . exemplar) in object
                                   for initial-weight = (get-configuration agent :initial-weight)
                                   for distribution = (make-distribution agent
                                                                         exemplar
                                                                         (get-configuration agent :distribution))
                                   for new-prototype = (make-instance 'prototype
                                                                      :channel channel
                                                                      :weight initial-weight
                                                                      :distribution distribution)
                                   collect new-prototype)))
