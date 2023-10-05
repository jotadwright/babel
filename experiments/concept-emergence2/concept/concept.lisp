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
    :initarg :prototypes :accessor prototypes :initform nil :type hash-table))
  (:documentation "Concept representation using prototypes."))
 
(defmethod make-concept ((agent cle-agent) (object cle-object) (mode (eql :distribution)))
  "Creates a concept (based on combinations of distributions) for the given object."
  (let* ((prototypes (loop for channel being the hash-keys of (attributes object)
                             using (hash-value observation)
                          ;; create a distribution for each channel
                           for distribution = (make-distribution agent
                                                                 observation
                                                                 (get-configuration (experiment agent) :distribution))
                           ;; create a prototype for each channel
                           for new-prototype = (make-instance 'prototype
                                                              :channel channel
                                                              :weight (get-configuration (experiment agent) :initial-weight)
                                                              :weight-mode (get-configuration (experiment agent) :weight-update-strategy)
                                                              :distribution distribution)
                           ;; only create prototypes for enabled sensors
                           if (and (not (gethash channel (disabled-channels agent)))
                                   observation)
                             collect new-prototype)))
    ;; create the concept
    (make-instance 'concept-distribution :prototypes (list-to-hash-table prototypes :key #'channel))))

;; -----------------------------------
;; + get/switch channel availability +
;; -----------------------------------

(defmethod get-prototypes ((concept concept))
  "Returns a list of all prototypes of the concept."
  (loop for prototype being the hash-values of (prototypes concept)
        collect prototype))

(defmethod get-available-prototypes ((agent cle-agent) (concept concept))
  "Returns a list of all prototypes of the concept that are not disabled."
  (loop with disabled-channels = (disabled-channels agent)
        for prototype in (get-prototypes concept)
        if (not (gethash (channel prototype) disabled-channels))
          collect prototype))

(defmethod switch-channel-availability ((agent cle-agent) (channel symbol))
  "Switches the availability of the given channel."
  (if (gethash channel (disabled-channels agent))
    (remhash channel (disabled-channels agent))
    (setf (gethash channel (disabled-channels agent)) channel)))

;; --------------------
;; + helper-functions +
;; --------------------

(defmethod copy-object ((concept concept-distribution))
  "Returns a copy of the concept."
  (make-instance 'concept-distribution
                 :id (id concept)
                 :prototypes (copy-object (prototypes concept))))

(defmethod print-object ((concept concept-distribution) stream)
  "Prints the concept."
  (pprint-logical-block (stream nil)
    (format stream "<Concept - channels: ~a,~:_"
            (loop for prototype in (get-prototypes concept) collect (channel prototype)))
    (format stream ">")))
