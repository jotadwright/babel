(in-package :cle)

;; ---------
;; + Agent +
;; ---------

(defclass cle-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon."
    :type list :accessor lexicon :initform nil)
   (trash
    :documentation "The lexicon trash, contains cxns with an entrenchment score of zero."
    :type list :accessor trash :initform nil)
   (disabled-channels
    :documentation "Disabled/defected channels."
    :type hash-table :accessor disabled-channels :initarg :disabled-channels :initform nil)
   (noise-in-each-sensor
    :documentation "Fixed noise on each channel"
    :type list :accessor noise-in-each-sensor :initarg :noise-in-each-sensor :initform nil)
   (noise-in-each-observation
    :documentation "Noise on each channel at every observation"
    :type list :accessor noise-in-each-observation :initarg :noise-in-each-observation :initform nil)
   (invented-or-adopted
    :documentation "Whether the agent invented or adopted during the interaction."
    :type boolean :accessor invented-or-adopted :initform nil)
   (usage-table
    :documentation "Keeps track of the cxns used with a sliding window."
    :type usage-table :accessor usage-table :initarg :usage-table)
    ))
  
(defmethod clear-agent ((agent cle-agent))
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (invented-or-adopted agent) nil
        (communicated-successfully agent) nil))

(defmethod find-in-lexicon ((agent cle-agent) (form string))
  "Finds constructions with the given form in the lexicon of the given agent."
  (let ((result (find form (lexicon agent) :key #'form :test #'string=)))
    (if result
      result
      (find form (trash agent) :key #'form :test #'string=))))

;; ---------
;; + NOISE +
;; ---------

(defmethod perceive-object-val2 ((agent cle-agent) (object cle-object) attr)
  "Perceives the value in a given sensor 'attr' of a given object.

   This reading can be affected by two types of noise.
   The raw observation is the true value in that channel of the object.
   The sensor-noise term is a fixed shift (in either direction).
   The observation noise term is different for every observation."
  (let ((raw-observation-val (get-object-val object attr))
        (sensor-noise (noise-in-sensor agent attr (get-configuration (experiment agent) :sensor-noise)))
        (observation-noise (noise-in-observation agent attr (get-configuration (experiment agent) :observation-noise))))
    (if raw-observation-val
      (min (max 0 (+ raw-observation-val sensor-noise observation-noise)) 1)
      nil)))

(defmethod perceive-object-val ((agent cle-agent) (object cle-object) attr)
  "Perceives the value in a given sensor 'attr' of a given object.

   This reading can be affected by two types of noise.
   The raw observation is the true value in that channel of the object.
   The sensor-noise term is a fixed shift (in either direction).
   The observation noise term is different for every observation."
  (get-object-val object attr))

;; -------------------
;; + noise-in-sensor +
;; -------------------

(defmethod determine-noise-in-sensor (experiment disabled-channels (mode (eql :none)))
  "Sets the fixed shift for each sensor to zero."
  (loop with remaining-channels = (set-difference (get-configuration experiment :available-channels) disabled-channels)
        for channel in remaining-channels
        collect (cons channel 0)))

(defmethod determine-noise-in-sensor (experiment disabled-channels (mode (eql :shift)))
  "Determines a fixed shift for each sensor."
  (loop with remaining-channels = (set-difference (get-configuration experiment :available-channels) disabled-channels)
        for channel in remaining-channels
        for shift = (random-gaussian 0 (get-configuration experiment :sensor-std))
        collect (cons channel shift)))

(defmethod noise-in-sensor ((agent cle-agent) (attr symbol) (mode (eql :none)))
  0)

(defmethod noise-in-sensor ((agent cle-agent) (attr symbol) (mode (eql :shift)))
  (assqv attr (noise-in-each-sensor agent)))

;; ------------------------
;; + noise-in-observation +
;; ------------------------

(defmethod determine-noise-in-observation (experiment disabled-channels (mode (eql :none)))
  "Sets the observation noise for each sensor to zero."
  (loop with remaining-channels = (set-difference (get-configuration experiment :available-channels) disabled-channels)
        for channel in remaining-channels
        collect (cons channel 0)))

(defmethod determine-noise-in-observation (experiment disabled-channels (mode (eql :shift)))
  "Determines a standard deviation for each sensor at each observation."
  (loop with remaining-channels = (set-difference (get-configuration experiment :available-channels) disabled-channels)
        for channel in remaining-channels
        for shift = (get-configuration experiment :observation-std)
        collect (cons channel shift)))

(defmethod noise-in-observation ((agent cle-agent) (attr symbol) (mode (eql :none)))
  0)

(defmethod noise-in-observation ((agent cle-agent) (attr symbol) (mode (eql :shift)))
  (random-gaussian 0 (assqv attr (noise-in-each-observation agent))))

;; helper function
(defun random-gaussian (mean st-dev)
  0
  #|(distributions:from-standard-normal (distributions:draw-standard-normal) mean st-dev)|#
  )
