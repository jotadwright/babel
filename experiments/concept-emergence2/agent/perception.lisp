(in-package :cle)

;; --------------
;; + Perception +
;; --------------

(defclass perceive ()
  ((disabled-features
    :documentation "Disabled/defected features."
    :type hash-table :accessor disabled-features :initarg :disabled-features :initform nil)
   (noise-in-each-sensor
    :documentation "Fixed noise on each feature"
    :type list :accessor noise-in-each-sensor :initarg :noise-in-each-sensor :initform nil)
   (noise-in-each-observation
    :documentation "Noise on each feature at every observation"
    :type list :accessor noise-in-each-observation :initarg :noise-in-each-observation :initform nil)
   (perceived-objects
    :documentation "Stores perceived objects"
    :type perceived-objects :accessor perceived-objects :initform (make-hash-table))))

;; ------------------------
;; + feature availability +
;; ------------------------

(defmethod switch-feature-availability (agent (feature-name symbol))
  "Switches the availability of the given feature."
  (if (gethash feature-name (disabled-features agent))
    (remhash feature-name (disabled-features agent))
    (setf (gethash feature-name (disabled-features agent)) feature-name)))

;; ---------
;; + NOISE +
;; ---------

;; (defmethod perceive-object-val ((agent cle-agent) (entity entity) attr)
;;   "Perceives the value in a given sensor 'attr' of a given object.

;;    This reading can be affected by two types of noise.
;;    The raw observation is the true value in that feature of the object.
;;    The sensor-noise term is a fixed shift (in either direction).
;;    The observation noise term is different for every observation."
;;   (if (and (gethash (id object) (perceived-objects agent))
;;            (gethash attr (gethash (id object) (perceived-objects agent))))
;;     ;; CASE 1: object found + existing attr
;;     (gethash attr (gethash (id object) (perceived-objects agent)))
;;     ;; CASE 2: if could not find entry directly
;;     (let* ((raw-observation-val (get-object-val object attr))
;;            (sensor-noise (noise-in-sensor agent attr (get-configuration (experiment agent) :sensor-noise)))
;;            (observation-noise (noise-in-observation agent attr (get-configuration (experiment agent) :observation-noise)))
;;            (final-val
;;             ;; process the observation in function of its nature (categorical vs continuous)
;;             (if (not raw-observation-val)
;;               ;; no observation, for example if a feature is disabled!
;;               nil
;;               ;; observation received
;;               (if (feature-continuous-p (world (experiment agent)) (current-view agent) attr)
;;                 ;; observation is continuous
;;                 (if (and (zerop sensor-noise) (zerop observation-noise))
;;                   ;; no noise to add, return the raw (true) observation
;;                   raw-observation-val
;;                   ;; add the noise, but cap final result between 0 and 1
;;                   (min (max 0 (+ raw-observation-val sensor-noise observation-noise)) 1))
;;                 ;; observation is categorical
;;                 raw-observation-val))))

;;       (if (gethash (id object) (perceived-objects agent))
;;         ;; case 2A: object existed, but no entry
;;         (setf (gethash attr (gethash (id object) (perceived-objects agent))) final-val)
;;         ;; CASE 2B: object did not exist
;;         (progn
;;           ;; first create object
;;           (setf (gethash (id object) (perceived-objects agent)) (make-hash-table))
;;           ;; then set value
;;           (setf (gethash attr (gethash (id object) (perceived-objects agent))) final-val)))
;;       ;; in both cases return the final-value
;;       (gethash attr (gethash (id object) (perceived-objects agent))))))

;; -------------------
;; + noise-in-sensor +
;; -------------------

(defmethod determine-noise-in-sensor (experiment features (mode (eql :none)))
  "Sets the fixed shift for each sensor to zero."
  (loop for feature in features
        collect (cons feature 0)))

(defmethod determine-noise-in-sensor (experiment features (mode (eql :shift)))
  "Determines a fixed shift for each sensor."
  (loop for feature in features
        for shift = (random-gaussian 0 (get-configuration experiment :sensor-std))
        collect (cons feature shift)))

(defmethod noise-in-sensor ((agent cle-agent) (attr symbol) (mode (eql :none)))
  "No noise on sensor reading."
  0)

(defmethod noise-in-sensor ((agent cle-agent) (attr symbol) (mode (eql :shift)))
  "Sensor reading is shifted by a fixed value."
  (assqv attr (noise-in-each-sensor agent)))

;; ------------------------
;; + noise-in-observation +
;; ------------------------

(defmethod determine-noise-in-observation (experiment features (mode (eql :none)))
  "Sets the observation noise for each sensor to zero."
  (loop for feature in features
        collect (cons feature 0)))

(defmethod determine-noise-in-observation (experiment features (mode (eql :shift)))
  "Determines a standard deviation for each sensor at each observation."
  (loop for feature in features
        for shift = (get-configuration experiment :observation-std)
        collect (cons feature shift)))

(defmethod noise-in-observation ((agent cle-agent) (attr symbol) (mode (eql :none)))
  "No noise on sensor reading."
  0)

(defmethod noise-in-observation ((agent cle-agent) (attr symbol) (mode (eql :shift)))
  "Sensor reading is shifted by a randomly sampled value from a gaussian distribution with mean 0 and a specified std."
  (random-gaussian 0 (assqv attr (noise-in-each-observation agent))))

;; helper function
(defun random-gaussian (mean st-dev)
  "Returns a random number from a gaussian distribution with the given mean and standard deviation."
  (distributions:from-standard-normal (distributions:draw-standard-normal) mean st-dev))
  ;0)
