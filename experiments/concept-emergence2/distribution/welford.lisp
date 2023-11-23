(in-package :cle)

;; ---------------------------------------------
;; + Gaussian using welford's online algorithm +
;; ---------------------------------------------

(defclass gaussian-welford (gaussian)
  ((M2
    :initarg :M2 :accessor M2 :initform nil :type number))
  (:documentation "Gaussian distribution using Welford's online algorithm"))

(defmethod make-distribution (agent observation (mode (eql :gaussian-welford)))
  "Create a gaussian distribution that will be updated using welford's online algorithm."
  (let* ((M2 (get-configuration (experiment agent) :M2))
         (nr-of-samples 1)
         (st-dev (sqrt (/ M2 nr-of-samples)))
         (mean observation)
         (history (list (list
                   (interaction-number (current-interaction (experiment agent)))
                   observation
                   mean
                   st-dev))))
    (make-instance 'gaussian-welford
                   :mean mean
                   :st-dev st-dev
                   :nr-of-samples nr-of-samples
                   :history history 
                   :M2 M2)))

(defmethod update-distribution ((new-observation number)
                                (distribution gaussian-welford))
  "Update the gaussian distribution using welford's online algorithm."
  ;; Step 1: increment nr-of-samples
  (incf (nr-of-samples distribution))
  ;; Step 2: update using welford's algorithm
  (let* ((delta-1 (- new-observation (mean distribution)))
         (new-mean (+ (mean distribution) (/ delta-1 (nr-of-samples distribution))))
         (delta-2 (- new-observation new-mean))
         (new-M2 (+ (M2 distribution) (* delta-1 delta-2))))
    (setf (mean distribution) new-mean
          (st-dev distribution) (sqrt (/ new-M2 (nr-of-samples distribution)))
          (M2 distribution) new-M2)))

;; --------------------
;; + Helper functions +
;; --------------------
(defmethod copy-object ((distribution gaussian-welford))
  (make-instance 'gaussian-welford
                 :mean (copy-object (mean distribution))
                 :st-dev (copy-object (st-dev distribution))
                 :nr-of-samples (copy-object (nr-of-samples distribution))
                 :history (copy-object (history distribution))
                 :M2 (copy-object (M2 distribution))))
