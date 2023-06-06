(in-package :cle)

;; ---------------------------------------------
;; + Gaussian using welford's online algorithm +
;; ---------------------------------------------
(defclass gaussian-welford (gaussian)
  ((M2
    :initarg :M2 :accessor M2 :initform nil :type number)
   (nr-of-samples
    :initarg :nr-of-samples :accessor nr-of-samples :initform nil :type number))
  (:documentation "Gaussian distribution using Welford's online algorithm"))

;; Constructor
(defmethod make-distribution (agent exemplar (mode (eql :gaussian-welford)))
  (let* ((M2 (get-configuration agent :M2))
         (nr-of-samples 1)
         (st-dev (sqrt (/ M2 nr-of-samples)))
         (mean exemplar)
         (history (list (list
                   (interaction-number (current-interaction (experiment agent)))
                   exemplar
                   mean
                   st-dev))))
    (make-instance 'gaussian-welford
                   :mean mean
                   :st-dev st-dev
                   :history history 
                   :M2 0
                   :nr-of-samples nr-of-samples)))

;; Update
(defmethod update-distribution ((new-observation number)
                                (distribution gaussian-welford))
  ;; Step 1: Increment nr-of-samples
  (incf (nr-of-samples distribution))
  ;; Step 2: Update
  (let* ((delta-1 (- new-observation (mean distribution)))
         (new-mean (+ (mean distribution) (/ delta-1 (nr-of-samples distribution))))
         (delta-2 (- new-observation new-mean))
         (new-M2 (+ (M2 distribution) (* delta-1 delta-2))))
    (setf (mean distribution) new-mean
          (st-dev distribution) (sqrt (/ new-M2 (nr-of-samples distribution)))
          (M2 distribution) new-M2)))

;; ------------------------------
;; + Updating prototype history +
;; ------------------------------
(defmethod update-prototype-history ((interaction-number number)
                                     (new-observation number)
                                     (distribution gaussian-welford)
                                     &key &allow-other-keys)
  "Update the distribution history."
  (setf (history distribution) (cons (list interaction-number
                                           new-observation
                                           (mean distribution)
                                           (st-dev distribution))
                                     (history distribution))))

;; --------------------
;; + Helper functions +
;; --------------------
(defmethod copy-object ((distribution gaussian-welford))
  (make-instance 'gaussian-welford
                 :mean (copy-object (mean distribution))
                 :st-dev (copy-object (st-dev distribution))
                 :history (copy-object (history distribution))
                 :nr-of-samples (copy-object (nr-of-samples distribution))
                 :M2 (copy-object (M2 distribution))))
