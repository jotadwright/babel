(in-package :concept-representations)

;; ------------------
;; + Abstract class +
;; ------------------

(defclass distribution ()
  ()
  (:documentation "Abstract class for distributions"))

;; ----------------------------
;; + Categorical distribution +
;; ----------------------------
(defclass bernoulli (distribution)
  ((frequencies
    :initarg :frequencies :accessor frequencies :initform (make-hash-table :test #'eq) :type hash-table)
   (nr-of-samples
    :initarg :nr-of-samples :accessor nr-of-samples :initform nil :type number))
  (:documentation "A categorical (i.e. a generalised Bernoulli) distribution."))


;; Constructor
(defmethod make-distribution ((feature-value symbol))
  (let* ((nr-of-samples 0)
         (distribution (make-instance 'categorical
                                      :nr-of-samples nr-of-samples)))
    (update-distribution feature-value distribution)
    distribution))

;; Update
(defmethod update-distribution ((distribution categorical)
                                (feature-value symbol))
  ;; Step 1: increment total count
  (incf (nr-of-samples distribution))
  ;; Step 2: increase count of the observed category
  (if (gethash feature-value (frequencies distribution))
    ;; key exists -> increment
    (incf (gethash feature-value (frequencies distribution)))
    ;; key does not exist -> create new key with value 1
    (setf (gethash feature-value (frequencies distribution)) 1)))

;; --------------------
;; + helper functions +
;; --------------------
(defmethod synchronize-hash-tables ((distribution1 categorical) (distribution2 categorical))
  "Synchronize two categorical distributions by updating missing keys with a value of zero."
  (let* ((hash-table1 (frequencies distribution1))
         (hash-table2 (frequencies distribution2))
         (keys1 (hash-keys hash-table1))
         (keys2 (hash-keys hash-table2)))
    ;; Add missing keys from hash-table1 to hash-table2 with a value of zero
    (dolist (key keys1)
      (unless (gethash key hash-table2)
        (setf (gethash key hash-table2) 0)))
    ;; Add missing keys from hash-table2 to hash-table1 with a value of zero
    (dolist (key keys2)
      (unless (gethash key hash-table1)
        (setf (gethash key hash-table1) 0)))))

(defmethod normalise ((distribution categorical))
  "Normalise the frequencies so that its a valid (discrete) probability distribution."
  (let* ((total (nr-of-samples distribution))
         (counts (loop for key being the hash-keys of (frequencies distribution)
                         using (hash-value frequency)
                       collect (cons key (/ frequency total)))))
    counts))

(defmethod number-of-categories ((distribution categorical))
  "Return the number of observed categories in the distribution."
  (hash-table-count (frequencies distribution)))

(defmethod copy-object ((distribution categorical))
  (make-instance 'categorical
                 :frequencies (copy-object (frequencies distribution))
                 :nr-of-samples (copy-object (nr-of-samples distribution))))

(defmethod print-object ((distribution categorical) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Categorical (~a): ~a" (nr-of-samples distribution) (hash-keys (frequencies distribution)))
    (format stream ">")))

;; -------------------------
;; + Gaussian distribution +
;; -------------------------
(defclass gaussian (distribution)
  ((mean
    :initarg :mean :accessor mean :initform nil :type number)
   (st-dev
    :initarg :st-dev :accessor st-dev :initform nil :type number)
   (nr-of-samples
    :initarg :nr-of-samples :accessor nr-of-samples :initform nil :type number)
   (M2
    :initarg :M2 :accessor M2 :initform nil :type number))
  (:documentation "Gaussian distribution using Welford's online algorithm"))

;; Constructor
(defmethod make-distribution ((feature-value number))
  "Create a gaussian distribution that will be updated using Welford's online algorithm."
  (let* ((M2 0.001) ;; TODO pass as argument with allow-other-keys
         (nr-of-samples 1)
         (st-dev (sqrt (/ M2 nr-of-samples)))
         (mean feature-value))
    (make-instance 'gaussian
                   :mean mean
                   :st-dev st-dev
                   :nr-of-samples nr-of-samples
                   :M2 M2)))

(defmethod update-distribution ((distribution gaussian)
                                (feature-value number))
  "Update the gaussian distribution using Welford's online algorithm."
  ;; Step 1: increment nr-of-samples
  (incf (nr-of-samples distribution))
  ;; Step 2: update using Welford's algorithm
  (let* ((delta-1 (- feature-value (mean distribution)))
         (new-mean (+ (mean distribution) (/ delta-1 (nr-of-samples distribution))))
         (delta-2 (- feature-value new-mean))
         (new-M2 (+ (M2 distribution) (* delta-1 delta-2))))
    (setf (mean distribution) new-mean
          (st-dev distribution) (sqrt (/ new-M2 (nr-of-samples distribution)))
          (M2 distribution) new-M2)))

;; --------------------
;; + Helper functions +
;; --------------------
(defmethod print-object ((distribution gaussian) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Gaussian:~
                        ~:_ mean: ~,3f,~:_ st-dev: ~,3f~:_"
            (mean distribution) (st-dev distribution))
    (format stream ">")))

(defmethod copy-object ((distribution gaussian))
  (make-instance 'gaussian
                 :mean (copy-object (mean distribution))
                 :st-dev (copy-object (st-dev distribution))
                 :nr-of-samples (copy-object (nr-of-samples distribution))
                 :M2 (copy-object (M2 distribution))))
