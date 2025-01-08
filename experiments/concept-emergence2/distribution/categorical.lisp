(in-package :cle)

;; ----------------------------
;; + Categorical distribution +
;; ----------------------------
(defclass categorical (distribution)
  ((cat-table
    :initarg :cat-table :accessor cat-table :initform (make-hash-table :test #'equalp) :type hash-table)
   (nr-of-samples
    :initarg :nr-of-samples :accessor nr-of-samples :initform nil :type number)
   (history
    :initarg :history :accessor history :initform nil :type list))
  (:documentation "A categorical (i.e. a generalised Bernoulli) distribution."))

;; Constructor
(defmethod make-distribution (agent observation (mode (eql :categorical)))
  (let* ((nr-of-samples 0)
         (history (list (list
                         (interaction-number (current-interaction (experiment agent)))
                         observation)))
         (distribution (make-instance 'categorical
                                      :nr-of-samples nr-of-samples
                                      :history history)))
    (update-distribution observation distribution)
    distribution))

;; Update
(defmethod update-distribution ((new-observation symbol)
                                (distribution categorical))
  ;; Step 1: increment total count
  (incf (nr-of-samples distribution))
  ;; Step 2: increase count of the observed category
  (if (gethash new-observation (cat-table distribution))
    ;; key exists -> increment
    (incf (gethash new-observation (cat-table distribution)))
    ;; key does not exist -> create new key with value 1
    (setf (gethash new-observation (cat-table distribution)) 1)))

;; ------------------------------
;; + Updating prototype history +
;; ------------------------------
(defmethod update-distribution-history ((interaction-number number)
                                        (new-observation symbol)
                                        (distribution categorical)
                                        &key &allow-other-keys)
  "Update the distribution history."
  (setf (history distribution) (cons (list interaction-number
                                           new-observation)
                                     (history distribution))))

;; --------------------
;; + Helper functions +
;; --------------------
(defun synchronize-hash-tables (distribution1 distribution2)
  "Synchronize two hash-tables by adding missing keys with a value of zero."
  (let* ((hash-table1 (cat-table distribution1))
         (hash-table2 (cat-table distribution2))
         (keys2 (hash-keys hash-table1))
         (keys1 (hash-keys hash-table2)))
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
         (counts (loop for key being the hash-keys of (cat-table distribution)
                         using (hash-value frequency)
                       collect (cons key (/ frequency total)))))
    counts))

(defmethod number-of-categories ((distribution categorical))
  "Return the number of observed categories in the distribution."
  (hash-table-count (cat-table distribution)))

(defmethod copy-object ((distribution categorical))
  (make-instance 'categorical
                 :cat-table (copy-object (cat-table distribution))
                 :nr-of-samples (copy-object (nr-of-samples distribution))))

(defmethod print-object ((distribution categorical) stream)
  (pprint-logical-block (stream nil)
    (format stream "<Categorical (~a): ~a" (nr-of-samples distribution) (hash-keys (cat-table distribution)))
    (format stream ">")))
