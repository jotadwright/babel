(in-package :cle)

;; ------------------------------
;; + Tracking construction usage +
;; ------------------------------

(defclass usage-tracker ()
  ((usage-tracker
    :documentation "Hash table of usage counts per form."
    :type hash-table
    :accessor usage-tracker
    :initarg :usage-tracker)))

(defclass unbounded-usage-tracker (usage-tracker)
  ()
  (:documentation "Tracks all form usage without forgetting.")
  )

(defclass bounded-usage-tracker (usage-tracker)
  ((window
    :documentation "Circular buffer tracking recent form usage."
    :type simple-vector
    :accessor window
    :initarg :window)
   (window-size
    :documentation "Maximum number of recent forms to track."
    :type integer
    :accessor window-size
    :initarg :window-size)
   (head
    :documentation "Index of the oldest form in the window."
    :type integer
    :accessor head
    :initform 0)
   (tail
    :documentation "Index of the newest form in the window."
    :type integer
    :accessor tail
    :initform 0))
  (:documentation "Tracks form usage within a fixed-size sliding window."))

(defun create-usage-tracker (window-size)
  "Create a usage tracker, either bounded (if WINDOW-SIZE is an integer) or unbounded."
  (if (integerp window-size)
      (make-instance 'bounded-usage-tracker
                     :usage-tracker (make-hash-table :test 'equal)
                     :window (make-array window-size :initial-element nil)
                     :window-size window-size)
      (make-instance 'unbounded-usage-tracker
                     :usage-tracker (make-hash-table :test 'equal))))

(defgeneric update-usage-count (tracker form)
  (:documentation "Update the usage count of FORM in the given TRACKER."))

(defmethod update-usage-count ((tracker unbounded-usage-tracker) form)
  (let* ((counts (usage-tracker tracker))
         (current (gethash form counts)))
    (setf (gethash form counts) (if current (1+ current) 1))))

(defmethod update-usage-count ((tracker bounded-usage-tracker) form)
  (let* ((counts (usage-tracker tracker))
         (window (window tracker))
         (wsize (window-size tracker))
         (head (head tracker))
         (tail (tail tracker)))

    (if (null (aref window tail)) ;; window not yet full
        (progn
          (setf (aref window tail) form)
          (incf (tail tracker))
          (when (>= (tail tracker) wsize)
            (setf (tail tracker) (1- wsize))) ; clamp tail
          (incf (gethash form counts 0)))

        ;; window full, evict oldest and insert new
        (let ((old-form (aref window head)))
          (decf (gethash old-form counts))
          (when (zerop (gethash old-form counts))
            (remhash old-form counts))

          (incf (gethash form counts 0))
          (setf (aref window head) form)

          (setf (head tracker) (mod (1+ head) wsize))
          (setf (tail tracker) (mod (1+ tail) wsize))))))

(defun unique-forms-in-tracker (tracker)
  "Return the number of unique forms tracked."
  (hash-table-count (usage-tracker tracker)))
