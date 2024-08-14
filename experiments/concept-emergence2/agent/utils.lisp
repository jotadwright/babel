(in-package :cle)

;; -----------------------------------
;; + Utilities for the agent package +
;; -----------------------------------

;; ------------------------------
;; + keeping track of cxn usage +
;; ------------------------------

(defclass usage-table-top ()
  ((usage-counts
    :documentation "Hash table of usage counts."
    :type hash-table :accessor usage-counts :initarg :usage-counts)))

(defclass usage-table-full (usage-table-top)
  ()
  )

(defclass usage-table (usage-table-top)
  ((window
    :documentation "Array keeping track of the usage in a sliding window."
    :type simple-vector :accessor window :initarg :window)
   (window-size
    :documentation "Size of the sliding window."
    :type int :accessor window-size :initarg :window-size)
   (head
    :documentation "Index of the oldest element in the window."
    :type int :accessor head :initform 0)
   (tail
    :documentation "Index of the newest element in the window."
    :type int :accessor tail :initform 0)))

(defun create-usage-table (window-size)
  (if (not (integerp window-size))
    (make-instance 'usage-table-full
                   :usage-counts (make-hash-table :test 'equal))
    (make-instance 'usage-table
                   :usage-counts (make-hash-table :test 'equal)
                   :window (make-array window-size :initial-element nil)
                   :window-size window-size)))

(defun update-usage-count (agent form)
  "Update the usage count of a form in the usage table of an agent."
  (if (eql (type-of (usage-table agent)) 'usage-table)
    (update-usage-table-window agent form)
    (update-usage-table-full agent form)))

(defun update-usage-table-full (agent form)
  (if (gethash form (usage-counts (usage-table agent)))
    (incf (gethash form (usage-counts (usage-table agent))))
    (setf (gethash form (usage-counts (usage-table agent))) 1)))

(defun update-usage-table-window (agent form)
  (let ((usage-table (usage-table agent)))
    (if (not (aref (window usage-table) (tail usage-table)))
      ;; CASE 1: vector is not filled
      (progn
        ;; append newest element
        (setf (aref (window usage-table) (tail usage-table)) form)
        ;; update tail index
        (setf (tail usage-table) (+ (tail usage-table) 1))
        ;; avoid overflow
        (when (= (tail usage-table) (window-size usage-table))
          (setf (tail usage-table) (- (window-size usage-table) 1)))
        ;; update-hash-table
        (if (gethash form (usage-counts usage-table))
          (incf (gethash form (usage-counts usage-table)))
          (setf (gethash form (usage-counts usage-table)) 1)))
      ;; CASE 2: once vector has been built
      (let ((head-val (aref (window usage-table) (head usage-table))))
        ;; update-hash-table
        (decf (gethash head-val (usage-counts usage-table)))
        (when (zerop (gethash head-val (usage-counts usage-table)))
          (remhash head-val (usage-counts usage-table)))
        (if (gethash form (usage-counts usage-table))
          (incf (gethash form (usage-counts usage-table)))
          (setf (gethash form (usage-counts usage-table)) 1))
        ;; replace oldest element (the head) with new form
        (setf (aref (window usage-table) (head usage-table)) form)
        ;; update head and tail indexes
        (setf (head usage-table) (mod (+ (head usage-table) 1) (window-size usage-table)))
        (setf (tail usage-table) (mod (+ (tail usage-table) 1) (window-size usage-table)))))))

(defun unique-forms-in-window (agent)
  (hash-table-count (usage-counts (usage-table agent))))
