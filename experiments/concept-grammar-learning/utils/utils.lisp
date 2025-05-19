(in-package :monitors)

(export '(store-monitor))

;;;; store-monitor
(defclass store-monitor (monitor)
  ((file-name :documentation "The file name of the file to write"
              :initarg :file-name
              :reader file-name))
  (:documentation "Monitor that stores data using cl-store"))

(defmethod initialize-instance :around ((monitor store-monitor)
					&key &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method))

(defmethod print-object ((primitive irl::primitive) stream)
  "Prints the concept."
  (pprint-logical-block (stream nil)
    (format stream "<Primitive: ~a"
            (id primitive))
    (format stream ">")))

(defun set-up-monitors (monitors configuration)
  (monitors::deactivate-all-monitors)
  (loop for monitor-name in monitors
        do (monitors::activate-monitor-method (read-from-string monitor-name))
        finally (loop for monitor being the hash-values of monitors::*monitors*
                      when (monitors::active monitor)
                        do (ensure-monitor-directory-exists monitor configuration))))

(defun ensure-monitor-directory-exists (monitor configuration)
  (when (slot-exists-p monitor 'file-name)
    (setf (slot-value monitor 'file-name)
          (ensure-directories-exist
           (merge-pathnames (make-pathname :directory `(:relative ,(get-configuration configuration :experiment-run-name))
                                           :name (pathname-name (file-name monitor)) 
                                           :type (pathname-type (file-name monitor)))
                            (babel-pathname :directory `("experiments"
                                                         "concept-grammar-learning"
                                                         "logging"
                                                         ,(get-configuration configuration :experiment-group)
                                                         ,(get-configuration configuration :dataset-split)
                                                         ,(get-configuration configuration :experiment-name))))))))

(defun get-current-date ()
  (multiple-value-bind
      (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~d-~2,'0d-~d_~dh~dm~ds" year month day hour minute second)))

(defun generate-log-dir-name (seed)
  ;; set a random seed to generate the 5-character random number (to avoid collisions) 
  (set-seed -1)
  ;; create a log-dir-name based on the current-data, the seed, and the random number
  (mkstr (internal-symb (list-of-strings->string
                         (list (get-current-date)
                               (mkstr (format nil "seed~a" seed))
                               (mkstr (random 10) (random 10) (random 10) (random 10) (random 10)))
                         :separator "-"))))
