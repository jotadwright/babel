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
