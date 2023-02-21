(in-package :qc)

(defclass attribute ()
  ((name
    :initarg :name
    :type string
    :accessor name)
   (value
    :initarg :value
    :accessor value
    :type (or integer string))))
