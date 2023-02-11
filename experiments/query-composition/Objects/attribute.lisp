(in-package :qc)

(defclass attribute ()
  ((name
    :initarg :name
    :accessor name)
   (value
    :initarg :value
    :accessor value
    :type (or integer string))))