(in-package :slp)

(defclass signed-form-predicates ()
  ((predicates
    :type list
    :accessor predicates
    :initform nil
    :initarg :predicates)))