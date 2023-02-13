(defclass table ()
  ((name
    :type string
    :initarg :name
    :accessor name)
   (attributes
    :type list
    :initform nil
    :initarg :name
    :accessor attributes)))