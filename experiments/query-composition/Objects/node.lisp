(in-package :qc)

(defclass node ()
  ((id
    :type integer
    :initarg :id
    :accessor id)
   (query
    :initarg :query
    :accessor query)
   (attributes
    :initarg :attributes
    :accessor attributes)
   (parent
    :type node
    :initform nil
    :initarg :parent-node
    :accessor parent-node)
   (children-node
    :type list
    :initform nil
    :initarg :children-node
    :accessor children-node)
    (depth
    :type integer
    :initarg :depth
    :accessor depth)))
