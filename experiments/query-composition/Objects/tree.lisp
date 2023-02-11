(in-package :qc)

(defclass query-tree ()
  ((query
    :type string
    :initarg :query
    :accessor query)
    (nodes
    :type list
    :initarg :nodes
    :accessor  nodes)
    (root-node
     :type node
     :initarg :root-node
     :accessor root-node)))
