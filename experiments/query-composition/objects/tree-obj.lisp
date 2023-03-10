(in-package :qc)

(defclass query-tree ()
  ((nodes :type list :initarg :nodes :accessor nodes)
   (root :type node :initarg :root :accessor root))
  (:documentation "Object representing the search tree for SQL queries. This object is represented by the set of nodes that the tree consists of as well as its very first node."))
