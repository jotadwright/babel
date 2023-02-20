(in-package :qc)

(defclass node ()
  ((id :type integer :initarg :id :accessor id)
   (parent :type node :initarg :parent :initform nil :accessor parent)
   (children :type list :initarg :children :initform '() :accessor children)
   (depth :type integer :initarg :depth :initform nil :accessor depth)
   (tble :type table :initarg :tble :initform nil :accessor tble)
   (attrs :type attrs :initarg :attrs :initform nil :accessor attrs)
   (q :type integer :initarg :q :initform "" :accessor q)))

;;DEBUGGER
(defun init-node (id node attributes table)
  "function that create a node with the SELECT .. FROM .. clause and return the newly created node with its associated parent."
    (let* ((q "SELECT ")
           (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1)  :q "" :tble table))
           (last-elem (last attributes)))
      (dolist (att-n attributes)
        (if (or (equal (length attributes) 1) (equal att-n (first last-elem)))
         (setf q (concatenate 'string q att-n))
          (setf q (concatenate 'string q att-n ","))))
      (setf q (concatenate 'string q " FROM "(name  table)))
      (setf (q child) q)
      child))
        
    
;;OK
(defun where-node (id node attribute operator value att)
  "function that creates a node with the WHERE clause and returns the newly created node with its associated parent."
  (let* ((val (change-type value))
         (q (concatenate 'string (q node) " WHERE " attribute " " operator " '"val"'"))
          (child (make-instance 'node :id id :parent node :depth (+ (depth node) 1) :q q :attrs att :tble (tble node))))
    (push child (children node))
    child))
;;OK
(defun and-node (id depth node attribute operator value)
  "function that creates a node with the AND clause and returns the newly created node with its associated parent."
  (let* ((q (concatenate 'string (q node) " AND " attribute " " operator " '" value "'"))
          (child (make-instance 'node :id id :parent node :depth depth :q q)))
    (push child (children node))
    node))
;;OK
(defun or-node (id depth node attribute operator value)
  "function that creates a node with the OR clause and returns the newly created node with its associated parent."
  (let* ((q (concatenate 'string (q node) " OR " attribute " " operator " '" value "'"))
          (child (make-instance 'node :id id :parent node :depth depth :q q)))
    (push child (children node))
    node))