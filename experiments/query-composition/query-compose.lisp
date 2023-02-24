(in-package :qc)

(defclass query-composer ()
  ((queue :accessor queue
               :initarg :queue
               :type list
               :documentation "List of node to expand.")
   (tree :accessor tree
           :initarg :tree
           :type tree-query
           :documentation "Tree that represent the path of the object.")
   (current-id :accessor current-id
                    :initarg :current-id
                    :initform 1
                    :type integer
                    :documentation "Current id to be assigned to the new tree node.")
   (tables :accessor tables
               :initarg :tables
               :type list
               :documentation "List of table and attributes who represent the database.")
   (queries :accessor queries
                 :initarg :queries
                 :initform '()
                 :type sql-query
                 :documentation "List of SQL query object that the method return.")))

(defmethod compose-query ((composer query-composer) answer &key exclude-id)
  (loop until (not (queue composer))
           for parent = (pop (queue composer))
           do
          (if (equal (depth parent) 0)
            (progn
              (dolist (tble (tables composer))
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (attributes tble) (length (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (mapcar #'(lambda (x) (push (name x) attributes-names)) perm)
                      (setf child-node (init-node (current-id composer)
                                                                parent
                                                                attributes-names
                                                                tble))
                      (setf (current-id composer) (+ (current-id composer) 1))
                      (setf (children parent) (push child-node (children parent)))
                      (setf (queue composer) (push-end child-node (queue composer)))
                      (if (goal-test answer child-node)
                        (progn
                          (setf (queries composer) (push (q child-node) (queries composer)))
                          (return-from compose-query (q child-node))))))))))
          (if (not (equal (depth parent) 0))
              (progn
                (let ((attrs '()))
                  (if exclude-id
                    (setf attrs (remove-if #'(lambda (item) (equal (name item) "id")) (attributes (tble parent))))
                    (setf attrs (attributes (tble parent))))
                  (dolist (attr attrs)
                    (let ((result (flatten (query (concatenate 'string "SELECT " (name attr) " FROM " (name (tble parent)))))))
                      (dolist (val result)
                        (dolist (operator list-of-operator)
                          (if (equal (depth parent) 1)
                            (progn
                              (let ((child-node (where-node (current-id composer)
                                                                             parent
                                                                             (name attr)
                                                                             operator
                                                                             val
                                                                             '())))
                                (setf (current-id composer) (+ (current-id composer) 1))
                                (setf (children parent) (push child-node (children parent)))
                                (setf (queue composer) (push-end child-node (queue composer)))
                                (if (goal-test answer child-node)
                                  (progn
                                    (setf (queries composer) (push (q child-node) (queries composer)))
                                    (return-from compose-query (q child-node))))))))))))))))

(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))

(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))
          