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
                 :documentation "List of SQL query object that the method return.")
   (state :accessor state
             :initarg :state
             :initform 0
             :type integer
             :documentation "The state to check if a new query is found.")))

;;PARAMS
;; composer: A query-composer object.
;; answer: The goal of the tested queries.
;; KEYS
;; exclude-id: If true, that's exclude the id in the clause condition.
;; all-queries: If true, the program returns all queries that match the answer.
(defmethod compose-query ((composer query-composer) answer &key exclude-id all-queries)
  (loop until (not (queue composer))
           for parent = (pop (queue composer))
           do
          (if (or (not (equal (length (queries composer)) (state composer))) all-queries)
            (progn
              (setf (state composer) (+ (state composer) 1))
              (return-from compose-query (nth (- (length (queries composer)) (state composer)) (queries composer)))))
          (if (equal (depth parent) 0)
            (progn
              (let ((join-nodes '()))
              (dolist (tble (tables composer))
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (sort-by-type (attributes tble) (first answer)) (length (first answer)))))
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
                          (setf (queries composer) (push (q child-node) (queries composer))))))))))))
          (if (not (equal (depth parent) 0))
              (progn
                (let ((attrs '()))
                  (if exclude-id
                    (setf attrs (remove-if #'(lambda (item) (equal (name item) "id")) (attributes (tble parent))))
                    (setf attrs (attributes (tble parent))))
                  (dolist (attr attrs)
                    (let ((result (flatten (query (concatenate 'string "SELECT " (name attr) " FROM " (name (tble parent)))))))
                      (dolist (val result)
                        (let ((operators '("<" ">" "<=" ">=" "!=" "=")))
                          (if (not (typep (first result) 'bit))
                            (setf operators  '("!=" "=")))
                        (dolist (operator operators)
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
                                    (setf (queries composer) (push (q child-node) (queries composer)))))))
                            (progn
                              (let ((and-child (and-node (current-id composer)
                                                                         parent
                                                                         attr
                                                                         operator
                                                                         val))
                                     (or-child (or-node (+ (current-id composer) 1)
                                                                 parent
                                                                 attr
                                                                 operator
                                                                 val)))
                                (setf (children parent) (append (children parent) (list and-child or-child)))
                                (setf (current-id composer) (+ (current-id composer) 2))
                                (if (not (equal (length (attrs and-child)) (length (attributes (tble parent)))))
                                  (progn
                                    (setf (queue composer) (push-end and-child (queue composer)))
                                    (setf (queue composer) (push-end or-child (queue composer))))
                                  (progn
                                    (write "finish leaf")))
                                (if (goal-test answer and-child)
                                  (progn
                                    (push (q and-child) queries)))
                                (if (goal-test answer or-child)
                                  (progn
                                    (push (q or-child) queries))))))))))))))))
                                  

(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))
          