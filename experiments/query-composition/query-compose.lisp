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
                      ;;Join part
                      ;;create node that satisty constraint
                      (let ((select-node (init-node 1 parent attributes-names tble :join t)))
                        (setf join-nodes (append join-nodes (inner-outer-compose composer select-node answer))))
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

(defmethod inner-outer-compose ((composer query-composer) node answer)
  (let ((queue (list node))
         (answers '()))
    (loop until (not queue)
             for parent = (pop queue)
             do
             (let* ((q1 "SELECT tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name=")
                     (q2 "SELECT tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND ccu.table_name=")
                    (is-referenced  (query (concatenate 'string q1 "'" (name (car (last (ref-tbles parent)))) "'")))
                    (references (query (concatenate 'string q2 "'" (name (car (last (ref-tbles parent))))"'")))
                    (inner-node nil)
                    (outer-node nil))
               (if is-referenced
                 (progn
                   (dolist (ref is-referenced)
                     (let* ((ref-obj (init-reference-info ref)))
                       (if (not (is-table-present (foreign-table ref-obj) (ref-tbles parent)))
                         (progn
                           (setf inner-node (join-node 1 parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t)  :foreign-ref t))
                           (setf outer-node (join-node 1 parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t)  :foreign-ref t :outer-join t))
                           (push-end inner-node queue)
                           (push-end outer-node queue)
                           (setf answers (push-end inner-node answers))
                           (setf answers (push-end outer-node answers))
                           (if (goal-test answer inner-node)
                             (progn
                               (setf (queries composer) (push (q inner-node) (queries composer)))))
                           (if (goal-test answer outer-node)
                             (progn
                               (setf (queries composer) (push (q outer-node) (queries composer)))))
                           ))))))
               (if references
                 (progn
                   (dolist (ref references)
                     (let ((ref-obj (init-reference-info ref)))
                       (if (not (is-table-present (table-name ref-obj) (ref-tbles parent)))
                         (progn
                           (setf inner-node (join-node 1 parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t)))
                           (setf outer-node (join-node 1 parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t) :outer-join t))
                           (push-end inner-node queue)
                           (push-end outer-node queue)
                           (setf answers (push-end inner-node answers))
                           (setf answers (push-end outer-node answers))
                           (if (goal-test answer inner-node)
                             (progn
                               (setf (queries composer) (push (q inner-node) (queries composer)))))
                           (if (goal-test answer outer-node)
                             (progn
                               (setf (queries composer) (push (q outer-node) (queries composer)))))
                           ))))))))
    answers))

(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))
          