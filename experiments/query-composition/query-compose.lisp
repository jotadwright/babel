(in-package :qc)

(defclass query-composer ()
  ((queue :accessor queue
               :initarg :queue
               :initform '()
               :type list
               :documentation "List of node to expand.")
   (tree :accessor tree
           :initarg :tree
           :initform nil
           :type tree-query
           :documentation "Tree that represent the path of the object.")
   (tables :accessor tables
               :initarg :tables
               :initform '()
               :type list
               :documentation "List of table and attributes who represent the database.")
   (queries :accessor queries
                 :initarg :queries
                 :initform '()
                 :type sql-query
                 :documentation "List of SQL query object that the method return."))
  (:documentation "Object representing the core of the research. This represents all the information to generate the tree. In particular the tree, the list of nodes to expand, the set of tables in the database as well as the set of nodes that lead to a valid query."))

(defmethod initialize-instance :after ((composer query-composer) &key)
  (let* ((result (query "SELECT name FROM continent where name='Africa'"))
       (root-node (make-instance 'node :id (make-id) :parent nil :children '() :depth 0 :q ""))
       (tree (make-instance 'query-tree :nodes  (list  root-node) :root root-node)))
    (setf (tables composer) (init-schema))
    (setf (tree composer) tree)
    (setf (queue composer) (nodes tree))))
    


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
         (if (not (equal (depth parent) 0))
            (progn
              (if (goal-test answer parent)
                (progn
                  (if all-queries
                    (setf (queries composer) (push parent (queries composer)))
                    (return-from compose-query (q parent)))))))
          
          (if (equal (depth parent) 0)
            (select-compose composer parent answer))
          (if (not (equal (depth parent) 0))
            (let ((start-time (get-internal-real-time)))
              (condition-compose composer parent :exclude-id exclude-id)
              (let ((end-time (get-internal-real-time)))
                (write (- end-time start-time))
                (terpri)))))
  
  (queries composer))


(defmethod condition-compose ((composer query-composer) parent &key exclude-id)
  (let ((attrs '())
         (nodes-lst '()))
    (if exclude-id
      (setf attrs (remove-if #'(lambda (item) (equal (name item) "id")) (attributes (tble parent))))
      (setf attrs (attributes (tble parent))))
    (dolist (attr attrs)
      (if (not (attr-is-present parent attr))
        (progn
          (let ((result (flatten (query (concatenate 'string "SELECT Distinct(" (name attr) ") FROM " (name (tble parent)))))))
            (dolist (val result)
              (dolist (operator (operators attr))
                (if (equal (depth parent) 1)
                  (progn
                    (let ((child-node (where-node parent attr operator val)))
                      (setf nodes-lst (append nodes-lst (list child-node)))))
                  (progn
                    (let ((and-child (and-node composer parent attr operator val))
                          (or-child (or-node composer parent attr operator val)))
                      (if and-child
                        (progn
                          (if (not (equal (length (attrs and-child)) (length (attributes (tble parent)))))
                            (setf nodes-lst (append nodes-lst (list and-child))))))
                      (if and-child
                        (progn
                          (if (not (equal (length (attrs or-child)) (length (attributes (tble parent)))))
                            (setf nodes-lst (append nodes-lst (list or-child))))))))
                  )))))))
    (setf (children parent) nodes-lst)
    (setf (queue composer) (append (queue composer) nodes-lst))))

(defmethod select-compose ((composer query-composer) parent answer)
   (let ((join-nodes '())
          (tables (sort-table composer answer)))
              (dolist (tble tables)
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (sort-type tble (first answer)) (length (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (mapcar #'(lambda (x) (push (name x) attributes-names)) perm)
                      (setf child-node (init-node parent attributes-names tble))
                      (setf (children parent) (nconc (children parent) (list child-node)))
                      (setf (queue composer) (nconc (queue composer) (list child-node)))
                      ;;Join part
                      ;;create node that satisty constraint
                      (let ((select-node (init-node parent attributes-names tble :join t)))
                        (setf join-nodes (append join-nodes (inner-outer-compose composer select-node))))
                      ))))
             ; (setf (queue composer) (nconc (queue composer) join-nodes))
              ))

(defmethod inner-outer-compose ((composer query-composer) node)
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
                           (setf inner-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (tables composer) :get-obj t)  :foreign-ref t))
                           (setf queue (push-end inner-node queue))
                           ;(setf outer-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t)  :foreign-ref t :outer-join t))
                           (setf answers (push-end inner-node answers))
                           ;(setf answers (push-end outer-node answers))
                           ))))))
               (if references
                 (progn
                   (dolist (ref references)
                     (let ((ref-obj (init-reference-info ref)))
                       (if (not (is-table-present (table-name ref-obj) (ref-tbles parent)))
                         (progn
                           (setf inner-node (join-node parent ref-obj (is-table-present (table-name ref-obj) (tables composer) :get-obj t)))
                           (setf queue (push-end inner-node queue))
                           ;(setf outer-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t) :outer-join t))
                           (setf answers (push-end inner-node answers))
                           ;(setf answers (push-end outer-node answers))
                           ))))))))
    answers))

(defmethod sort-table ((composer query-composer) answer)
  (let ((row (first answer))
         (tables  '()))
    (dolist (value row)
      (dolist (table (tables composer)) 
        (dolist (att (attributes table))
          (if (typep value (type-att att))
          ;query the database
            (let ((result (query (concatenate 'string "SELECT " (name att) " FROM " (name table) " WHERE " (name att) " = '" (change-type value) "'"))))
              (if result
                (setf tables (push table tables))))))))
   (if (empty tables)
     (tables composer)
     tables)))

;;UNPRECATED
(defmethod is-permutation ((composer query-composer) node)
  ;get the node with the same depth as new-node
  (let ((node-to-compare '()))
    (mapcar #'(lambda (x)
                (if (equal (depth x) (depth node))
                  (push x node-to-compare)))
              (queue composer))
    ;sort this list in which are the same attrs, table
    (loop until (not node-to-compare )
             for n = (pop node-to-compare)
             do
             (if (and (equal (attributes-selected n) (attributes-selected node))
                         (equal (tble n) (tble node)))
               (progn
                 
                 (let* ((current-n (sort (cdn-to-compare node) #'string-lessp :key #'second))
                        (other-n  (sort (cdn-to-compare n) #'string-lessp :key #'second)))
                   (if (and (equal current-n other-n) (equal (query (sql-compile (q n))) (query (sql-compile (q node)))))
                     (setf (stop n) t))))))))

(defun goal-test (answer node)
  (let ((start-time (get-internal-real-time))
         (res-of nil))
    (setf res-of (query (sql-compile (q node))))
    (let ((end-time (get-internal-real-time)))
      (if (equal answer res-of)
        (progn
          (setf (time-result node) (float (/ (- end-time start-time) 1000)))
          t)))))



