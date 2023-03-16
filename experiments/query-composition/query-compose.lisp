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
  (let* ((root-node (make-instance 'node :id (make-id) :parent nil :children '() :depth 0 :q ""))
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
              (progn (if (goal-test answer parent)
                       (progn (if all-queries
                                (setf (queries composer) (push parent (queries composer)))
                                (return-from compose-query (q parent)))))))
            (if (equal (depth parent) 0)
              (select-compose composer parent answer))
            (if (not (equal (depth parent) 0))
              ;(let ((start-time (get-internal-real-time)))
                (condition-compose composer parent :exclude-id exclude-id)))
                ;(let ((end-time (get-internal-real-time)))
                 ; (write (- end-time start-time))
                 ; (terpri)))))
    (queries composer))


(defmethod compose-query2 ((composer query-composer) answer &key exclude-id all-queries)
  (let ((expand-lst '()))
    (loop until (not (queue composer))
          for parent = (pop (queue composer))
          do
            (if (not (equal (depth parent) 0))
              (progn (if (goal-test answer parent)
                       (progn (if all-queries
                                (setf (queries composer) (push parent (queries composer)))
                                (return-from compose-query2 (q parent)))))))
            (push parent expand-lst)
            (if (not (queue composer))
              (progn
                (expand composer expand-lst answer :exclude-id exclude-id)
                (setf expand-lst '()))))))

    

(defmethod expand ((composer query-composer) queue answer &key exclude-id)
  (loop until (not queue)
           for parent = (pop queue)
           do
          (if (equal (depth parent) 0)
            (select-compose composer parent answer))
          (if (not (equal (depth parent) 0))
            ;(let ((start-time (get-internal-real-time)))
              (condition-compose composer parent :exclude-id exclude-id))))
             ; (let ((end-time (get-internal-real-time)))
              ;  (write (- end-time start-time))
               ; (terpri))))))


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
                (let ((permutations nil))
                  (cond ((equal (length (attributes tble)) (length (first answer))) (setf permutations '(("*"))))
                            ((> (length (attributes tble)) (length (first answer))) (setf permutations (get-selection tble (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (if (equal (length (attributes tble)) (length (first answer)))
                        (setf attributes-names perm)
                        (mapcar #'(lambda (x) (push (name x) attributes-names)) perm))
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
          (if (and (typep value (type-att att)) (equal (type-att att) 'string))
            (let ((result (query (concatenate 'string "SELECT " (name att) " FROM " (name table) " WHERE " (name att) " = '" (change-type value) "'"))))
              (if result
                (return-from sort-table (list table)))))
          (if (typep value (type-att att))
          ;query the database
            (let ((result (query (concatenate 'string "SELECT " (name att) " FROM " (name table) " WHERE " (name att) " = '" (change-type value) "'"))))
              (if result
                (setf tables (push table tables))))))))
   (if (empty tables)
     (tables composer)
     tables)))


;OK
(defmethod get-selection (table answer)
  (let ((list-to-merge '()))
    (dolist (part answer)
      (let ((att-of-type '()))
      (if (typep part 'string)
        (mapcar #'(lambda (x)
                    (if (equal (type-att x) 'string)
                      (push x att-of-type))) (attributes table)))
      (if (typep part 'integer)
        (mapcar #'(lambda (x)
                    (if (equal (type-att x) 'integer)
                      (push x att-of-type))) (attributes table)))
      (pushend att-of-type list-to-merge)))
    (apply #'combinations list-to-merge)
    (let ((selection (apply #'combinations list-to-merge)))
        (mapcar #'(lambda (x) (if (duplicates? x) (setf selection (remove x selection)))) selection)
        selection)))
    

(defun goal-test (answer node)
  (let ((start-time (get-internal-real-time))
         (res-of nil))
    (setf res-of (query (sql-compile (q node))))
    (let ((end-time (get-internal-real-time)))
      (if (equal answer res-of)
        (progn
          (setf (time-result node) (float (/ (- end-time start-time) 1000)))
          t)))))



