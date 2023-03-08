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
                 :documentation "List of SQL query object that the method return.")))

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
                    (return-from compose-query (sql-compile (q parent))))))))
          (if (equal (depth parent) 0)
            (select-compose composer parent answer))
          (if (not (equal (depth parent) 0))
            (condition-compose composer parent :exclude-id exclude-id))))

(defmethod condition-compose ((composer query-composer) parent &key exclude-id)
  (let ((attrs '()))
    (if exclude-id
      (setf attrs (remove-if #'(lambda (item) (equal (name item) "id")) (attributes (tble parent))))
      (setf attrs (attributes (tble parent))))
    (dolist (attr attrs)
      (let ((result (flatten (query (concatenate 'string "SELECT " (name attr) " FROM " (name (tble parent)))))))
        (dolist (val result)
          (let ((operators '(:< :> :<= :>= :!= :=)))
            (if (not (typep (first result) 'bit))
              (setf operators  '(:!= :=)))
            (dolist (operator operators)
              (if (equal (depth parent) 1)
                (progn
                  (let ((child-node (where-node parent
                                                (name attr)
                                                operator
                                                val
                                                '())))
                    (setf (children parent) (push child-node (children parent)))
                    (setf (queue composer) (push-end child-node (queue composer)))))))))))))
                  ;(let ((and-child (and-node parent attr operator val))
                   ;     (or-child (or-node parent attr operator val)))
                    ;(setf (children parent) (append (children parent) (list and-child or-child)))
                    ;(if (not (equal (length (attrs and-child)) (length (attributes (tble parent)))))
                     ; (progn
                      ;  (setf (queue composer) (push-end and-child (queue composer)))
                       ; (setf (queue composer) (push-end or-child (queue composer))))
                      ;(progn
                       ; (write "finish leaf")))))))))))))

(defmethod select-compose ((composer query-composer) parent answer)
   (let ((join-nodes '()))
              (dolist (tble (tables composer))
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (sort-by-type (attributes tble) (first answer)) (length (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (mapcar #'(lambda (x) (push (name x) attributes-names)) perm)
                      (setf child-node (init-node parent
                                                                attributes-names
                                                                tble))
                      (setf (children parent) (push child-node (children parent)))
                      (setf (queue composer) (push-end child-node (queue composer)))
                      ;;Join part
                      ;;create node that satisty constraint
                      (let ((select-node (init-node parent attributes-names tble :join t)))
                        (setf join-nodes (append join-nodes (inner-outer-compose composer select-node))))
                      ))))
              (setf (queue composer) (append (queue composer) join-nodes))))

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
                           (write (foreign-table ref-obj))
                           (setf inner-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t)  :foreign-ref t))
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
                           (setf inner-node (join-node parent ref-obj (is-table-present (table-name ref-obj) (ref-tbles parent) :get-obj t)))
                           ;(setf outer-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (ref-tbles parent) :get-obj t) :outer-join t))
                           (setf answers (push-end inner-node answers))
                           ;(setf answers (push-end outer-node answers))
                           ))))))))
    answers))

(defun goal-test (answer node)
  (let ((start-time (get-internal-real-time))
         (res-of nil))
    (setf res-of (query (sql-compile (q node))))
    (let ((end-time (get-internal-real-time)))
      (if (equal answer res-of)
        (progn
          (setf (time-result node) (float (/ (- end-time start-time) 1000)))
          t)))))



