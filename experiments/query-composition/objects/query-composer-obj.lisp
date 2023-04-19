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

;; #########################################
;; compose-query
;; --------------------------------------------------------------------

(defmethod compose-query ((composer query-composer) answer &key sort-table star-shortcut exclude-constraint all-queries)
  (let ((exclusion-list '()))
    (loop until (not (queue composer))
             for parent = (pop (queue composer))
             do
            (cond ((not (equal exclusion-list (attrs parent))) (setf exclusion-list (append exclusion-list (attrs parent)))))
            (setf exclusion-list (remove-duplicates exclusion-list))
            (let ((result nil))
              (if (equal (depth parent) 0)
                (setf result (select-compose composer parent answer :sort-table sort-table :star-shortcut star-shortcut))
                (setf result (condition-compose composer parent answer exclusion-list :exclude-id exclude-constraint)))
              (if (and result all-queries)
                (setf (queries composer) (append (queries composer result))))
              (if (and result (not all-queries))
                (return-from compose-query result))))
    (queries composer)))

;; #########################################
;; expand
;; --------------------------------------------------------------------

(defmethod expand ((composer query-composer) answer parent exclusion-list &key sort-table star-shortcut exclude-constraint)
  (cond
   ((equal (depth parent) 0) (select-compose composer parent answer :sort-table sort-table :star-shortcut star-shortcut))
   ((not (equal (depth parent) 0)) (condition-compose composer parent exclusion-list :exclude-id exclude-constraint))))

;; #########################################
;; condition-compose
;; --------------------------------------------------------------------

(defmethod condition-compose ((composer query-composer) parent answer exclusion-list &key exclude-id)
  (dolist (ref-table (ref-tbles parent))
    (let ((attrs '())
          (nodes-lst '()))
      (if exclude-id
        (setf attrs (remove-if #'(lambda (item) (or (equal (constraint item) 'primary) (equal (constraint item) 'foreign))) (attributes ref-table)))
        (setf attrs (attributes ref-table)))
      (dolist (attr attrs)
        (if (or (not (attr-is-present parent attr)) (not (find attr exclusion-list)))
          (progn
            (let ((result (flatten (query (concatenate 'string "SELECT Distinct(" (name attr) ") FROM " (name ref-table))))))
              (dolist (val result)
                (dolist (operator (operators attr))
                  (if (equal (depth parent) 1)
                    (progn
                      (let ((child-node (where-node parent ref-table attr operator val)))
                        (if (goal-test answer child-node) (return-from condition-compose child-node))
                        (setf nodes-lst (append nodes-lst (list child-node)))))
                    (progn
                      (let ((and-child (condition-node composer parent ref-table attr operator val ':and))
                            (or-child (condition-node composer parent ref-table attr operator val ':or)))
                        (if and-child
                          (progn
                            (if (not (equal (length (attrs and-child)) (length (attributes (tble parent)))))
                              (progn
                                 (if (goal-test answer and-child) (return-from condition-compose and-child))
                                 (setf nodes-lst (append nodes-lst (list and-child)))))))
                        (if or-child
                          (progn
                            (if (not (equal (length (attrs or-child)) (length (attributes (tble parent)))))
                              (progn
                                 (if (goal-test answer or-child) (return-from condition-compose or-child))
                                 (setf nodes-lst (append nodes-lst (list or-child))))))))))))))))
      (setf (children parent) nodes-lst)
      (setf (queue composer) (append (queue composer) nodes-lst))))
  nil)

;; #########################################
;; select-compose
;; --------------------------------------------------------------------

(defmethod select-compose ((composer query-composer) parent answer &key sort-table star-shortcut)
   (let ((nodes '())
          (tables nil))
     (if sort-table
       (setf tables (sort-table composer answer))
       (setf tables (tables composer)))
     (dolist (tble tables)
       (let ((permutations nil))
         (cond
          ((and (equal (length (attributes tble)) (length (first answer))) star-shortcut) (setf permutations '((("*")))))
          ((> (length (attributes tble)) (length (first answer))) (setf permutations (get-selection tble (first answer))))
          ((and (equal (length (attributes tble)) (length (first answer))) (not star-shortcut)) (setf permutations (get-selection tble (first answer)))))
         (dolist (permutation permutations)
           (let ((child-node nil))
             (setf child-node (init-node parent permutation tble))
             (setf (children parent) (nconc (children parent) (list child-node)))
             (setf nodes (append nodes (list child-node)))
             ;;Join part
             (let ((select-node (init-node parent permutation tble :join t)))
               (setf nodes (append nodes (inner-outer-compose composer select-node))))))))
             (dolist (child nodes)
               (if (goal-test answer child)
                 (progn
                   (return-from select-compose child))))
     (setf (queue composer) (append (queue composer) nodes))
     nil))

;; #########################################
;; inner-outer-compose
;; --------------------------------------------------------------------

(defmethod inner-outer-compose ((composer query-composer) node)
  (let ((queue (list node))
         (answers '()))
    (loop until (not queue)
             for parent = (pop queue)
             do
             (let* ((q1 "SELECT tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND tc.table_name=")
                     (q2 "SELECT ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name, tc.table_name, kcu.column_name FROM  information_schema.table_constraints AS tc JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name WHERE constraint_type = 'FOREIGN KEY' AND ccu.table_name=")
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
                           (setf inner-node (join-node parent ref-obj (is-table-present (foreign-table ref-obj) (tables composer) :get-obj t)))
                           (setf queue (push-end inner-node queue))
                           (setf answers (push-end inner-node answers))
                           ))))))
               (if references
                 (progn
                   (dolist (ref references)
                     (let ((ref-obj (init-reference-info ref)))
                       (if (not (is-table-present (table-name ref-obj) (ref-tbles parent)))
                         (progn
                           (setf inner-node (join-node parent ref-obj (is-table-present (table-name ref-obj) (tables composer) :get-obj t)))
                           (setf queue (push-end inner-node queue))
                           (setf answers (push-end inner-node answers))
                           ))))))))
    answers))

;; #########################################
;; sort-table
;; --------------------------------------------------------------------

(defmethod sort-table ((composer query-composer) answer)
  (let ((row (first answer))
         (tables  '()))
    (dolist (value row)
      (dolist (table (tables composer)) 
        (dolist (att (attributes table))
          (if (and (typep value (type-att att)) (equal (type-att att) 'string))
            (let ((result (query (concatenate 'string "SELECT " (name att) " FROM " (name table) " WHERE " (name att) " = '" (change-type value) "'"))))
              (if (notempty result)
                (return-from sort-table (list table)))))
          (if (and (typep value (type-att att)) (not (equal (type-att att) 'string)))
            (let ((result (query (concatenate 'string "SELECT " (name att) " FROM " (name table) " WHERE " (name att) " = '" (change-type value) "'"))))
              (if result
                (setf tables (push table tables))))))))
   (if (empty tables)
     (tables composer)
     tables)))

;; #########################################
;; get-selection
;; --------------------------------------------------------------------

(defmethod get-selection (table answer &key function?)
  (let ((list-to-merge '())
         (list-of-function '(:avg :max :min)))
    (dolist (part answer)
      (let ((att-of-type '()))
      (if (typep part 'string)
        (mapcar #'(lambda (x)
                    (if (equal (type-att x) 'string)
                      (pushend (list x) att-of-type))) (attributes table)))
      (if (typep part 'integer)
        (mapcar #'(lambda (x)
                    (if (and (equal (type-att x) 'integer) function?)
                      (dolist (func list-of-function)
                        (pushend (list x func) att-of-type))
                      (pushend (list x) att-of-type))) (attributes table)))
      (push att-of-type list-to-merge)))
    (let ((selection (apply #'combinations list-to-merge)))
        (mapcar #'(lambda (x) (if (duplicates? x) (setf selection (remove x selection)))) selection)
        selection)))