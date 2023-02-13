;;initialisation of fake table and attributes
(defun init-table ()
  (let* ((tables '()))
    (dotimes (n 4)
      (let* ((table-ref (make-instance 'table :name "table"))
            (attribute-list '()))
        (dotimes (i 2)
          (setf attribute-list (append attribute-list (list (make-instance 'attribute :name "attributeX" :value "AA" :table-ref table-ref)))))
        (setf (attributes table-ref) attribute-list)
        (if (not (notempty tables))
          (setf tables (list table-ref))
          (setf tables (append tables (list table-ref))))))
    tables))

(defun init-tree (db tree parent-node)
  (when (notempty db)
    (let ((id 1)
          (list-of-node '())
          (table (first db)))
      (dolist (attribute (attributes table))
        ;create the node and push the node into the tree
        (let* ((query-of-node (concatenate 'string (query parent-node) "SELECT " (name attribute) " FROM " (name table)))
              (child-node (make-instance 'node :id id :parent-node parent-node :query query-of-node :attributes (list attribute) :depth 1 :children-node '())))
          (push child-node (nodes tree))
          (push child-node list-of-node)))
      (if (not (notempty (children-node parent-node)))
        (setf (children-node parent-node) list-of-node)
        (setf (children-node parent-node) (append (children-node parent-node) list-of-node)))
      (init-tree (cdr db) tree parent-node))))

;;generate the node with the operator specified
(defun node-operator-generator (id node tree operators)
  (when operators
    (let* ((operator (first operators))
          (query (concatenate 'string (query node) operator " damn"))
          (new-id (+ id 1))
          (child (make-instance 'node :id new-id :parent-node node :query query :attributes (attributes node))))
      (push child (children-node node))
      (push child (nodes tree))
    (node-operator-generator new-id node tree (cdr operators)))))

;general function to generate tree
(defun general (database-schema tree answer)
  (let ((depth 1)
        (id 1))
    (dotimes (n 2) 
          (if (= depth 1)
            (init-tree database-schema tree (first(nodes tree))))
          (if (= depth 2)
            (progn
              (let ((list-of-node (get-all-items-from-depth tree 1))
                    (first 1))
                (dolist (node list-of-node)
                  (clause (get-all-attributes-from-table (list (table-ref (first (attributes node))))) node tree first)))))
          (setf depth (+ depth 1)))))

(defun clause (attributes parent-node tree first)
  (when (notempty attributes)
    (if (equal first 1)
      (progn
        (dolist (operator list-of-operator)
          (let* ((query-or (concatenate 'string (query parent-node) " WHERE " (name (first attributes)) " " operator " value"))
                 (query-and (concatenate 'string (query parent-node) " WHERE " (name (first attributes)) " " operator " value"))
                 (and-node (make-instance 'node
                                          :id 1
                                          :parent-node parent-node
                                          :query query-and
                                          :attributes (attributes parent-node)))
                 (or-node (make-instance 'node
                                         :id 1
                                         :parent-node parent-node
                                         :query query-or
                                         :attributes (attributes parent-node))))

            ;add children to parent
            (if (not (notempty (children-node parent-node)))
              (setf (children-node parent-node) (list and-node or-node))
              (setf (children-node parent-node) (append (children-node parent-node) (list and-node or-node))))
            ;and branche
            (push and-node (nodes tree))
            (clause (cdr attributes) and-node tree '0)
            ;or branche
            (push or-node (nodes tree))
            (clause (cdr attributes) or-node tree '0))))
      (progn
        (dolist (operator list-of-operator)
          (let* ((and-node (and-clause 1 parent-node (first attributes) operator))
                 (or-node (or-clause 1 parent-node (first attributes) operator)))
            ;add children to parent
            (if (not (notempty (children-node parent-node)))
              (setf (children-node parent-node) (list and-node or-node))
              (setf (children-node parent-node) (append (children-node parent-node) (list and-node or-node))))
            ;and branch
            (push and-node (nodes tree))
            (clause (cdr attributes) and-node tree '0)
            ;or branch
            (push or-node (nodes tree))
            (clause (cdr attributes) or-node tree '0)))))
        
   
    (if (equal (length attributes) 1)
      (progn
        (dolist (operator  list-of-operator)
          (let ((and-node (and-clause 1 parent-node (first attributes) operator))
                (or-node (or-clause 1 parent-node (first attributes) operator)))

            ;add children to parent
            (if (not (notempty (children-node parent-node)))
              (setf (children-node parent-node) (list and-node or-node))
              (setf (children-node parent-node) (append (children-node parent-node) (list and-node or-node))))
            (push and-node (nodes tree))
            (push or-node (nodes tree))))))))
;;global variables
(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))
(setf list-of-string-operator '("=" "!="))
(setf list-of-integer-operator '("<" ">" "<=" ">=" "=" "!="))


(defun and-clause (id parent-node attribute operator)
  (let* ((query (concatenate 'string (query parent-node) " AND " (name attribute) " " operator " value"))
        (node-and (make-instance 'node
                                 :id id
                                 :parent-node parent-node
                                 :query query
                                 :attributes (attributes parent-node))))
    node-and))

(defun or-clause (id parent-node attribute operator)
  (let* ((query (concatenate 'string (query parent-node) " OR " (name attribute) " " operator " value"))
         (node-or (make-instance 'node
                                 :id id
                                 :parent-node parent-node
                                 :query query
                                 :attributes (attributes parent-node))))
    node-or))