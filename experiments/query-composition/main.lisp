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
            ;(write (length (nodes tree))))
          (if (= depth 2)
            (progn
              (let ((list-of-node (get-all-items-from-depth tree 1)))
                (dolist (node list-of-node)
                  ;(write (get-all-attributes-from-table (list (table-ref (first (attributes node))))))))))
                  (clause (get-all-attributes-from-table (list (table-ref (first (attributes node))))) node tree)))))
                  ;(write 't)))))
                  ;(node-operator-generator 5 node tree list-of-operator))
          (setf depth (+ depth 1)))))

(defun clause (attributes parent-node tree)
  (when (notempty attributes)
    ;manque le premier element de la recursivite
    (if (equal (length attributes) 1)
      (progn
        (dolist (operator  list-of-operator)
          (let* ((query (concatenate 'string (query parent-node) operator " value"))
                (node (make-instance 'node :id 1 :parent-node parent-node :query query :attributes (attributes parent-node))))
            (push node (nodes tree)))))
      (progn
        (dolist (operator list-of-operator)
          (let* ((query-and (concatenate 'string (query parent-node) " AND " (name (first attributes)) operator " value"))
                 (query-or (concatenate 'string (query parent-node) " OR "(name (first attributes)) operator " value"))
                 (node-and (make-instance 'node :id 1 :parent-node parent-node :query query-and :attributes (attributes parent-node)))
                 (node-or (make-instance 'node :id 1 :parent-node parent-node :query query-or :attributes (attributes parent-node))))
            ;and branch
            (push node-and (nodes tree))
            (clause (cdr attributes) node-and tree)
            ;or branch
            (push node-or (nodes tree))
            (clause (cdr attributes) node-or tree)))))))
;;global variables
(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))
(setf list-of-string-operator '("=" "!="))
(setf list-of-integer-operator '("<" ">" "<=" ">=" "=" "!="))
