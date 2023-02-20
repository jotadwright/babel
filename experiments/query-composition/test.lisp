(ql:quickload :qc)          
(in-package :qc)


(connect-toplevel "master_db" "postgres" "root" "localhost")

(write (query "select id from continent where name='Africa'"))


(let* ((result (query "select id from continent where id ='1'"))
       (node (make-instance 'node :id 0 :parent nil :children '() :depth '0 :q ""))
      (tree (make-instance 'query-tree :nodes '(node) :root node :q "")))
  (create (list node) tree 1 result (init-schema)))

(disconnect-toplevel)





(sql (:select 'size :from 'continent))

(query (:select 'size :from 'continent))

(let* ((result (query (:select 'size :from 'continent)))
      (val (change-type (first (first result)))))
  (write (type-of val))
  (query (concatenate 'string "SELECT id FROM continent WHERE size= " val)))

(let* ((result (query (:select 'name :from 'continent)))
       (val (change-type (first (first result))))
       (q (concatenate 'string "SELECT id FROM continent WHERE name= \'"val"\'")))
  (write (type-of (first (first result))))
  (write q)
  (query q))
  



(defun create (nodes-to-expand tree  id answer &optional tables)
  (let ((nodes nodes-to-expand)
        (parent (first nodes-to-expand)))
      (if (equal (depth parent) 0)
        (progn 
          (dolist (tble tables)
;First rule
            (let ((permutations '()))
              (if (>= (length (attributes tble)) (length (first answer)))
                (setf permutations (permutations-of-length (attributes tble) (length (first answer)))))
;type de sortie ( (attributes) (attributes) )
              (dolist (perm permutations)
;(attributes attributes)
                (let* ((attributes-names '())
                       (child-node nil))
                  (mapcar #'(lambda(x) (push (name x) attributes-names)) perm)
                  (setf child-node (init-node id parent attributes-names tble))
                  (setf (children parent) (push  child-node (children parent)))
                  ;test the node created
                  (if (goal-test answer child-node)
                    (progn
                      (return-from create (q child-node))))))))
                      
;Push nodes creates
          (setf nodes (append nodes (children parent)))
          (setf nodes (remove parent nodes))
          (create nodes tree '1 answer)))
      (if (equal (depth parent) 1)
        (progn
          (let ((list-of-perm (permutations-of-length (attributes (tble parent)) (length (attributes (tble parent))))))
            (dolist (perm list-of-perm)
;get data from the query
              (let ((result (flatten (query (concatenate 'string "SELECT " (name (first perm)) " FROM " (name (tble parent)))))))
;iterate the result value and the operators
                (dolist (value result)
                  (dolist (operator list-of-operator)
                    (let ((child-node (where-node id parent (name (first perm)) operator value (cdr perm))))
                      (setf (children parent) (push child-node (children parent)))
                      (if (goal-test answer child-node)
                        (return-from create (q child-node)))
                      ))))))
          (setf nodes (remove parent nodes))
          (setf nodes (append nodes (children parent)))
          (create nodes tree '1 answer)))))


