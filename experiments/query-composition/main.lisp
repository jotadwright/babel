;(ql:quickload :qc)
(in-package :qc)


;(disconnect-toplevel)



(defun concat-array (array)
  (concatenate 'string "" (first array)))


(defun init-schema ()
  ;(connect-toplevel "master_db" "postgres" "root" "localhost")
  (let ((queries '( "select table_name, column_name from information_schema.columns where table_name = 'continent';"  "select table_name, column_name from information_schema.columns where table_name = 'country';" "select table_name, column_name from information_schema.columns where table_name = 'city';" "select table_name, column_name from information_schema.columns where table_name = 'river';" "select table_name, column_name from information_schema.columns where table_name = 'road';" "select table_name, column_name from information_schema.columns where table_name = 'country_river';" ))
        (tables '()))
    (dolist (q queries)
      (push (init-table (query q)) tables))
    tables))

(defun create (nodes-to-expand tree  id answer &optional tables)
  (let ((nodes nodes-to-expand)
        (parent (first nodes-to-expand)))
      (if (equal (depth parent) 0)
        (progn 
          (dolist (tble tables)
            (let ((permutations (get-all-permutations (attributes tble) '1 '())))
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
          (create nodes tree '1 answer)))))

(defun get-all-permutations (att len subsets)
  (let ((sub subsets))
    (if (<= len (length att))
      (progn
        (setf sub (append sub (permutations-of-length att len)))
        (get-all-permutations att (+ len 1) sub))
      sub)))

(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))

(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))


