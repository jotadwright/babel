(in-package :qc)

(defun query-generator (queue tree answer &optional tables)
  (let ((queries '()) 
        (id 1))
    (loop
        named search
        until (not queue)
        for parent = (pop queue)
        do
          (if (equal (depth parent) 0)
            (progn
              (dolist (tble tables)
                (let ((permutations '()))
                  (if (>= (length (attributes tble)) (length (first answer)))
                    (setf permutations (permutations-of-length (attributes tble) (length (first answer)))))
                  (dolist (perm permutations)
                    (let* ((attributes-names '())
                           (child-node nil))
                      (mapcar #'(lambda (x) (push (name x) attributes-names)) perm)
                      (setf child-node (init-node id parent attributes-names tble))
                      (setf id (+ id 1))
                      (setf (children parent) (push child-node (children parent)))
                      (if (goal-test answer child-node)
                        (progn
                          (push (q child-node) queries)
                          (return-from query-generator (q child-node))))
                      (push child-node queue)))))))
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
                            (setf id (+ id 1))
                            (setf (children parent) (push child-node (children parent)))
                            (push child-node queue)
                            (if (goal-test answer child-node)
                              (progn
                                (push (q child-node) queries)))))))))))
            (if (> (depth parent) 1)
              (progn
                (let ((results (flatten (query (concatenate 'string "SELECT " (name (first (attrs parent))) " FROM " (name (tble parent)))))))
                  (dolist (value results)
                    (dolist (operator list-of-operator)
                      (let ((and-child (and-node id parent (name (first (attrs parent))) operator value))
                            (or-child (or-node (+ id 1) parent (name (first (attrs parent))) operator value)))
                        (setf (children parent) (append (children parent) (list and-child or-child)))
                        (setf id (+ id 2))
                        (if (goal-test answer and-child)
                          (push (q and-child) queries))
                        (if (goal-test answer or-child)
                          (push (q or-child) queries))
                        (if (> (length (attrs parent)) 1)
                          (progn
                            (push or-child queue)
                            (push and-child queue))))))))))
    queries))
       
       

;INNER JOIN CLAUSE
;;TODO
;OUTER JOIN CLAUSE
;;TODO


;Goal test
(defun goal-test (answer node)
  (let ((res-of (query (q node))))
    (if (equal answer res-of)
      t)))

(setf list-of-operator '("<" ">" "<=" ">=" "!=" "="))


