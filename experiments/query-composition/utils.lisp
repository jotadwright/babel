; not empty
(defun notempty (list)
  (not (null list)))

; remove duplicates form lisp
(defun remove-duplicates-from-list (list)
  (remove-duplicates list :test #'equal))

; coefficient binomial
(defun binomial-coefficient (n k)
  (let ((result 1))
    (dotimes (i k)
      (setf result (* result (- n i) (/ (+ i 1) (1+ i)))))
    result))

; get all items of the list where depth = x in param
(defun get-all-items-from-depth(tree depth)
  (let ((items '()))
    (dolist (node (nodes tree))
      (if (= (depth node) depth)
        (push node items)))
    items))

;;DEBUGGER
;get all attributes from list of tables
(defun get-all-attributes-from-table (tables)
  (let ((list-of-attribute '()))
    (dolist (table tables)
      (if (not (notempty list-of-attribute))
        (setf list-of-attribute (attributes table))
        (setf list-of-attribute (append list-of-attribute (attributes table)))))
    list-of-attribute))
        
