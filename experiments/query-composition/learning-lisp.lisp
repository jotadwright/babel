;random choice
(defun one-of (set)
    (list (random-elem set)))

(defun random-elem (choice)
    (elt choice (random (length choice))))

(defun attribute (type-of-result)
  (if (typep type-of-result 'integer)
      (one-of '(attribute1 attribute2))
   (one-of '(attribute3 attribute4))))

(defun table ()
  (one-of'(table1 table2 table3 table4)))

(defun operator ()
  (one-of '(< > = != <= >=)))

(defun separator (elem)
  (if (not (last elem))
    (setf r (list(car elem)))
    (setf r (append (list(car elem))(last elem))))
  r)

;; clause function
;; made for contruct clause condition after the WHERE in SQL
(defun clause (criteres &optional critere-query)
  (when criteres
    (if (not critere-query) ; if critere-query is not NIL
        (setf re (separator(car criteres)))
      (if (equal (length criteres) 1) ; if there are only one elem in the list
          (setf re (append critere-query (separator (car criteres)))) ; yes then app the simple elem
        (setf re (append critere-query separator (car criteres))))) ; no add the first one in the critere-query
    (if (equal (length criteres) 1) ; if there are only one elem in the list
        (clause NIL critere-query)
      (clause (cdr criteres) re)))
  re)

;; query-compositor function
;; made for contruct the entire SQL query by different parameters
(defun query-compositor(list-of-critere list-of-information-needed)
  (setf query '(select))
  (setf query (append query (attribute list-of-information-needed)))
  (setf query (append query '(from)))
  (setf query (append query '(table)))
  (setf query (append query '(where)))
  (setf query (append query (clause list-of-critere))))

;(setf init-criteres (list (list 'a 'and) (list 'b nil)))