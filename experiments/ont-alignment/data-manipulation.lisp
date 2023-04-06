(in-package :ont-alignment)

;------------------;
;lists manipulation;
;------------------;

(defun pick-random-elem (list)
  "function to get a random element from a given list"
  (nth (random (length list)) list))

(defun get-nth-element (lst n)
  "Returns the nth element of list lst"
  (nth n lst))

(defun list-to-string (list)
  "Takes a list as input and makes it a string able to be processed in an in-clause in sql."
  (let ((formatted-string (concatenate 'string "'" (first list) "'")))
    (loop for elem in list
          do (if (not (string= (first list) elem))
               (setf formatted-string (concatenate 'string formatted-string ", '" elem "'"))))
    formatted-string))

(defun list-to-set (lst)
  "Takes a list as entry and returns a set."
  (remove-duplicates lst :test #'equal))

(defun most-frequent-element (lst)
  "Takes a list as a paremeter and returns the most frequent element of that list."
  (let ((counter (make-hash-table)))
    (dolist (item lst)
      (let ((count (gethash item counter 0)))
        (setf (gethash item counter) (1+ count))))
    (let ((max-count 0)
          (max-item nil))
      (maphash (lambda (item count)
                 (when (> count max-count)
                   (setf max-count count
                         max-item item)))
               counter)
      max-item)))

(defun get-left (lst el)
  (let ((idx (position el lst)))
    (when (and idx (/= idx 0))
      (nth (1- idx) lst))))

(defun get-right (lst el)
  (let ((idx (position el lst)))
    (when (and idx (< idx (1- (length lst))))
      (nth (1+ idx) lst))))

(defun is-member (element list)
  (if (member element list)
      t
      nil))

;--------------------;
;strings manipulation;
;--------------------;

(defun is-substring (substring string)
  "Check if a substring is part of a string."
  (let ((result (search substring string)))
    (if (not (null result))
      (format t "~a is a substring of ~a~%" substring string)
      (format t "~a is not a substring of ~a~%" substring string))))


(defun split-string (str)
  "Splits a string using whitespace as the delimiter."
  (split-sequence:SPLIT-SEQUENCE #\Space str))

;------------;
;json parsing;
;------------;

(defun read-json-data (json-file)
  "function to read data from json file"
  (let* ((file-stream (open json-file :if-does-not-exist nil))
         (json-data (when file-stream (json:decode-json file-stream))))
    (close file-stream)
    json-data))

;---------------------------;
;sql query <-> predicates;
;---------------------------;

;predicates that take the left and the right elem : = < > <= >= <> in not-in like not-like null not-null and or
;predicates that take their right elem : having where count round avg max min sum day month year hour minute second 
;predicates that take the in-between elem and right : select-from (SELECT...FROM...) inner-join-on (INNER JOIN... ON...) between-and (WETWEEN...AND...)

(defvar *sql-keywords-type* (make-hash-table))
(setf (gethash 'right-elem *sql-keywords-type*) (list :having :where :count :round :avg :max :min :sum :day :month :year :hour :minute :second))
(setf (gethash 'left-and-right *sql-keywords-type*) (list := :< :> :<= :>= :<> :in :not-in :like :not-like :null :not-null :and :or))
(setf (gethash 'between-and-right *sql-keywords-type*) (list :select :from :inner-join :between))

(defvar *sql-keyword-predicate*
  (list (cons :SELECT (list 'select-from '?value-out '?result-arg '?select-table))
        (cons :HAVING (list 'having '?filter-result '?filter-condition))
        (cons :WHERE (list 'where '?filter-result '?value-out '?filter-condition))
        (cons :COUNT (list 'count '?result-arg '?value-set))
        (cons :ROUND (list 'round '?result-arg '?num-value))
        (cons :AVG (list 'avg '?result-arg '?num-value))
        (cons :MAX (list 'max '?result-arg '?value-set))
        (cons :MIN (list 'min '?result-arg '?value-set))
        (cons :SUM (list 'sum '?result-arg '?value-set))
        (cons :DAY (list 'day '?result-arg '?date-value))
        (cons :MONTH (list 'month '?result-arg '?date-value))
        (cons :YEAR (list 'year '?result-arg '?date-value))
        (cons :HOUR (list 'hour '?result-arg '?datetime-value))
        (cons :MINUTE (list 'minute '?result-arg '?datetime-value))
        (cons :SECOND (list 'second '?result-arg '?datetime-value))
        (cons := (list '= '?filter-condition '?result '?left-member '?right-member))
        (cons :<> (list '<> '?filter-condition '?result '?left-member '?right-member))
        (cons :< (list '< '?filter-condition '?result '?left-number '?right-number))
        (cons :> (list '> '?filter-condition '?result '?left-number '?right-number))
        (cons :<= (list '<= '?filter-condition '?result '?left-number '?right-number))
        (cons :>= (list '>= '?filter-condition '?result '?left-number '?right-number))
        (cons :IN (list 'in '?condition '?left-member '?value-set))
        (cons :LIKE (list 'like '?condition '?left-string '?regex-string))
        (cons :NULL (list 'null '?condition '?evaluated-value))
        (cons :BETWEEN (list 'between '?condition '?evaluated-value '?low-range '?high-range))
        (cons :INNER-join (list 'inner-join '?join-result '?value-out '?table2 '?table1-prim-key '?table2-foreign-key))
        (cons :AND (list 'and '?cond1 '?cond2))
        (cons :OR (list 'or '?cond1 '?cond2))))


(defun print-hash-table (hash-table)
  (maphash #'(lambda (key value)
               (format t "~a: ~a~%" key value))
           hash-table))

;(print-hash-table *sql-keywords-type*)

(defun find-cons (list-of-cons value)
  "Given an item and a list of cons, retrieves the corresponding cons where the car is equal to the item."
  (cond ((null list-of-cons) nil)
        ((equal (car (car list-of-cons)) value) (car list-of-cons))
        (t (find-cons (cdr list-of-cons) value))))

(defun sql-query-to-predicates (list-query)
  "From an sql keyword such as SELECT or WHERE, sends out a predicate equivalent."
  (let ((predicate-list '())
        (start-index (position :where list-query)))
    ;let's check if the second element of the query (coming after the select) is a keyword
    (if (is-member (get-nth-element list-query 2) (gethash 'right-elem *sql-keywords-type*))
      (push (find-cons *sql-keyword-predicate* (get-nth-element list-query 2)) predicate-list))
    ;then we send the select element to the list
    (push (find-cons *sql-keyword-predicate* :select) predicate-list)
    ;then we send to the list any other predicate equivalent to a found keyword in the query
    (loop for elem in (subseq list-query start-index)
            do (if (symbolp elem)
                 (push (find-cons *sql-keyword-predicate* elem) predicate-list)))
    (print predicate-list)))

;misses : how to push the surrounding element of a symbol to the predicate-list

(sql-query-to-predicates *random-list*)

(print (type-of (get-nth-element *random-list* 1)))

(defun predicates-to-sql-query (predicate-query)
  "From a predicate such as, sends out the sql keyword equivalent."
  )

;(defvar *random-list* (list :select 'film :from 'actorsfilms :where (list := 'film "Perfect Combination")))
;((select-from ?value-out film actorsfilms)(where ?filter-condition ?value-out)(= ?result ?filter-condition film "Perfect Combination"))

;(print (get-left *random-list* :from))

;(sql-query-to-predicates *random-list*)