;------------------------;
;sql query <-> predicates;
;------------------------;

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

;misses :
; - how to push the surrounding element(s) of a symbol to the predicate-list (using get-left and get-right functions?)
; - and the reverse function

;(defvar *random-list* (list :select 'film :from 'actorsfilms))
;((select-from ?value-out film actorsfilms)(where ?filter-condition ?value-out)(= ?result ?filter-condition film "Perfect Combination"))

;(print (get-left *random-list* :from))

;(sql-query-to-predicates *random-list*)

;(print (type-of (get-nth-element *random-list* 1)))



;(execute-postmodern-query '(:select (:count actor) (:avg year) :from actorsfilms))

;; '(:select actor film :from actorsfilms)
'((comma ?columns ?first-column ?second-column)
  (bind column ?first-column actor)
  (bind column ?second-column film)
  (select ?result ?columns ?table ?where-clause)
  (bind table ?table actorsfilms))

;; '(:select (:count actor) :from actorsfilms)
'((aggregate ?aggregate-clause ?aggregator ?column)
  (bind aggregator ?aggregator count)
  (bind column ?column actor)
  (select ?result ?aggregate-clause ?table ?where-clause)
  (bind table ?table actorsfilms))

;; '(:select film :from actorsfilms :where (:= actor "Gerard Depardieu"))
  '((bind column ?column film)
    (select ?result ?column ?table ?where-clause)
    (bind table ?table actorsfilms)
    (where ?where-clause ?filter-condition)
    (equals ?filter-condition ?column-2 ?comparator)
    (bind column ?column-2 actor)
    (bind concept ?comparator "Gerard Depardieu"))