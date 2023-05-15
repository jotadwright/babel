;------------------------;
;sql query <-> predicates;
;------------------------;

(defvar *sql-keywords-type* (make-hash-table))
(setf (gethash 'right-elem *sql-keywords-type*) (list :having :where :count :round :avg :max :min :sum :day :month :year :hour :minute :second))
(setf (gethash 'left-and-right *sql-keywords-type*) (list := :< :> :<= :>= :<> :in :not-in :like :not-like :null :not-null :and :or))
(setf (gethash 'between-and-right *sql-keywords-type*) (list :select :from :inner-join :between))




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

(defun sql-to-predicates (list-query)
  (let ((predicate-network '()))
    return predicate-network))