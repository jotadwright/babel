(in-package :database-querying)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructing an SQL query ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substitute-query-parts (meaning-network)
  "Substitute the predicates in a meaning network into an SQL query"
  (cond
   ((eq 1 (length meaning-network))
    (first meaning-network))
   ((not (connected-semantic-network meaning-network))
    (error "Cannot execute query: predicates are not fully integrated into a network."))
   (t
    (loop for predicate in meaning-network
          when (variable-p (first predicate))
          return (substitute-query-parts (substitute-query-part predicate (remove predicate meaning-network)))))))

(defun substitute-query-part (predicate meaning-network)
  "Substitutes predicate in meaning network, based on variable"
  (let ((variable (first predicate))
        (substitution (rest predicate)))
    (mapcar #'(lambda (element)
                (if (find variable element :test 'equalp)
                   (substitute substitution variable element) element)) meaning-network)))

(defun format-sql-query (query)
  (let* (;; string with escaped quotes
         (query (string-append "'" (write-to-string query :escape t) "'"))
         ;; remove package qualifiers
         (query (cl-ppcre:regex-replace-all "[^ ()]+::" query ""))
         ;; solve problem with extra space after count
         (query (fcg::replace-all query "count (*)" "COUNT(*)" :test 'equalp)))
    query))

