(in-package :qc)

(defmethod make-html-node ((node-obj node))
  (let ((query-string (sql-compile (q node-obj))))
    (cond
     ((search "<=" query-string) (setf query-string (string-replace query-string "<=" "&le;")))
     ((search ">=" query-string) (setf query-string (string-replace query-string ">=" "&ge;")))
     ((search "<" query-string) (setf query-string (string-replace query-string "<" "&lt;")))
     ((search ">" query-string) (setf query-string (string-replace query-string ">" "&gt;"))))
  `((table  :style ,(format nil "border:1px solid black"))
    ((tr)
      ((th), "ID")
      ((td), (id node-obj)))
    ((tr)
      ((th), "Query")
      ((td), query-string)))))

(defmethod get-parent ((node-obj node))
  (make-html-node node-obj))

(defmethod get-children ((node-obj node))
  (loop for child in (children node-obj)
           collect (make-html-node child)))

(defmethod get-tree-html ((node-obj node))
  (make-html-node node-obj)
  (loop for child in (children node-obj)
           collect
          (if (children child)
            (get-tree-html child))))


(define-css 'test "table.report { border:1px solid black }")

(defmethod make-html-report ((compose-obj query-composer) (question-obj question) time-result node-obj sorts)
  (let ((report
  `((table  :class "report")
    ((tr)
     ((th) "Excepted query")
     ((td) ,(query-associated question-obj)))
    ((tr)
     ((th) "Query found")
     ((td) ,(sql-compile (q node-obj))))
    ((tr)
     ((th) "Execution time")
     ((td) ,(time-result node-obj)))
    ((tr)
     ((th) "Search time")
     ((td)  ,time-result))
     ((tr)
     ((th) "Sort performed")
     ((td) ,@(loop for sort in sorts
                    collect `((ul)
                                   ((li) ,sort))))))))
    (add-element report)))



