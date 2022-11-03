
(ql:quickload :read-csv)
(in-package :cl-user)
(ql:quickload :grammar-learning)

(load (babel-pathname
       :directory '("systems" "fcg" "construction-inventory-processor")
       :name "construction-inventory-processor"
       :type "lisp")) ;; force recompilation
#+sbcl (ql:quickload :split-sequence)

(in-package :intention-reading)

;; start the web monitor
(activate-monitor trace-fcg)
;; (deactivate-monitor trace-fcg)


(defun summarize-results (results)
  (loop for result in results
        for index = (first result)
        for status-list = (second result)
        for status = (first status-list)
        for added-by = (second status-list)
        for repair = (string (third status-list))
        do (format t "~d has status ~a ~a ~a~%" index status (string added-by) repair)))


(setf *fcg-constructions* (cl-store:restore (babel-pathname :directory '("systems" "grammar-learning" "storage") :name "cluster-learning-all-cxns" :type "store")))
(setf *clevr-results* (cl-store:restore (babel-pathname :directory '("systems" "grammar-learning" "storage") :name "cluster-learning-results" :type "store")))

(wi:add-element (make-html *fcg-constructions*))
;(wi:add-element (make-html (categorial-network
;                              *fcg-constructions*) :weights? t))
(summarize-results *clevr-results*)
;#+sbcl (sb-ext:quit)

