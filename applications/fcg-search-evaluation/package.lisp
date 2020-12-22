(in-package :cl-user)

(defpackage :fcg-search-evaluation
  (:documentation "Evaluating the search in FCG")
  (:use :common-lisp :utils :monitors
   :fcg :clevr-world :clevr-grammar)
  (:import-from :cl-csv :read-csv-row :write-csv-row :do-csv)
  (:shadowing-import-from :fcg :size :attributes))
