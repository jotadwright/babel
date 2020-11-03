(in-package :cl-user)

(defpackage :fcg-search-evaluation
  (:documentation "Evaluating the search in FCG")
  (:use :common-lisp :utils :monitors
   :fcg :clevr-world :clevr-grammar)
  (:shadowing-import-from :fcg :size :attributes))
