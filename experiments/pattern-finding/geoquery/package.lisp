(in-package :cl-user)

(defpackage :pf-for-sql
  (:documentation "Pattern finding for SQL")
  (:use :common-lisp :common-lisp-user :utils :experiment-framework
   :plot-raw-data :monitors :web-interface
   :tasks-and-processes :meta-layer-learning
   :irl :fcg :clevr-world)
  (:import-from :cl-json :decode-json-from-source)
  (:shadowing-import-from :clevr-world :size :attributes))