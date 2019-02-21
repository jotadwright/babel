;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Web service to access the CLEVR Grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :irl :fcg :clevr)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :clevr-evaluation
                :preprocess-sentence
                :get-target-value
                :answer->str
                :program->program-tree
                :program-tree->alist))
