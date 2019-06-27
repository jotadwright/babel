;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :irl :fcg :clevr-world :clevr-primitives)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :clevr-evaluation
                :preprocess-sentence
                :get-target-value
                :answer->str
                :program->program-tree)
  (:shadowing-import-from :fcg :size))
