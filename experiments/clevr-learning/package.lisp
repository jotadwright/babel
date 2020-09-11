;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :irl :fcg :clevr-world :clevr-primitives
        :trivial-timeout)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :clevr-evaluation
                :preprocess-sentence
                :get-target-value
                :answer->str
                :program->program-tree)
  (:import-from :cl-json :decode-json-from-string)
  (:shadowing-import-from :fcg :attributes))

(in-package :clevr-learning)
