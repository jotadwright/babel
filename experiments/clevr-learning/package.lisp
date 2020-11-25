;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :tasks-and-processes :meta-layer-learning
        :irl :fcg :clevr-world :clevr-primitives)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :cl-json :decode-json-from-string
                :decode-json-from-source)
  (:shadowing-import-from :fcg :size :attributes))
