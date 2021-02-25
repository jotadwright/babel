;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :tasks-and-processes :meta-layer-learning
        :irl :fcg :type-hierarchies :clevr-world
        :clevr-primitives :gl)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :cl-jonathan :decode-json-as-plist-from-source)
  (:shadowing-import-from :fcg :size :attributes))
