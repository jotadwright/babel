(in-package :cl-user)

(defpackage clevr-grammar-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:nicknames :cgl)
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :tasks-and-processes :meta-layer-learning
        :irl :fcg :type-hierarchies :clevr-world
        :clevr-primitives :gl)
  (:import-from :clevr-grammar :*CLEVR*)
  (:shadowing-import-from :fcg :size :attributes))


