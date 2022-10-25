;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-learning
  (:documentation "Tutor-learner experiment to learn the clevr grammar")
  (:use :common-lisp :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :tasks-and-processes :meta-layer-learning
        :irl :fcg :type-hierarchies :clevr-world :gl)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :clevr-primitives :*clevr-primitives*)
  (:import-from :hybrid-primitives :*hybrid-primitives*
                :load-image :request-attn :clear-session)
  (:shadowing-import-from :fcg :size :attributes))

;; fix circular dependecy between fcg and type hierarchies
(load
 (babel-pathname
  :directory '("systems" "fcg" "construction-inventory-processor")
  :name "construction-inventory-processor"
  :type "lisp"))