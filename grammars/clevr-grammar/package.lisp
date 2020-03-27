;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-grammar
  (:documentation "FCG grammar for CLEVR dataset with IRL primitives")
  (:use :common-lisp :utils :fcg :clevr-world)
  (:import-from :monitors
                :activate-monitor)
  (:import-from :irl-2 :bind)
  (:import-from :cl-json
                :decode-json-from-source)
  (:shadowing-import-from :fcg :size :attributes)
  (:export :*CLEVR*))
