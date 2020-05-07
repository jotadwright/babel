;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-grammar
  (:documentation "FCG grammar for CLEVR dataset with IRL primitives")
  (:use :common-lisp :utils :fcg :clevr-world :clevr-primitives)
  (:import-from :monitors
                :activate-monitor)
  (:import-from :irl :bind
                :get-target-var)
  (:import-from :cl-json
                :decode-json-from-source)
  (:shadowing-import-from :fcg :size)
  (:export :*CLEVR*))
