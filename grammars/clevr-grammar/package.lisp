;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-grammar
  (:documentation "FCG grammar for CLEVR dataset with IRL primitives")
  (:use :common-lisp :utils :fcg)
  (:import-from :monitors
                :activate-monitor)
  (:import-from :cl-json
                :decode-json-from-source))
