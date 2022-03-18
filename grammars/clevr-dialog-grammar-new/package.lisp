(in-package :cl-user)

(defpackage :clevr-dialog-grammar
  (:documentation "FCG grammar for CLEVR dataset with IRL primitives")
  (:use :common-lisp :monitors :utils :web-interface :fcg ;:clevr-world
   )
  ;(:import-from :monitors
  ;              :activate-monitor)
  (:import-from :irl :bind))