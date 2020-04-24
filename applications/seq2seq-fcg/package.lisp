(in-package :cl-user)

(defpackage :seq2seq-fcg
  (:documentation "Using Seq2Seq models for the FCG search heuristics")
  (:use :common-lisp :utils :monitors :fcg)
  (:import-from :clevr-grammar :*CLEVR*))
