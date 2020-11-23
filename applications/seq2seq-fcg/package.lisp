(in-package :cl-user)

(defpackage :seq2seq-fcg
  (:documentation "Using Seq2Seq models for the FCG search heuristics")
  (:use :common-lisp :utils :monitors :fcg :clevr-grammar :clevr-world)
  (:import-from :clevr-dialog-grammar :*clevr-dialog*)
  (:import-from :trivial-timeout :with-timeout
                :timeout-error)
  (:shadowing-import-from :fcg :size :attributes))
