(in-package :cl-user)

(defpackage :seq2seq-fcg
  (:documentation "Using Seq2Seq models for the FCG search heuristics")
  (:use :common-lisp :utils :monitors :fcg)
  (:import-from :snooze
                :explain-condition
                :http-condition
                :status-code
                :defroute
                :payload-as-string)
  (:import-from :cl-json
                :encode-json-to-string
                :encode-json-alist-to-string
                :decode-json-from-string)
  (:import-from :clevr-grammar :*CLEVR*)
  (:import-from :clevr-dialog-grammar :*clevr-dialog*)
  (:import-from :clevr-evaluation :preprocess-sentence))
  (:import-from :clevr-grammar :*CLEVR*))
