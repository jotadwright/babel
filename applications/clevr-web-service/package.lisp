;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-web-service
  (:documentation "Web service to access the CLEVR Grammar")
  (:use :common-lisp :utils :clevr)
  (:import-from :irl
                :evaluate-irl-program
                :bind
                :irl-program-p
                :var
                :value)
  (:import-from :fcg
                :statuses :size
                :applied-constructions)
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
  (:import-from :clevr-evaluation
                :preprocess-sentence
                :get-target-value
                :answer->str
                :program->rpn))
