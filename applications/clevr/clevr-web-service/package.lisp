;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-web-service
  (:documentation "Web service to access the CLEVR Grammar")
  (:use :common-lisp :utils :clevr-world :clevr-primitives)
  (:import-from :irl
                :evaluate-irl-program
                :bind
                :irl-program-p
                :var
                :value
                :primitives-evaluated
                :bindings)
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
  (:import-from :clevr-grammar :*CLEVR* 
                :preprocess-utterance 
                :preprocess-program)
  (:import-from :clevr-evaluation
                :preprocess-program-for-web-service
                :get-target-value
                :answer->str
                :program->rpn
                :linked-bind-statement
                :bind-statement-value
                :input-vars))
