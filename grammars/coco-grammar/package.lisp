;;;; package.lisp

(in-package :cl-user)

(defpackage :coco-grammar
  (:documentation "FCG grammar for the 'cats and dogs demo'")
  (:use :common-lisp :utils :web-interface :fcg
        :clevr-world :clevr-primitives)
  (:import-from :monitors
                :activate-monitor)
  (:import-from :irl :bind
                :get-target-var)
  (:import-from :cl-json
                :decode-json-from-source)
  (:shadowing-import-from :fcg :size :attributes)
  (:export :*COCO*))

(defpackage :coco-web-service
  (:use :common-lisp :utils :web-interface
        :clevr-world :clevr-primitives)
  (:import-from :irl :bind
                :get-target-var)
  (:import-from :fcg :statuses
                :applied-constructions
                :analyse-solution
                :unit-bindings->graph)
  (:import-from :snooze
                :explain-condition
                :http-condition
                :status-code
                :defroute
                :payload-as-string)
  (:import-from :cl-json
                :encode-json-alist-to-string
                :decode-json-from-string)
  (:import-from :coco-grammar :*COCO*))