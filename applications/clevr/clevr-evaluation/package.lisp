;;;; package.lisp

(in-package :cl-user)

(defpackage :clevr-evaluation
  (:documentation "Evaluating the CLEVR grammar and primitives")
  (:use :common-lisp
        :utils
        :monitors
        :plot-raw-data
        :experiment-framework
        :fcg
        :clevr-world
        :clevr-primitives
        :clevr-grammar)
  (:import-from :web-interface
                :add-element
                :s-dot->svg
                :s-dot->image
                :define-css
                :clear-page)
  (:import-from :irl
                :bind :var
                :evaluate-irl-program
                :irl-program->svg
                :get-target-var
                :trace-irl
                :trace-irl-verbose)
  (:import-from :cl-json
                :decode-json-from-source
                :decode-json-from-string
                :encode-json-to-string
                :encode-json-alist-to-string)
  (:import-from :trivial-timeout
                :with-timeout
                :timeout-error)
  (:import-from :cl-csv
                :do-csv :read-csv-row :write-csv-row)
  (:shadowing-import-from :fcg :size :attributes))
  
