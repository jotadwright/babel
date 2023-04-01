(in-package :cl-user)

(defpackage :mwm-evaluation
  (:documentation "Integrating the multi-dimensional concepts in vqa")
  (:use :common-lisp
        :utils
        :monitors
        :plot-raw-data
        :experiment-framework
        :web-interface
        :fcg
        :irl
        :clevr-world
        :clevr-grammar-v2
        :mwm)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :mwm
   :trace-interaction-in-web-interface :lexicon :learner :experiment :mwm-experiment :display-lexicon)
  (:import-from :monitors
   :deactivate-all-monitors :activate-monitor)
  (:import-from :web-interface
   :define-css :clear-page :add-element)
  (:import-from :cl-store :restore)
  (:import-from :clevr-world :get-pathname)
  (:import-from :irl :trace-irl)
  (:shadowing-import-from :fcg :size :attributes :trace-fcg))
  
