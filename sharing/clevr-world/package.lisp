(in-package :cl-user)

(defpackage :clevr-world
  (:use :common-lisp
        :utils
        #+:hunchentoot-available-on-this-platform :web-interface
        ;:irl
        :irl-2)
  ;(:import-from :fcg :size :attributes) ;; needed to resolve name conflict
  (:shadowing-import-from :fcg :size)
  (:shadowing-import-from :irl-2 :size)
  (:import-from :cl-json
                :decode-json-from-string
                :decode-json-from-source)
  (:documentation "A package that provides an interface to the CLEVR dataset"))