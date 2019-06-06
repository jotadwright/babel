(in-package :cl-user)

(defpackage :clevr-world
  (:use :common-lisp
        :utils
        #+:hunchentoot-available-on-this-platform :web-interface
        :irl)
  (:import-from :fcg :size) ;; needed to resolve name conflict
  (:import-from :cl-json
                :decode-json-from-string
                :decode-json-from-source)
  (:documentation "A package that provides an interface to the CLEVR dataset"))