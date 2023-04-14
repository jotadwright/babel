(in-package :cl-user)

(defpackage :mindstorms-interface
  (:use :common-lisp
        :test-framework)
  (:import-from :utils
                :exec-and-return :run-prog
                :babel-pathname :always
                :list-of-strings->string
                :stream->list :deg-to-rad
                :mkstr :downcase)
  (:import-from :cl-json
                :encode-json-to-string
                :encode-json-alist-to-string
                :decode-json-from-string)
  (:import-from :drakma :http-request)
  (:import-from :assoc-utils :alistp)
  (:documentation "Implementing the Babel robot interface for the Mindstorms platform"))
