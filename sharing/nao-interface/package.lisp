
(in-package :cl-user)

(defpackage :nao-interface
  (:use :common-lisp
        :test-framework)
  (:import-from :utils
                :exec-and-return :run-prog
                :babel-pathname
                :always)
  (:import-from :cl-json
                :encode-json-to-string
                :decode-json-from-string)
  (:import-from :drakma
                :http-request)
  (:import-from :assoc-utils
                :alistp)
  (:export :start-nao-server :stop-nao-server :test-server-connection)
  (:documentation "Interface between Babel2 and the Nao Robot"))
