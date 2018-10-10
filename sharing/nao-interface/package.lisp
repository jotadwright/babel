
(in-package :cl-user)

(defpackage :nao-interface
  (:use :common-lisp
        :test-framework
        :utils
        :cl-json
        :drakma)
  (:shadow "PROTOTYPE")
  (:export :start-nao-server :stop-nao-server :test-server-connection)
  (:documentation "Interface between Babel2 and the Nao Robot"))
