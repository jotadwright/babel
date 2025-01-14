(in-package :cl-user)

(defpackage :cle
  (:documentation "Emergent concept learning")
  (:use :common-lisp
        :test-framework
        :utils
        :web-interface
        :monitors
        :experiment-framework
        :irl
        :cl-store)
  (:local-nicknames (:jzon :com.inuoe.jzon)))
