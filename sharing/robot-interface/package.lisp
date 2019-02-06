(in-package :cl-user)

(defpackage :robot-interface
  (:use :common-lisp
        :test-framework
        :nao-interface)
  (:import-from :utils :babel-pathname :deg-to-rad :run-prog)
  (:documentation "A high-level interface for using robots in the Babel multi-agent experiment framework"))
