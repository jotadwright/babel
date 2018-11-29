
(in-package :cl-user)

(defpackage :robot-interface
  (:use :common-lisp
        :test-framework
        :nao-interface)
  (:import-from :utils
                :babel-pathname :deg-to-rad :run-prog)
  (:documentation "Interface between Babel2 and physical robots."))
