(in-package :cl-user)

(defpackage :cl-framenet
  (:use :common-lisp :cl-user)
 ; (:shadow ())
  (:export "*fn-data*")
  (:documentation "A Common Lisp interface to Framenet."))

(pushnew :cl-framenet *features*)
