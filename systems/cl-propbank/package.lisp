(in-package :cl-user)

(defpackage :cl-propbank
  (:use :common-lisp :cl-user)
  (:export "*pb-data* load-pb-data")
  (:documentation "A Common Lisp interface to PropBank"))

(pushnew :cl-propbank *features*)
