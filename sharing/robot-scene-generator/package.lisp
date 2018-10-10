(in-package :cl-user)

(defpackage :scene-generator
  (:use :common-lisp
        :utils
        :hunchentoot
        :cl-who
        :ht-simple-ajax)
  (:documentation "Generate random scenes on a web page"))