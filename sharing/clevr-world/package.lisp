(in-package :cl-user)

(defpackage :clevr-world
  (:nicknames :cw)
  (:use :common-lisp
        :utils
        #+:hunchentoot-available-on-this-platform :web-interface
        :irl
        :cl-jonathan)
  (:documentation "A package that provides an interface to the CLEVR dataset"))
