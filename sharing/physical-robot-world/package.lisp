
(in-package :cl-user)

(defpackage :physical-robot-world
  (:use :common-lisp
        :utils
        :monitors
        #+:hunchentoot-available-on-this-platform :web-interface)
  (:documentation "An interface to the robotdata repository"))
