(in-package :cl-user)

(defpackage :aipp-cookingbot
  (:use :cl-user
        :common-lisp
        :utils
        :monitors
        :web-interface
        :irl
        :fcg
        :cl-json)
  (:shadow "PROTOTYPE" "PP")
  (:documentation "Your very own AIPP Project"))
