(in-package :cl-user)

(defpackage :propbank-english
  (:documentation "A large propbank-based construction grammar for English")
  (:shadowing-import-from :cl-propbank :id)
  (:use :common-lisp :cl-user :utils :monitors :fcg :irl :web-interface :cl-propbank)
  (:shadow ))
