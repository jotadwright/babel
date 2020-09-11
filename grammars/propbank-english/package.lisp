(in-package :cl-user)

(defpackage :propbank-english
  (:documentation "A large propbank-based construction grammar for English")
  (:shadowing-import-from :cl-propbank :id)
  (:use :common-lisp :cl-user :utils :monitors :fcg :web-interface :cl-propbank :cl-store)
  (:shadow ))
