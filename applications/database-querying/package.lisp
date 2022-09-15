
(in-package :cl-user)

(defpackage :database-querying
  (:use :common-lisp
   :utils :monitors :fcg
   #+:hunchentoot-available-on-this-platform :web-interface)
  (:documentation "A package for querying databases using FCG."))
