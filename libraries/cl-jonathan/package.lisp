
(in-package :common-lisp)

(defpackage :cl-jonathan
  (:use :common-lisp :cl-user :jonathan)
  (:import-from :cl-ppcre :regex-replace-all)
  (:import-from :utils :list-of-strings->string
                       :make-kw :upcase)
  (:documentation "cl-jonathan provides the ease of use of cl-json, with the speed with jonathan"))
