(in-package :asdf)

(defsystem :cl-propbank
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license "To be determined"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/cl-propbank"
  :depends-on (:cl-store :xmls :cl-ppcre)
  :description "A Common Lisp interface to PropBank."
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "configuration")
               (:file "propbank-data")
               (:file "predicate")
               (:file "roleset")
               (:file "alias")
               (:file "role")
               (:file "load-pb-data")))
