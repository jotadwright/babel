(in-package :asdf)

(defsystem :cl-framenet
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license "To be determined"
  :homepage "https://gitlab.ai.vub.ac.be/ehai/cl-framenet"
  :depends-on (:cl-store :xmls)
  :description "A Common Lisp interface to FrameNet."
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "configuration")
               (:file "load-fn-data")))
