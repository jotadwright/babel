(defsystem printer-interface
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license ""
  :homepage "https://gitlab.ai.vub.ac.be/ehai/printer-interface"
  :depends-on (:uiop :split-sequence)
  :serial t
  :components ((:file "package")
               (:file "printer-interface"))
  :description "An implementation-independent Common Lisp package for
interface for printing documents using UNIX lpr. See 'man lpr' for
more information on the options and their possible values.")