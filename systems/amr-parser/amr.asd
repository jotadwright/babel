(defsystem amr
  :author "Paul Van Eecke & Katrien Beuls"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license ""
  :homepage "https://gitlab.ai.vub.ac.be/ehai/amr"
  :serial t
  :depends-on (:utils :fcg)
  :components ((:file "package")
               (:file "amr")
               (:file "equivalent-amr-networks"))
  :description "A Common Lisp package for manipulating AMR meaning representations.")
