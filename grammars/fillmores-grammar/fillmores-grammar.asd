(defsystem fillmores-grammar
  :author "Alexane Jouglar"
  :maintainer "EHAI <ehai@ai.vub.ac.be>"
  :license ""
  :serial t
  :depends-on (:utils :fcg :amr :irl)
  :components ((:file "package")
               (:file "parameters")
               (:file "grammatical-cxns")
               (:file "lexical-cxns"))
  :description "A grammar implementing all example of constructions given by C.J. Fillmore in his article from 1988.")
