(in-package :asdf)

(defsystem :fillmores-grammar
  :description "Testing out Fillmore's cxns from 1988 paper."
  :author "Alexane Jouglar <ehai@ai.vub.ac.be>"
  :maintainer "Alexane Jouglar <ehai@ai.vub.ac.be>"
  :license "To be determined."
  :depends-on (:utils :fcg)
  :serial t
  :components ((:file "package")

               (:module cxns
                :serial t
                :components ((:file "lexical-items")
                             (:file "grammatical-cxns")))))