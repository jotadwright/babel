(in-package :asdf)

(defsystem :seq2seq-heuristics
  :author "EHAI Team"
  :maintainer "EHAI Team"
  :license "Not to be distributed for now."
  :homepage "https://gitlab.ai.vub.ac.be/ehai/seq2seq-heuristics"
  :depends-on (:fcg :cl-json  #+lispworks drakma #-lispworks dexador)
  :description "A subsystem for using sequence-to-sequence heuristics in Fluid Construction Grammar"
  :serial t
  :components ((:file "next-cxn")
               (:file "cxn-supplier")
               (:file "cip-priority")))
