(in-package :asdf)

(defsystem :fcg-learn
  :description "A Babel package for learning construction grammars through anti-unification."
  :depends-on (:amr :fcg :irl :predicate-networks :au-benchmark)
  :serial t
  :components ((:file "classes")
               (:file "corpus-processor")
               (:file "monitors")
               (:file "routine-processing")
               (:file "meta-processing")
               (:file "pattern-finding")))
               
