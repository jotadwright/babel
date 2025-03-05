(in-package :asdf)

(defsystem :distributional-fcg
  :depends-on (:fcg :nlp-tools)
  :description "Integrating distributional information in FCG."
  :serial t
  :components ((:file "de-render")
               (:file "procedural-attachment")
               (:file "heuristics")
               (:file "utils")
               (:file "distributional-propbank-utils")
               (:file "cxn-supplier")
               (:file "learning-with-distributions")
               ))