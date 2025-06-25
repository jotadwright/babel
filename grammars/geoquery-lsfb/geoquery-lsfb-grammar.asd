(in-package :asdf)

(defsystem :geoquery-lsfb-grammar
  :description "A package for the geoquery-lsfb grammar"
  :maintainer "Liesbet De Vos"
  :depends-on (:utils
               :web-interface
               :monitors
               :irl
               :fcg
               :slp)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "grammar-configurations")
               (:file "answer-cxns")
               (:file "state-next_to-cxns")
               (:file "stateid-cxns")
               (:file "haut-cxns")
               (:file "categorial-network")
               (:file "evaluation")))