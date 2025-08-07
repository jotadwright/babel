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
               (:file "categorial-network")
               (:file "identification-cxns")
               (:file "depicting-signs")
               (:file "pointing-signs")
               (:file "stateid-cxns")
               (:file "state-cxns")
               (:file "countryid-cxns")
               (:file "cityid-cxns")
               (:file "riverid-cxns")
               (:file "capital-cxns")
               (:file "adjective-adverb-cxns")
               (:file "city-cxns")
               (:file "evaluation")))