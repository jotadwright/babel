(in-package :asdf)

(defsystem :geoquery-lsfb-grammar-copy
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
               (:file "depicting-signs")
               (:file "pointing-signs")
               (:file "stateid-cxns")
               (:file "state-cxns")
               ;(:file "answer-cxns")
               ;(:file "state-next-to-cxns")
               ;(:file "stateid-cxns")
               ;(:file "highest-point-cxns")
               ;(:file "city-cxns")
               ;(:file "largest-cxns")
               ;(:file "sum-cxns")
               ;(:file "most-cxns")
               ;(:file "state-cxns")
               ;(:file "river-cxns")
               ;(:file "population-cxns")
               ;(:file "countryid-cxns")
               ;(:file "shortest-cxns")
               ;(:file "major-cxns")
               ;(:file "longest-cxns")
               ;(:file "smallest-cxns")
               ;(:file "capital-cxns")
               ;(:file "lowest-cxns")
               (:file "evaluation")))