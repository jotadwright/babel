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
               (:file "countryid-cxns")
               (:file "largest-cxns")
               (:file "cityid-cxns")
               (:file "evaluation")))