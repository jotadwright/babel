(in-package :asdf)

(defsystem :origins-of-syntax
  :description ("A system for loading experiments on the origins of syntax.")
  :depends-on (:utils :test-framework :experiment-framework :monitors :plot-raw-data :irl :fcg :category-hierarchies  #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
   ((:file "package")
    (:file "class-definitions")
    (:file "helper-functions")
    (:file "web-monitors")
    (:file "monitors")
    (:file "html")
    (:file "experiment")
    (:file "lexical-strategy")
    (:file "grouping-strategy")
    (:file "n-gram-strategy")
    (:file "categorisation-strategy")
    ;(:file "marking-strategy")
    (:file "anti-unification-categorisation")))
