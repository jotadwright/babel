(in-package :asdf)

(defsystem :moral-foundations
  :description "A Babel package for extracting moral foundation categories from texts."
  :author "Remi van Trijp & Lara Verheyen"
  :maintainer "Remi van Trijp & Lara Verheyen"
  :license "To be determined."
  :depends-on (:utils :nlp-tools :cl-store :fcg :irl :fcg-server :monitors :dexador :propbank-grammar)
  :serial t
  :components ((:file "package")
               (:file "global-parameters")
               (:file "moral-foundations-class")
               (:module "utils"
                :serial t
                :components ((:file "porter-stemmer")
                             (:file "load-mf-dictionary")))
               (:module "tests"
                :serial t
                :components ((:file "test-dictionary")))
               (:file "init")))

