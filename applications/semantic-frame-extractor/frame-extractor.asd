(in-package #:asdf)

(defsystem :frame-extractor
  :depends-on (:closer-mop
               :pie
               :xmls
               :nlp-tools
               ;;:type-hierarchies
               :tasks-and-processes
              ;; :fcg-hybrids
               :corpus-processing 
               :cl-json
               :snooze
               :cl-mop)
  :version "3.2.0"
  :author "Katrien Beuls"
  :description "A tool for extracting semantic frames from text."
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "frame-definitions")
               (:file "cxn-processing")
               (:file "dependencies")
               (:file "configuration")
               (:module "web-service"
                :serial t
                :components ((:file "web-service")))
              ; (:file "tests")
               ))