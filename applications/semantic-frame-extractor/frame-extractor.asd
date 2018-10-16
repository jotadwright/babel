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
  :description "A hybrid English grammar with frames"
  :long-description "New version of the BENG for Odycceus project, includes PIE testing and hybrids"
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