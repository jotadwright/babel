(in-package #:asdf)

(defsystem :frame-extractor
  :depends-on (:closer-mop
               :pie
               :cl-ppcre
               :xmls
               :nlp-tools
               ;;:type-hierarchies
               :tasks-and-processes
               :corpus-processing 
               :cl-json
               :snooze
               :cl-mop
               :trivial-timeout)
  :version "4.0"
  :author "Katrien Beuls, Paul Van Eecke and Vanja Cangalovic"
  :description "A tool for extracting semantic frames from text."
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "frame-definitions")
               (:file "dependencies")
               (:file "configuration")
               (:file "conll-evaluation")
               (:file "evaluate-guardian-annotations")
               (:file "twitter-causation")
               (:module "web-service"
                :serial t
                :components ((:file "web-service")))))