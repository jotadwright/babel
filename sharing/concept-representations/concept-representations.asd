(in-package :asdf)

(defsystem :concept-representations
  :description "System for representing, updating and comparing concepts (with entities)."
  :author "Hermes I Research Team"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
  :depends-on (:utils 
               :test-framework
               :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "entity")
               (:file "concept")
               (:file "distribution")
               (:file "similarity")
               (:file "update")
               ;(:file "visualisation")
               ))

;; idea for modules: 1. representing, 2 visualising, 3 comparing, 4 updating