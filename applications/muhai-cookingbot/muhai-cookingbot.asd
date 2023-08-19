(in-package :asdf)

(defsystem :muhai-cookingbot
  :description "A project for building a cookingbot that can execute natural language recipes in a kitchen simulator."
  :depends-on (:s-dot
               :utils
               :monitors
               :web-interface
               :irl
               :fcg
               :cl-json
               :closer-mop
               :nlp-tools
               :dexador
               :yason)
  :serial t
  :components ((:file "package")
               (:file "parse-yaml")
               (:file "ontology-generated")
               (:file "vr-primitives")
               (:file "primitives")
               (:file "vr-requests")
               (:file "visualisations")
               (:module personal-dynamic-memory
                :serial t
                :components ((:file "personal-dynamic-memory")))
               (:module language-processing
                :serial t
                :components ((:file "de-render")
                             (:file "understand")
                             (:file "expansions")
                             (:file "utils")
                             (:file "heuristics")))
               (:module evaluation
                :serial t
                :components ((:file "irl-helpers")
                             (:file "helpers")
                             (:file "solutions")
                             (:file "environments")
                             (:file "evaluate")))))
