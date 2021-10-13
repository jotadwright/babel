(in-package :asdf)

(defsystem :aipp-cookingbot
  :description "A project for building a cookingbot that can execute natural language recipes in a kitchen simulator."
  :depends-on (:s-dot
               :utils
               :monitors
               :web-interface
               :irl
               :fcg
               :cl-json
               #+lispworks :drakma)
  :serial t
  :components ((:file "package")
               (:file "cooking-macros")
               (:file "ontology")
               (:file "primitives")
              ; (:file "vr-simulator")
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
                             (:file "heuristics")
                             (:file "grammar")))))
