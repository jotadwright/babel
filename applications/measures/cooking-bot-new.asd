(in-package :asdf)

(defsystem :cooking-bot-new
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
               #+lispworks :drakma)
  :serial t
  :components ((:file "package")
               (:file "ontology")
               (:file "primitives")
               (:file "visualisations")
               (:module measures
                :serial t
                :components ((:file "nodenames")
                             (:file "ontology")
                             (:file "grammar")
                             (:file "discourse")
                             (:file "mental-simulation")))
               (:module personal-dynamic-memory
                :serial t
                :components ((:file "personal-dynamic-memory")
                             (:file "initial-kitchen-state")))
               (:module language-processing
                :serial t
                :components ((:file "de-render")
                             (:file "understand")
                             (:file "expansions")
                             (:file "utils")
                             (:file "heuristics")))
               (:module grammars
                :serial t
                :components ((:file "almond-cookies-grammar")))
               (:file "utils")))
