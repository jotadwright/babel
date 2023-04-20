(defsystem duckie-language-learning
  :depends-on ("utils"
               "monitors"
               "plot-raw-data"
               "web-interface"
               "fcg"
               "irl"
               "meta-layer-learning"
               "cl-change-case"
               "experiment-framework"
               "test-framework"
               "plot-raw-data"
               "cl-json"
               "jonathan"
               "dexador")
  :serial t
  :components ((:file "package")
               (:module experiment-setup
                :serial t
                :components ((:file "agent")
                             (:file "experiment")
                             (:file "interaction")))
               (:module utils
                :serial t
                :components ((:file "web-monitors")
                             (:file "unique-utils")
                             (:file "gl-utils")
                             (:file "fcg-utils")))
               (:module irpf
                :serial t
                :components ((:file "handle-fix")
                             (:file "diagnostic-unknown")
                             (:file "diagnostic-partial")
                             (:file "diagnostic-failure")
                             (:file "composer")))
               (:module repairs
                :serial t
                :components ((:file "add-holophrase")
                             (:file "add-categorial-links")
                             (:file "holophrase->item-based--substitution")
                             (:file "holophrase->item-based--addition")
                             (:file "holophrase->item-based--deletion")
                             (:file "item-based->lexical")
                             (:file "lexical->item-based")
                             (:file "grammar")
                             (:file "utils")))
                
               (:module ontology
                :serial t
                :components ((:file "category")
                             (:file "ontology")
                             (:module object
                              :serial t
                              :components ((:file "object")
                                           (:file "object-set")
                                           (:file "building")
                                           (:file "car")
                                           (:file "home")))
                             (:file "utils")))
               (:module primitives
                :serial t
                :components ((:file "duckie-primitives")
                             (:file "simulation-primitives")
                             (:file "world-primitives")))
               )             
  :description "Duckiebot demonstration of language acquisition through intention reading and pattern finding")
