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
                "grammar-learning"
                "dexador")
  :serial t
  :components ((:file "package")
               (:module experiment-setup
                :serial t
                :components ((:file "run-helpers")
                             (:file "agent")
                             (:file "experiment")
                             (:file "interaction")))
                (:module irpf
                :serial t
                :components ((:file "handle-fix")
                             (:file "diagnostics-and-repairs")
                             (:file "composer")))
                (:module repairs
                 :serial t
                 :components ((:file "add-holophrase")
                              (:file "holophrase->item-based--substitution")
                              (:file "holophrase->item-based--addition")
                              (:file "holophrase->item-based--deletion")
                              (:file "grammar")))
                
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
                 :components ((:file "simulation-primitives")
                              (:file "duckie-primitives")
                              (:file "world-primitives")))
                (:module utils
                 :serial t
                 :components ((:file "fcg-utils")
                              (:file "utils"))))             
  :description "Duckiebot demonstration of language acquisition through intention reading and pattern finding")
