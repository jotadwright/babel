;;;; /robot-origins-of-syntax.asd

(in-package :asdf)

(defsystem :robot-origins-of-syntax
  :description "Mini Talking Heads + Origins of Syntax experiment on human-robot"
  :depends-on (:utils
               :experiment-framework
               :test-framework
               :monitors
               :plot-raw-data
               :tasks-and-processes
               :meta-layer-learning
               #+:hunchentoot-available-on-this-platform :web-interface
               :irl
               :fcg
               :nao-interface
               :robot-interface
               :scene-generator
               :cl-json
               :closer-mop
               :type-hierarchies)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "color-conversions")
               (:file "world")
               (:file "categories")
               (:file "lexicon")
               (:file "processes")
               (:file "lexical-processes")
               (:file "anti-unification-categorisation")
               (:file "grammatical-processes")
               (:file "agent")
               (:file "experiment")
               (:file "interaction")
               (:file "html")
               (:module "monitors"
                        :serial t
                        :components ((:file "web-monitors")
                                     (:file "trace-monitors")
                                     (:file "metric-monitors")))))
               