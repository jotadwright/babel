(in-package :asdf)

(defsystem #:clevr-grammar-learning
  :description "Tutor-learner experiment to learn the clevr grammar"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :experiment-framework
               :plot-raw-data
               :monitors
               :web-interface
               :tasks-and-processes
               :meta-layer-learning
               :irl
               :fcg
               :clevr-world
               :cl-json)
  :serial t
  :components ((:file "package")
               (:module "clevr-primitives"
                :serial t
                :components ((:file "primitive-inventory")
                             (:file "get-context")
                             (:file "filter")
                             (:file "query")
                             (:file "same")
                             (:file "equal")
                             (:file "count")
                             (:file "exist")
                             (:file "unique")
                             (:file "relate")
                             (:file "intersect")
                             (:file "union")
                             (:file "equal-less-greater")))
               (:module "clevr-grammar"
                :serial t
                :components ((:module "fcg-utils"
                              :serial t
                              :components ((:file "cxn-supplier")
                                           (:file "de-render")
                                           (:file "node-test")))
                             (:file "lex-and-morph")
                             (:file "rpn-meaning")
                             (:file "grammar")
                             (:file "nominal")
                             (:file "query")
                             (:file "relate")
                             (:file "count")
                             (:file "exist")
                             (:file "multi-hop")
                             (:file "single-and")
                             (:file "single-or")
                             (:file "compare-integer")
                             (:file "same-relate")
                             (:file "comparison")
                             (:file "preprocess-utterance")))
               (:module "grammar-learning"
                :serial t
                :components ((:file "fcg-utils")
                             (:file "goal-tests")
                             (:file "problems-diagnostics")
                             (:file "utils")
                             (:file "web-monitors")))
               (:module "experiment"
                :serial t
                :components ((:file "agent")
                             (:file "alignment")
                             (:file "experiment")
                             (:file "interaction")))
               (:module "irpf"
                :serial t
                :components ((:module "diagnostics"
                              :serial t
                              :components ((:file "diagnostic-failed")
                                           (:file "diagnostic-unknown")
                                           (:file "diagnostic-partial")
                                           (:file "diagnostic-meaning")))
                             (:file "composer")
                             (:file "fcg-utils")
                             (:file "goal-tests")
                             (:file "grammar")
                             (:file "utils")
                             
                             (:file "utils3")))
               (:module "repairs"
                :serial t
                :components ((:file "clevr-learning-repair")
                             (:file "add-holophrase")
                             (:file "holophrase-to-item-based--substitution")
                             (:file "holophrase-to-item-based--addition")
                             (:file "holophrase-to-item-based--deletion")
                             (:file "item-based-to-lexical")
                             (:file "lexical-to-item-based")
                             (:file "add-th-links")
                             (:file "add-th-links-formulation")))               
               (:module "utils"
                :serial t
                :components ((:file "utils")
                             (:file "processes")
                             (:file "tasks")
                             (:file "ontology")))
               (:module "monitoring"
                :serial t
                :components ((:file "web-monitors")
                             (:file "monitors")
                             (:file "lisp-monitors")
                             (:file "plotting")))))
