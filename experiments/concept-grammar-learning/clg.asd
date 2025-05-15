(in-package :asdf)

(defsystem :clg
  :description "Tutor-learner experiment to learn the clevr grammar with concepts"
  :author "Hermes I Research Team"
  :maintainer "Jerome Botoko Ekila <jerome@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :experiment-framework
               :plot-raw-data
               :monitors
               :web-interface
               :tasks-and-processes
               :meta-layer-learning
               :concept-representations
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
                :components ((:file "lex-and-morph")
                             (:file "grammar")
                             (:file "rpn-meaning")
                             (:file "preprocess-utterance")
                             (:module "fcg-utils"
                              :serial t
                              :components ((:file "cxn-supplier")
                                           (:file "de-render")
                                           (:file "node-test")))
                             (:module "constructions"
                              :serial t
                              :components ((:file "compare-integer")
                                           (:file "comparison")
                                           (:file "count")
                                           (:file "exist")
                                           
                                           (:file "multi-hop")
                                           (:file "nominal")
                                           (:file "query")
                                           (:file "relate")
                                           (:file "same-relate")
                                           (:file "single-and")
                                           (:file "single-or")))
                             ))
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
                             (:file "competitors")
                             (:file "experiment")
                             (:file "interaction")
                             (:file "ontology")
                             (:file "score")))
               (:module "irpf"
                :serial t
                :components ((:file "handle-fix")
                             (:module "diagnostics"
                              :serial t
                              :components ((:file "diagnostic-failed")
                                           (:file "diagnostic-unknown")
                                           (:file "diagnostic-partial")
                                           (:file "diagnostic-meaning")))
                             (:module "repairs"
                              :serial t
                              :components (
                                           (:file "add-holophrase")
                                           (:file "holophrase-to-item-based--substitution")
                                           (:file "holophrase-to-item-based--addition")
                                           (:file "holophrase-to-item-based--deletion")
                                           (:file "item-based-to-lexical")
                                           (:file "lexical-to-item-based")
                                           (:file "add-th-links")
                                           (:file "add-th-links-formulation")
                                           (:file "update-concept")))  
                             (:file "composer")
                             (:file "goal-tests")
                             (:file "grammar")
                             (:file "utils")
                             (:file "chunking")))
               (:module "utils"
                :serial t
                :components ((:file "fcg-utils")
                             (:file "utils")
                             (:file "logging")
                             (:file "questions")
                             (:file "loading")
                             (:file "processes")
                             (:file "tasks")))
               (:module "monitoring"
                :serial t
                :components ((:file "web-monitors")
                             (:file "monitors")
                             (:file "lisp-monitors")))))
