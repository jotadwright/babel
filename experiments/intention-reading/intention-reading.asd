;;;; clevr-learning.asd

(in-package :asdf)

(defsystem #:intention-reading
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
               (:module "primitives"
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
                             (:file "utils")
                             (:file "goal-tests")
                             (:file "problems-diagnostics")
                             (:file "repair-add-categorial-links")
                             (:file "repair-nothing-to-holophrase-cxn")
                             (:file "repair-holophrase-to-item-based+lexical+lexical--substitution")
                             (:file "repair-holophrase-to-item-based+lexical--addition")
                             (:file "repair-holophrase-to-item-based+lexical+holophrase--deletion")
                             (:file "repair-lexical-to-item-based-cxn")
                             (:file "repair-item-based-to-lexical")
                             (:file "monitors")
                             (:file "web-monitors")))
               (:file "utils")
               (:file "goal-tests")
               (:file "grammar")
               (:file "fcg-utils")
               (:file "irl-utils")
               (:file "ontology")
               (:file "composer-utils") 
               (:file "agent")
               (:file "composer")
               (:file "alignment")
               (:file "diagnostics-and-repairs")
               (:module "repairs"
                :serial t
                :components ((:file "clevr-learning-repair")
                             (:file "add-holophrase")
                             (:file "holophrase->item-based--substitution")
                             (:file "holophrase->item-based--addition")
                             (:file "holophrase->item-based--deletion")
                             (:file "item-based->lexical")
                             (:file "lexical->item-based")
                             (:file "add-th-links")
                             (:file "add-th-links-formulation")))
               (:file "processes")
               (:file "tasks")
               (:file "experiment")
               (:file "interaction")
               (:module "monitoring"
                :serial t
                :components ((:file "web-monitors")
                             (:file "monitors")
                             (:file "lisp-monitors")
                             (:file "plotting")))))
