(in-package :asdf)

(defsystem #:visual-dialog
  :description "Visual Dialog"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Lara Verheyen <lara.verheyen@ai.vub.ac.be>"
  :license "Babel Research License"
  :depends-on (:utils
               :monitors
               :irl
               :fcg
               :cl-json
               :trivial-timeout
               :clevr-dialog-grammar
               :nao-interface
               :robot-interface)
  :serial t
  :components ((:file "package")
               (:module "ontology"
                :serial t
                :components ((:file "classes")
                             (:file "ontology")
                             (:file "utils")))
               (:module "execution"
                :serial t
                :components ((:file "execution-utils")
                             (:file "initialize-memory")
                             (:file "run-dialogs")
                             (:file "understand-execute-remember")
                             (:file "update-memory")))
               (:module "primitives"
                :serial t
                :components ((:module "both"
                              :serial t
                              :components ((:file "count-objects")
                                           (:file "exist-or-count")
                                           (:file "exists")
                                           (:file "get-last-attribute-category")
                                           (:file "get-last-topic")
                                           (:file "get-penultimate-topic")
                                           (:file "more-than-one")
                                           (:file "select-one")
                                           (:file "unique")))
                             (:module "hybrid"
                              :serial t
                              :components ((:file "extreme-relate")
                                           (:file "filter")
                                           (:file "find-in-context")
                                           (:file "immediate-relate")
                                           (:file "query")
                                           (:file "relate")
                                           (:file "segment-scene")
                                           (:file "set-diff")))
                             (:module "symbolic"
                              :serial t
                              :components ((:file "extreme-relate")
                                           (:file "filter")
                                           (:file "find-in-context")
                                           (:file "immediate-relate")
                                           (:file "query")
                                           (:file "relate")
                                           (:file "segment-scene")
                                           (:file "set-diff")))))
              (:module "evaluation"
                :serial t
                :components ((:file "evaluation")))
               (:file "html")))