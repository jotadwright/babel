(in-package :asdf)

(defsystem #:au-benchmark
  :description "Running benchmarks on anti-unification algorithms"
  :author "EHAI <ehai@ai.vub.ac.be>"
  :maintainer "Jens Nevens <jens@ai.vub.ac.be>"
  :license "Apache 2.0 License"
  :depends-on (:cl-pcp
               :trivial-timeout
               :clingon
               :split-sequence
               :alexandria)
  :serial t
  :components ((:file "packages")
               (:module utils
                :serial t
                :components ((:file "lists-and-sets")
                             (:file "symbols-and-strings")
                             (:file "math")
                             (:file "queue")
                             (:file "variables")
                             (:file "bindings")))
               (:file "classes")
               (:file "cost")
               (:file "anti-unify-predicate-sequence")
               (:module algorithms
                :serial t
                :components ((:module lcg
                              :serial t
                              :components ((:file "packages")
                                           (:file "lcg-utils")
                                           (:file "baseline")
                                           (:file "exhaustive")
                                           (:file "k-swap")))
                             (:module msg
                              :serial t
                              :components ((:file "packages")
                                           (:file "msg-utils")
                                           (:file "baseline")
                                           (:file "exhaustive")
                                           (:file "k-swap-heuristic")
                                           (:file "1-swap-omega")
                                           (:file "k-swap-omega")))))
               (:module benchmark
                :serial t
                :components ((:file "generate-benchmark")
                             (:file "run-benchmark")))
               (:file "cli")))
