(in-package :asdf)

(defsystem :pf-for-sql
  :description "Pattern finding for SQL"
  :author "Alexane Jouglar"
  :maintainer "Alexane Jouglar"
  :license "Babel Research License"
  :serial t
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
               :cl-json
               :com.inuoe.jzon)
  :components ((:file "package")
               (:file "utils")
               (:file "experiment")
               (:file "web-monitors")
               (:module repairs-and-diagnostics
                :serial t
                :components ((:file "diagnostics")
                             (:file "add-holophrase")
                             (:file "add-item-based"))))
  :description "Pattern finding for SQL.")

;; :irl :com.inuoe.jzon :web-interface
