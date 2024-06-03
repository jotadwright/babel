(ql:quickload :au-benchmark)
(in-package :au-benchmark)

(run-anti-unification-benchmark
 (make-pathname :directory '(:absolute "Users" "jensnevens" "quicklisp" "local-projects"
                             "k-swap-anti-unification" "code" "data")
                :name "clevr-mini" :type "lisp")
 '(:baseline_msg
   :exhaustive_msg
   :kswap_msg_omega))