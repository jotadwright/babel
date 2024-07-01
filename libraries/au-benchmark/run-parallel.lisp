(ql:quickload :au-benchmark)
(in-package :au-benchmark)

(run-anti-unification-benchmark-in-parallel
 (make-pathname :directory '(:absolute "Users" "Shared" "Projects" "quicklisp" "local-projects"
                             "k-swap-anti-unification" "code" "data")
                :name "clevr-5000-stage-1" :type "lisp")
 '(:exhaustive_msg :kswap_msg_omega) 5 
 :timeout 600 :k 1 :W 1
 :temp-dir
 (make-pathname :directory '(:absolute "Users" "Shared" "Projects" "quicklisp" "local-projects"
                             "k-swap-anti-unification" "code" ".tmp" "clevr-stage-1")))
