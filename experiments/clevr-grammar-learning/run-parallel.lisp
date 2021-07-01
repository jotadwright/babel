(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(let ((experiment-names
       '(training-10-series)))
  (run-parallel-batch-for-different-configurations 
   :asdf-system "clevr-grammar-learning"
   :package "clevr-grammar-learning"
   :experiment-class "clevr-grammar-learning-experiment"
   :number-of-interactions 47133
   :number-of-series 10
   :monitors (append (get-all-export-monitors)
                     (get-all-lisp-monitors)
                     (get-all-csv-monitors))
   :configurations '(
                     (training-10-series
                      ((:learner-th-connected-mode . :neighbours)
                       (:current-challenge-level . 1)
                       (:number-of-epochs . 1)
                       ))
                     )
   :shared-configuration '((:determine-interacting-agents-mode . :corpus-learner)
                           (:observation-sample-mode . :train)
                           (:run-mode :training))
   :output-dir (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data"))
   :heap-size 12288))