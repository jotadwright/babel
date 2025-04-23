(ql:quickload :clevr-grammar-learning)
(in-package :cgl)

;; --------------------------------
;; + SCRIPT FOR BATCH EXPERIMENTS +
;; --------------------------------

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors (get-all-lisp-monitors)))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'clevr-learning-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :named-configurations strategies
    :shared-configuration nil
    :monitors monitors
    :output-dir (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(run-experiments '(
                   (formulation-all-composer-solutions-smart
                    ((:determine-interacting-agents-mode . :default)
                     (:question-sample-mode . :all)    
                     (:scenes-per-question . 50)
                     (:confidence-threshold . 1.1)
                     (:tutor-sample-mode . :smart)
                     (:cxn-decf-score . 0.3)
                     (:cxn-inhibit-score . 0.1)
                     (:primitives . :symbolic)
                     (:learner-cxn-supplier . :hashed-and-scored)
                     (:alignment-strategy . :lateral-inhibition)
                     (:hide-type-hierarchy . t)))
                   )
                 :number-of-interactions 100000
                 :number-of-series 1
                 :monitors (append (get-all-lisp-monitors)
                                   (list "print-a-dot-for-each-interaction")))

(run-experiments '(
                   (test 
                    ((:determine-interacting-agents-mode . :default)
                     (:question-sample-mode . :all)
                     ;(:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:confidence-threshold . 1.1)
                     (:tutor-sample-mode . :random)
                     (:cxn-decf-score . 0.3)
                     (:primitives . :symbolic)
                     (:learner-cxn-supplier . :hashed-and-scored)
                     (:alignment-strategy . :lateral-inhibition)
                     (:hide-type-hierarchy . t)))
                   )
                 :number-of-interactions 50000
                 :number-of-series 1
                 :monitors (append (get-all-lisp-monitors)
                                   (get-all-export-monitors)
                                   (list "print-a-dot-for-each-interaction"
                                         "display-metrics")))



