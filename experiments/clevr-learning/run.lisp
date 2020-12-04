
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; use this file to run experiments (possibly on the cluster)

(run-experiments '(
                   (initial-test ((:questions-per-challenge . 1000)))
                   )
                 :number-of-interactions 10000
                 :number-of-series 1
                 :monitors (list "export-communicative-success"
                                 "export-lexicon-size"
                                 "export-meanings-per-form"
                                 "export-lexicon-change"
                                 "export-avg-cxn-score"
                                 "export-confidence-level"))