(ql:quickload :grammar-learning)
(in-package :grammar-learning)



(progn
  (deactivate-all-monitors)
  ;(activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'grammar-learning-experiment
                   :entries `((:observation-sample-mode . :sort-length-ascending)
                              (:meaning-representation . :amr)
                              (:de-render-mode . :de-render-string-meets-no-punct)
                              (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "bio-corpus-amr"))
                                     cl-user:*babel-corpora*))
                              (:corpus-data-file . ,(make-pathname :directory '(:relative "pre-processed")
                                                   :name "bio-corpus-amr" :type "json"))))))

;(run-interaction *experiment*)
;(comprehend "Ah !" :cxn-inventory (grammar (first (agents *experiment*))))
;(run-series *experiment* 6952)

;(add-element (make-html (grammar (first (agents *experiment*)))))