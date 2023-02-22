(ql:quickload :pattern-finding)
(in-package :pattern-finding)


(progn
  (deactivate-all-monitors)
  ;(activate-monitor display-metrics)
  (monitors::activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi)
  )

(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:meaning-representation . :irl) 
                              (:corpus-files-root . ,(merge-pathnames
                                                      (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                                      cl-user:*babel-corpora*))
                              (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                                   :name "stage-1" :type "jsonl"))
                              (:number-of-samples . nil)
                              (:shuffle-data-p . t)
                              (:sort-data-p . nil)
                              (:remove-duplicate-data-p . t)))))

(length (question-data *experiment*))
(run-series *experiment* 100)

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory*))-
(add-element (make-html (categorial-network (grammar (first (agents *experiment*))))))

(run-interaction *experiment*)
(loop repeat 800 do (run-interaction *experiment*))
(go-back-n-interactions *experiment* 1)
(remove-cxns-learned-at *experiment* 6)

(comprehend-all "Are any spheres visible?"
                :cxn-inventory *cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set1 ?context ?shape1)
                                         (bind shape-category ?shape1 sphere)
                                         (exist ?answer ?set1)))

(defun go-back-n-interactions (experiment n)
  (setf (interactions experiment)
        (subseq (interactions experiment) n)))

(defun remove-cxns-learned-at (experiment at)
  (let ((learned-at-cxns
         (find-all-if #'(lambda (cxn)
                          (string= (format nil "@~a" at)
                                   (attr-val cxn :learned-at)))
                      (constructions (grammar (learner experiment))))))
    (loop with grammar = (grammar (learner experiment))
          for cxn in learned-at-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn grammar)
          do (delete-cxn (name cxn) grammar :key #'name)
             (delete-cxn (name alter-ego-cxn) grammar :key #'name))))
