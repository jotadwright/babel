(ql:quickload :pattern-finding)
(in-package :pf)

;; activate monitors
(progn
  (monitors::deactivate-all-monitors)
  (monitors::activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-interactions-in-wi-verbose))

(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  )

;; make experiment
;; data = all ingredient lists of all recipes
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:number-of-epochs . 20)
                              (:repair-recursively . nil)
                              (:alignment-strategy . :lateral-inhibition)
                              (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                              (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))))))

(length (corpus *experiment*))

(run-interaction *experiment*)
(run-series *experiment* 263)
(run-series *experiment* (length (corpus *experiment*)))

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory* :sort-by-type-and-score t))
(add-element (make-html (categorial-network *cxn-inventory*)))

;;;; Time travel

(go-back-n-interactions *experiment* 1)
(remove-cxns-learned-at *experiment* 16)

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
          do (delete-cxn-and-grammatical-categories cxn grammar))))

;;;; Run experiments

(defun run-training ()
  (wi::reset)
  (run-experiments `(
                     (default-configurations-non-recursive
                      ((:experiment-name . default-configurations-non-recursive)))                     
                     (au-exhaustive-pa-heuristic-non-recursive
                      ((:anti-unification-mode . :exhaustive)
                       (:partial-analysis-mode . :heuristic)
                       (:experiment-name . au-exhaustive-pa-heuristic-non-recursive)))
                     (au-heuristic-pa-exhaustive-non-recursive
                      ((:anti-unification-mode . :heuristic)
                       (:partial-analysis-mode . :exhaustive)
                       (:experiment-name . au-heuristic-pa-exhaustive-non-recursive)))
                     (all-heuristic-mode-non-recursive
                      ((:anti-unification-mode . :heuristic)
                       (:partial-analysis-mode . :heuristic)
                       (:experiment-name . all-heuristic-mode-non-recursive)))
                     )
                   :shared-configuration `((:comprehend-all-n . 2)
                                           (:shuffle-data-p . nil)
                                           (:number-of-epochs . 5)
                                           (:repair-recursively . nil)
                                           (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                                           (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))
                                           (:output-dir . ,(babel-pathname :directory '("systems" "pattern-finding" "raw-data"))))
                   :number-of-interactions 1315
                   :number-of-series 1
                   :monitors (append '("print-a-dot-for-each-interaction"
                                       "summarize-results-after-n-interactions")
                                     (get-all-lisp-monitors)
                                     (get-all-export-monitors))))
;(run-training)


(create-graph-for-single-strategy
 :experiment-name "default-configurations-non-recursive-mre"
 :measure-names '("communicative-success" "grammar-size")
 :y-axis '(1 2) :y1-max 1
 :xlabel "Number of observations"
 :y1-label "Communicative Success"
 :y2-label "Grammar Size"
 :captions '("communicative success" "grammar size")
 :open nil)