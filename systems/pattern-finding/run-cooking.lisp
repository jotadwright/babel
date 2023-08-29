(ql:quickload :pattern-finding)
(in-package :pf)

;; activate monitors
(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

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
                              (:number-of-epochs . 5)
                              (:allow-cxns-with-no-strings . nil)
                              (:alignment-strategy . :lateral-inhibition)
                              (:anti-unification-mode . :exhaustive)
                              (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning"
                                                                                 "cooking" "data")))
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
                     (alignment-li-anti-unification-exhaustive
                      ((:comprehend-all-n . 2)
                       (:shuffle-data-p . nil)
                       (:number-of-epochs . 5)
                       (:allow-cxns-with-no-strings . nil)
                       (:alignment-strategy . :lateral-inhibition)
                       (:anti-unification-mode . :exhaustive)
                       (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                       (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))
                       (:experiment-name . alignment-li-anti-unification-exhaustive)))
                     (alignment-mrg-anti-unification-exhaustive
                      ((:comprehend-all-n . 2)
                       (:shuffle-data-p . nil)
                       (:number-of-epochs . 5)
                       (:allow-cxns-with-no-strings . nil)
                       (:alignment-strategy . :most-recent-generalisation)
                       (:anti-unification-mode . :exhaustive)
                       (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                       (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))
                       (:experiment-name . alignment-mrg-anti-unification-exhaustive)
                       (:output-dir . ,(babel-pathname :directory '("systems" "pattern-finding" "raw-data")))))
                     (alignment-li-anti-unification-heuristic
                      ((:comprehend-all-n . 2)
                       (:shuffle-data-p . nil)
                       (:number-of-epochs . 5)
                       (:allow-cxns-with-no-strings . nil)
                       (:alignment-strategy . :lateral-inhibition)
                       (:anti-unification-mode . :heuristic)
                       (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                       (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))
                       (:experiment-name . alignment-li-anti-unification-heuristic)
                       (:output-dir . ,(babel-pathname :directory '("systems" "pattern-finding" "raw-data")))))
                     (alignment-mrg-anti-unification-heuristic
                      ((:comprehend-all-n . 2)
                       (:shuffle-data-p . nil)
                       (:number-of-epochs . 5)
                       (:allow-cxns-with-no-strings . nil)
                       (:alignment-strategy . :most-recent-generalisation)
                       (:anti-unification-mode . :heuristic)
                       (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning" "cooking" "data")))
                       (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))
                       (:experiment-name . alignment-mrg-anti-unification-heuristic)
                       (:output-dir . ,(babel-pathname :directory '("systems" "pattern-finding" "raw-data")))))
                     )
                   :number-of-interactions 1315
                   :number-of-series 1
                   :monitors (append '("print-a-dot-for-each-interaction"
                                       "summarize-results-after-n-interactions")
                                     (get-all-lisp-monitors)
                                     (get-all-export-monitors))))
;(run-training)
