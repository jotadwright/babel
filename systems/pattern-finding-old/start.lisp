(ql:quickload :pattern-finding-old)
(in-package :pattern-finding-old)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))


(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


;;;; TO DO

;;;; Handle anti-unification cases where the pattern delta is empty.
;;;; Handle these cases differently from the "regular" cases?
;;;; Do we want the highest cost solution in this case? Do we want the
;;;; solution with the highest scoring cxn in this case?

;;;; top-arg/slot-arg predicates
;;;; Now, grammatical category (a constant) is added as the last argument in these
;;;; predicates. This will (likely) break down when applying repairs recursively.
;;;; However, we need these constants to find out which args on the form side
;;;; and the meaning side belong together. Another way to fix this would be to
;;;; run 'fresh-variables' on the form side and meaning side simultaneously, or
;;;; to keep some variables unchanged...

;;;; Add recursion!


(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:corpus-file . ,(make-pathname :directory '(:relative "val")
                                                              :name "stage-1" :type "jsonl"))))))

(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning"
                                                                                 "cooking" "data")))
                              (:corpus-file . ,(make-pathname :name "benchmark-ingredients-uniform" :type "jsonl"))))))

(length (corpus *experiment*))

;;;; Running interactions             

(run-interaction *experiment*)
(run-series *experiment* 18)

;;;; Showing the cxn inventory and categorial network

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory*))
(add-element (make-html (categorial-network *cxn-inventory*)))

;;;; Manually trying out sentences

(comprehend-all "Is there a blue block?"
                :cxn-inventory *cxn-inventory*
                :gold-standard-meaning '((get-context ?context)
                                         (filter ?set-1 ?context ?shape-1)
                                         (bind shape-category ?shape-1 cube)
                                         (filter ?set-2 ?set-1 ?color-1)
                                         (bind color-category ?color-1 blue)
                                         (exist ?target ?set-2)))

;;;; Time travel

(go-back-n-interactions *experiment* 1)
(remove-cxns-learned-at *experiment* 19)

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


;;;; Changing the order of repairs on the fly

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

(loop for repair in (get-repairs *cxn-inventory*)
      do (delete-repair *cxn-inventory* repair))

(loop for repair-name in '(nothing->holistic
                           anti-unify-partial-analysis 
                           anti-unify-cxn-inventory
                           add-categorial-links)
      do (add-repair *cxn-inventory* repair-name))


