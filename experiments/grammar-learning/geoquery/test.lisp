
(ql:quickload :grammar-learning)
(in-package :grammar-learning)


;(setf *raise-errors* nil)
;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; minimal logging after 100 interactions with type hierarchy
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; minimal logging after 100 interactions
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction)
  ;(activate-monitor export-categorial-network-evolution-to-jsonl)
  (activate-monitor export-type-hierarchy-to-json))



;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; sparse logging, no trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions))

(defun create-experiment ()
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (eval `(make-instance 'grammar-learning-experiment
                          :entries '((:repairs . (add-categorial-links
                                                  ;item-based->item-based--substitution
                                                  item-based->holistic
                                                  holistic->item-based--substitution
                                                  holistic->item-based--addition
                                                  holistic->item-based--deletion
                                                  holistic->item-based
                                                  nothing->holistic))
                                     (:observation-sample-mode . :train)
                                     (:number-of-epochs . 20)
                                     (:comprehend-n . 20)
                                     (:meaning-representation . :geo)
                                     (:cxn-decf-score . 0.1)
                                     (:cxn-incf-score . 0.3)
                                     (:mark-holophrases . t)
                                     (:remove-cxn-on-lower-bound . t)
                                     (:alignment-strategy . :lateral-inhibition)
                                     (:de-render-mode . :de-render-string-meets-no-punct)
                                     (:corpus-files-root . ,(merge-pathnames
                                                             (make-pathname :directory '(:relative "geoquery"))
                                                             cl-user:*babel-corpora*))
                                     (:corpus-data-file . ,(make-pathname
                                                            :name "geoquery_en" :type "jsonl")))))))

(defun create-lexicon-experiment ()
  (wi::reset)
  (notify reset-monitors)
  (defparameter *lex-experiment*
    (eval `(make-instance 'grammar-learning-experiment
                          :entries '((:repairs . (nothing->holistic))
                                     (:observation-sample-mode . :debug)
                                     (:comprehend-n . 1)
                                     (:mark-holophrases . nil)
                                     (:meaning-representation . :geo)
                                     (:remove-cxn-on-lower-bound . nil)
                                     (:cxn-decf-score . 0.0)
                                     (:cxn-incf-score . 0.0)
                                     (:de-render-mode . :de-render-string-meets-no-punct)
                                     (:corpus-files-root . ,(merge-pathnames
                                                             (make-pathname :directory '(:relative "geoquery"))
                                                             cl-user:*babel-corpora*))
                                     (:corpus-data-file . ,(make-pathname
                                                            :name "lexicon_geoquery_en" :type "jsonl")))))))

(defun initialize-experiment-with-lexicon ()
  (create-experiment)
  (create-lexicon-experiment)
  (deactivate-all-monitors)
  (run-series *lex-experiment* (length (question-data *lex-experiment*)))
  (loop for cxn in (constructions (grammar (first (agents *lex-experiment*))))
        do (add-cxn cxn (grammar (first (agents *experiment*)))))
  ;(activate-monitor display-metrics)
  ;(activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions))
  ;(activate-monitor trace-interactions-in-wi))

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-geo" :type "store"))
;(cl-store:store *experiment* (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "experiment-train-geo" :type "store"))

;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights? t :render-program "circo"))
;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights t :render-program "fdp"))
;(add-element (make-html (grammar (first (agents *experiment*))) :sort-by-type-and-score t :routine-only t))

;(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))

;(defparameter *th* (categorial-network (grammar (first (interacting-agents *experiment*)))))

(initialize-experiment-with-lexicon)

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
(run-series *experiment* (length (question-data *experiment*)))
;(run-series *experiment* 38)



#|
;; run 880, remove holophrases, run 880 of series 2
(run-series *experiment* 880)
(remove-holophrases
 (grammar (first (agents *experiment*))))
(run-series *experiment* 880)
 |#