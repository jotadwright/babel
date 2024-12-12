(ql:quickload :fcg-learn)
(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;; FCG-learn on geoquery dataset ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;---------------------;;
;; Activating monitors ;;
;;---------------------;;
;; (activate-monitor trace-fcg-learning)
;; (deactivate-monitor trace-fcg-learning)
;; (activate-monitor trace-fcg-search-process)
;; (activate-monitor trace-fcg-debugging)
(deactivate-all-monitors)
(activate-monitor trace-fcg-learning-in-output-browser)
(activate-monitor trace-fcg-learning-in-output-browser-verbose)

;;------------------------;;
;; Construction inventory ;;
;;------------------------;;

(def-fcg-constructions geoquery-english-cxn-inventory
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :hashed-with-regex-check)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:hash-mode . :filler-and-linking)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs  repair-add-categorial-link repair-through-anti-unification)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :punish-other-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.5)
                       (:best-solution-mode . :highest-average-link-weight)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:fix-selection-mode . :max-reuse)
                       (:form-generalisation-mode :altschul-erickson
                        ((:match-cost . 0)
                         (:mismatch-cost . 1)
                         (:gap-cost . 1)
                         (:gap-opening-cost . 5)
                         (:n-optimal-alignments . nil)
                         (:max-nr-of-gaps . 1)))
                       (:meaning-generalisation-mode . :exhaustive)
                       (:max-nr-of-nodes . 5000)
                       (:k-swap-k . 1)
                       (:k-swap-w . 1)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence-predicates)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:expand-nodes-in-search-tree . t)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))

;;-----------------------------;;
;; defining path to train file ;;
;;-----------------------------;;

;; train file should be a jsonl that where each line corresponds to one example in the training set. Each example should have an 'utterance' and 'meaning' property. 

(defparameter *geoquery-english-train*
  (merge-pathnames (make-pathname :directory '(:relative "geoquery-english" "train")
                                  :name "geoquery-english-train-250" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;;----------------------;;
;; loading train corpus ;;
;;----------------------;;
; result is corpus-processor object that contains four slots: corpus, counter, current-speech-act and source. Corpus is a list of speech acts, counter is set to 0 at the beginning, current-speech-act is nil and source is the pathname of the source-file. Each speech act contains a form and a meaning of one example.

(defparameter *geoquery-english-train-processor* (load-corpus *geoquery-english-train* :sort-p t :remove-duplicates t :ipa nil))

;;--------------------;;
;; making the grammar ;;
;;--------------------;;

(defparameter *geoquery-english-grammar* (make-geoquery-english-cxn-inventory-cxns))

;; make sure counter is set to 0 at start of learning
(setf (counter *geoquery-english-train-processor*) 0)

;;-----------------------------;;
;; testing on first speech act ;;
;;-----------------------------;;

(comprehend *geoquery-english-train-processor* :cxn-inventory *geoquery-english-grammar*  :nr-of-speech-acts 1)

;; no solution is found and so a problem is diagnosed (gold-standard-not-in-search-space). One fix is proposed: anti-unification-fix which is supplied by 'repair-through-anti-unification-repair'. A holophrastic construction is learned.


;; Setting the attribute-value of :fix to nil for all constructions in the inventory. Why?
(loop for cxn in (constructions-list *geoquery-english-grammar*)
      do (setf (attr-val cxn :fix) nil))

;; define a number of series to run and a number of speech-acts to comprehend in each serie? 
(defun run-speech-acts (from to series cxn-inventory corpus-processor)
  (loop for i from 1 to series
        do (setf (counter corpus-processor) from)
           (comprehend corpus-processor :cxn-inventory cxn-inventory  :nr-of-speech-acts (- to from))))

;; e.g. running one serie of 200 examples (= all training examples for geoquery-250)
;; warning: turn of webmonitors!!
(run-speech-acts 0 200 1 *geoquery-english-grammar* *geoquery-english-train-processor*)

;; comprehending a specific speech act
(comprehend (nth-speech-act *geoquery-english-train-processor* 100)  :cxn-inventory *geoquery-english-grammar*)





