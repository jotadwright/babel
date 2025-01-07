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
                       (:best-solution-mode . :highest-average-entrenchment-score)
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
                                  :name "geoquery-english-train-4500" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defun sort-using-templates (geoquery-corpus)
  

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

;;------------------------------;;
;; testing on single speech act ;;
;;------------------------------;;

(comprehend *geoquery-english-train-processor* :cxn-inventory *geoquery-english-grammar*  :nr-of-speech-acts 1)

;; define a number of series to run and a number of speech-acts to comprehend in each serie? 
(defun run-speech-acts (from to series cxn-inventory corpus-processor)
  (loop for i from 1 to series
        do (setf (counter corpus-processor) from)
           (comprehend corpus-processor :cxn-inventory cxn-inventory  :nr-of-speech-acts (- to from))))

(defun run-in-batches (from to series batch-size grammar corpus-processor)
  (loop with number-of-batches = (round (/ (- to from) batch-size))
        with number-of-remaining-speech-acts = (mod to batch-size)
        with begin-counter = from
        for i from 1 to number-of-batches
        for end-counter = (+ begin-counter batch-size)
        do (run-speech-acts begin-counter end-counter series grammar corpus-processor)
           (setf begin-counter end-counter)
        finally (run-speech-acts end-counter
                                 (+ end-counter number-of-remaining-speech-acts)
                                 series grammar corpus-processor)))

(defun reset-grammar-and-counter ()
  (defparameter *geoquery-english-grammar* (make-geoquery-english-cxn-inventory-cxns))
  (setf (counter *geoquery-english-train-processor*) 0))

;(reset-grammar-and-counter)
;(deactivate-all-monitors)
;(deactivate-monitor trace-fcg-learning)
;(activate-monitor trace-fcg-learning-in-output-browser)
;(activate-monitor trace-fcg-learning-in-output-browser-verbose)
;(run-in-batches 0 1000 5 10 *geoquery-english-grammar* *geoquery-english-train-processor*)
;(activate-monitor trace-fcg-learning)
;(run-speech-acts 200 250 1 *geoquery-english-grammar* *geoquery-english-train-processor*)


;; comprehending a specific speech act

(comprehend (nth-speech-act *geoquery-english-train-processor* 900)  :cxn-inventory *geoquery-english-grammar*)


(comprehend *geoquery-english-train-processor* :cxn-inventory *geoquery-english-grammar*  :nr-of-speech-acts 1)

(comprehend (nth-speech-act *geoquery-english-train-processor* 4)
            :cxn-inventory *geoquery-english-grammar* :learn nil :align nil :consolidate nil)

;;--------------------------------;;
;; Defining path to the test file ;;
;;--------------------------------;;


(defparameter *geoquery-english-test*
  (merge-pathnames (make-pathname :directory '(:relative "geoquery-english" "test")
                                  :name "geoquery-english-test-4500" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))


;;---------------------;;
;; loading test corpus ;;
;;---------------------;;

(defparameter *geoquery-english-test-processor* (load-corpus *geoquery-english-test* :sort-p t :remove-duplicates t :ipa nil))

(defun compute-test-accuracy (test-processor grammar)
  (loop with result-list = '()
        with nr-of-speech-acts
          = (length (corpus test-processor))
        for i from 1 to nr-of-speech-acts
        for (result solution cip)
          = (multiple-value-list
             (comprehend
             (nth-speech-act test-processor i)
             :cxn-inventory grammar
             :learn nil
             :align nil
             :consolidate nil))
        do (if (cdr
                (assoc
                 :best-solution-matches-gold-standard
                 (data cip)))
             (push 1 result-list)
             (push 0 result-list))
        finally
          (print (length result-list))
          (return (average result-list))))

(compute-test-accuracy *geoquery-english-test-processor* *geoquery-english-grammar*)


