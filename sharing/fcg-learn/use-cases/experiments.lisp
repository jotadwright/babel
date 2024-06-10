(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; FCG Learn experiment configurations ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deactivate-all-monitors)
(activate-monitor trace-fcg-learning-in-output-browser)

;; Holophrases only ;;
;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions holophrase-cxn-inventory
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
                       (:cxn-supplier-mode . :hashed) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:hash-mode . :filler-and-linking)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs repair-learn-holophrastic-cxn)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :lateral-inhibition-avg-entenchment-score)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.2)
                       (:best-solution-mode . :highest-average-entrenchment-score)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:form-generalisation-mode . :needleman-wunsch)
                       (:max-nr-of-gaps-in-form-predicates . 1)
                       (:meaning-generalisation-mode . :k-swap)
                       (:k-swap-k . 1)
                       (:k-swap-w . 1)
                       (:consolidate-repairs . t)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:expand-nodes-in-search-tree . t)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure))
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:show-categorial-network . t)))


(defparameter *clevr-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;;Takes 10-20 seconds to load corpus
(defparameter *clevr-stage-1-train-processor* (load-corpus *clevr-stage-1-train* :sort-p t))
(defparameter *clevr-stage-1-grammar* (make-holophrase-cxn-inventory-cxns))

(progn
  (reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-holophrase-cxn-inventory-cxns))
  (comprehend *clevr-stage-1-train-processor*
              :cxn-inventory *clevr-stage-1-grammar*
              :nr-of-speech-acts 
            (array-dimension (corpus *clevr-stage-1-train-processor*) 0)
              ))


