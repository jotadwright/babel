(in-package :fcg)

;;(ql:quickload :fcg-learn)


(def-fcg-constructions empty-cxn-inventory
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
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:meaning-representation-format . :irl)
                       (:diagnostics diagnose-cip-against-gold-standard)
                       (:repairs repair-add-categorial-link repair-learn-holophrastic-cxn
                        repair-through-anti-unification)
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


;;#######################################
;; Loading training data for CLEVR French
;;#######################################

(defparameter *clevr-french-stage-1-train*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "clevr-french" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

  
;;Takes 10-20 seconds to load corpus
(defparameter *clevr-french-processor* (load-corpus *clevr-french-stage-1-train* :sort-p t))

(defparameter *first-500-shuffled* (shuffle-first-nth-speech-acts *clevr-french-processor* 500))

(defparameter *clevr-french-grammar* (make-empty-cxn-inventory-cxns))

(reset-cp *clevr-french-processor*)

(comprehend (next-speech-act *clevr-french-processor*) :cxn-inventory *clevr-french-grammar* )
(comprehend (current-speech-act *clevr-french-processor*) :cxn-inventory *clevr-french-grammar*)

(loop for i from 0 to 20 do 
        (comprehend (next-speech-act *clevr-french-processor*) :cxn-inventory *clevr-french-grammar*))




;;#############################################
;; Testing
;;#############################################

(defparameter *clevr-french-stage-1-test*
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning" "clevr-french" "val")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

(defparameter *sorted-observations-test* (load-and-sort-observations *clevr-french-stage-1-test*))
(comprehend (first (random-elt *sorted-observations-test*)) :cxn-inventory *holophrase-grammar*)



