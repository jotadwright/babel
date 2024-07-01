(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; FCG Learn experiment configurations ;;
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :fcg-learn)
;; (activate-monitor trace-fcg-learning)
;; (deactivate-monitor trace-fcg-learning)
(deactivate-all-monitors)
(activate-monitor trace-fcg-learning-in-output-browser)
(activate-monitor trace-fcg-learning-in-output-browser-verbose)

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
                       (:alignment-mode . :punish-non-gold-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.1)
                       (:best-solution-mode . :highest-average-link-weight)
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
(defparameter *clevr-stage-1-train-processor* (load-corpus *clevr-stage-1-train* :sort-p t :remove-duplicates t))
(defparameter *clevr-stage-1-grammar* (make-holophrase-cxn-inventory-cxns))

(progn
  (reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-holophrase-cxn-inventory-cxns))
  (comprehend *clevr-stage-1-train-processor*
              :cxn-inventory *clevr-stage-1-grammar*
              :nr-of-speech-acts 
            (array-dimension (corpus *clevr-stage-1-train-processor*) 0)))


;;;;;;;;;;;;;;;;;;;;;;
;; all repairs      ;;
;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions clevr-cxn-inventory
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
                       (:repairs  repair-add-categorial-link repair-through-anti-unification)
                       (:learning-mode . :pattern-finding)
                       (:alignment-mode . :punish-non-gold-solutions)
                       (:li-reward . 0.2)
                       (:li-punishement . 0.5)
                       (:best-solution-mode . :highest-average-link-weight)
                       (:induce-cxns-mode . :filler-and-linking)
                       (:form-generalisation-mode . :needleman-wunsch)
                       (:meaning-generalisation-mode . :exhaustive)
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
  (merge-pathnames (make-pathname :directory '(:relative "clevr-grammar-learning-ipa" "train")
                                  :name "stage-1" 
                                  :type "jsonl")
                   cl-user:*babel-corpora*))

;;Takes 10-20 seconds to load corpus
(defparameter *clevr-stage-1-train-processor* (load-corpus *clevr-stage-1-train* :sort-p t :remove-duplicates t :ipa nil))
(defparameter *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))

(reset-cp *clevr-stage-1-train-processor*)
(setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
(comprehend *clevr-stage-1-train-processor* :cxn-inventory *clevr-stage-1-grammar*  :nr-of-speech-acts 1)

(comprehend (current-speech-act *clevr-stage-1-train-processor*)  :cxn-inventory *clevr-stage-1-grammar*)


(progn
  (reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
  (comprehend *clevr-stage-1-train-processor*
              :cxn-inventory *clevr-stage-1-grammar*
              :nr-of-speech-acts 3000) ;;(array-dimension (corpus *clevr-stage-1-train-processor*) 0)
  )


(deactivate-monitor trace-fcg-learning)
(activate-monitor trace-fcg-learning)

(reset-cp *clevr-stage-1-train-processor*)
  (setf *clevr-stage-1-grammar* (make-clevr-cxn-inventory-cxns))
(comprehend *clevr-stage-1-train-processor* :cxn-inventory *clevr-stage-1-grammar* :nr-of-speech-acts 1)

(add-element (make-html (meaning (nth-speech-act *clevr-stage-1-train-processor* 20))))
(inspect (cip  *saved-cipn*))

(gold-standard-solution-p (fcg-get-transient-structure (get-data (cip *saved-cipn*) :best-solution-node))
                          (nth-speech-act *clevr-stage-1-train-processor* 469)
                          (direction (cip *saved-cipn*))
                          (configuration (construction-inventory (cip *saved-cipn*))))

(form (speech-act (attr-val *saved-cxn* :fix)))
make-html

(inspect *saved-cxn*)

equivalent-cxn