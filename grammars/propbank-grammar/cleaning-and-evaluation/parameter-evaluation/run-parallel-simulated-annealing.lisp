(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "parameter-learn-eval" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation")
                      :name "parallel-simulated-annealing" :type "lisp"))

;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


(defparameter *train-corpus* (shuffle (append (train-split *ontonotes-annotations*)
                                              (train-split *ewt-annotations*))))

(defparameter *dev-corpus* (subseq (shuffle (append (dev-split *ontonotes-annotations*)
                                              (dev-split *ewt-annotations*))) 0 2000))

;; Setting the globals
;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration-all*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2
     :nr-of-units-matched
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :edge-weight
     :prefer-local-bindings
     :frequency
     )
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:sort-cxns-before-application . nil)

    (:node-expansion-mode . :full-expansion)
    (:hash-mode . :hash-lemma)
    
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles
     :argm-leaf
     :argm-pp
     :argm-sbar
     :argm-phrase-with-string)
    (:excluded-rolesets
     ((:be.01 :be.02 :be.03))
     ((:have.01 :have.02 :have.03 :have.04 :have.05 :have.06 :have.07 :have.08 :have.09 :have.10 :have.11))
     ((:get.03 :get.06 :get.24)))))

; (:excluded-rolesets
;      :be.01 :be.02 :be.03
;      :have.01 :have.02 :have.03 :have.04 :have.05 :have.06 :have.07 :have.08 :have.09 :have.10 :have.11
;      :get.03 :get.06 :get.24)

(defparameter training-configuration-new nil)

(defparameter test-grammar nil)
    
;; Learn and make predictions for a PropBank grammar using simulated annealing to explore the search space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make a list of parameter combinations
(setf combinations-parameters (all-combinations-parameters '(:HEURISTICS :LEARNING-MODES :EXCLUDED-ROLESETS)))

;; filter that list to only include combinations with certain parameters
(setf filtered-combinations (filter-combinations combinations-parameters
                                         :parameters-to-exclude '((:FREQUENCY :EDGE-WEIGHT :PREFER-LOCAL-BINDINGS) (:nr-of-units-matched-x2 :nr-of-units-matched) (:FREQUENCY :EDGE-WEIGHT) (:FREQUENCY :PREFER-LOCAL-BINDINGS) (:EDGE-WEIGHT :PREFER-LOCAL-BINDINGS))
                                         :parameters-to-include '((:NR-OF-APPLIED-CXNS :CORE-ROLES))))

;; use simulated annealing to explore the search space of the list of combinations. Steps indicate how many configurations it will learn and predict in every thread.
(parallel-simulated-annealing-plots '((:HEURISTICS :NR-OF-APPLIED-CXNS) (:LEARNING-MODES :CORE-ROLES) (:EXCLUDED-ROLESETS)) filtered-combinations :num-threads 10 :steps 100)





