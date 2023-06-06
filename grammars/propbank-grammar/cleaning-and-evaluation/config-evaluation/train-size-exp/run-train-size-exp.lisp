(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "train-size-exp")
                      :name "parameter-learn-eval" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "parallel-simulated-annealing" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "learning-grammars-sa")
                      :name "train-size-exp" :type "lisp"))

;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)

(defparameter *train-corpus* (shuffle (append (train-split *ontonotes-annotations*)
                                              (train-split *ewt-annotations*))))

(defparameter *dev-corpus* (subseq (shuffle (append (dev-split *ontonotes-annotations*)
                                              (dev-split *ewt-annotations*))) 0 1000))

;; Setting the globals
;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration-frequency*
  `((:de-render-mode . :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-units-matched-x2
     :nr-of-applied-cxns
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
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
     :argm-phrase-with-string)))

(defparameter *training-configuration-edge-weight*
  `((:de-render-mode . :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-units-matched-x2
     :nr-of-applied-cxns
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :edge-weight
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
     :argm-phrase-with-string)))

(defparameter *training-configuration-local-bindings*
  `((:de-render-mode . :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-units-matched-x2
     :nr-of-applied-cxns
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :prefer-local-bindings
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
     :argm-phrase-with-string)))

; (:excluded-rolesets
;      :be.01 :be.02 :be.03
;      :have.01 :have.02 :have.03 :have.04 :have.05 :have.06 :have.07 :have.08 :have.09 :have.10 :have.11
;      :get.03 :get.06 :get.24)

(defparameter test-grammar nil)
    
;; Learn and make predictions for a PropBank grammar using simulated annealing to explore the search space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-experiments ()
  (learn-comprehend-evaluate-grammar-train-size 123648 :output-file-exp "results-train-size-exp-frequency.csv" :fcg-configuration *training-configuration-frequency*)
  (learn-comprehend-evaluate-grammar-train-size 123648 :output-file-exp "results-train-size-exp-local-bindings.csv" :fcg-configuration *training-configuration-local-bindings*)
  (learn-comprehend-evaluate-grammar-train-size 123648 :output-file-exp "results-train-size-exp-edge-weight.csv" :fcg-configuration *training-configuration-edge-weight*))

(run-experiments)





