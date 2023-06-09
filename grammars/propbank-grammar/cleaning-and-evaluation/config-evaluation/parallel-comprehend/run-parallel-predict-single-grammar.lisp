(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "parallel-comprehend")
                      :name "parallel-comprehend" :type "lisp"))

;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")

;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*

(defparameter *train-corpus* (shuffle (append (train-split *ontonotes-annotations*)
                                              (train-split *ewt-annotations*))))

(defparameter *dev-corpus* (shuffle (append (dev-split *ontonotes-annotations*)
                                              (dev-split *ewt-annotations*))))


(defparameter *train-corpus* (subseq (shuffle (train-split *ewt-annotations*)) 0 2000))

(defparameter *dev-corpus* (subseq (shuffle (train-split *ewt-annotations*)) 0 20))


;; Learning grammars from the annotated data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)   
    (:cxn-supplier-mode . :hashed-categorial-network)
    
    (:heuristics
     :nr-of-applied-cxns
     :nr-of-units-matched-x2 ;;nr-of-units-matched
     ;;:argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :edge-weight) 
    ;;Additional heuristics: :prefer-local-bindings :frequency
    
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


(learn-propbank-grammar
 *train-corpus*
 :cxn-inventory '*test-grammar*
 :fcg-configuration *training-configuration*)

;; Storing / restoring the grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *test-grammar* 
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "gr5555"
                                :type "fcg"))

(defparameter *test-grammar* (cl-store:restore  
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "gr5555"
                                :type "fcg")))

;; Making predictions with a grammar, storing and evaluating them.
;; This function is parallelized and uses batching to reduce RAM usage.
;; The predictions are stored in a .store file and can be restored using the "restore-predictions" function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(single-grammar-predict "predictions-file-name" :propbank-sentences *dev-corpus* :test-batch-size 10 :test-grammar *test-grammar* :random-number 5555 :num-threads 4)


