;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Learning and evaluating PropBank-based grammars.  ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;

;; Loading the :propbank-grammar system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)
 
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
     :argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
     :edge-weight) 
    ;;Additional heuristics: :prefer-local-bindings :frequency
    
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:sort-cxns-before-application . nil)

    (:node-expansion-mode . :full-expansion)
    (:hash-mode . :hash-lemma)
    
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles)))

(defparameter *propbank-ewt-learned-cxn-inventory* nil)

(learn-propbank-grammar
 ;(train-split *ewt-annotations*)
 (append
  (train-split *ewt-annotations* )
  (train-split *ontonotes-annotations*))
  :excluded-rolesets '("be.01" "be.02" "be.03"
                                             "do.01" "do.02" "do.04" "do.11" "do.12"
                                             "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                             "get.03" "get.06" "get.24")
 :selected-rolesets nil
 :cxn-inventory '*propbank-ewt-learned-cxn-inventory*
 :fcg-configuration *training-configuration*)

;; Storing and restoring grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-ewt-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                                :type "fcg"))

(defparameter *propbank-ewt-learned-cxn-inventory*
  (cl-store:restore 
   (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                   :type "fcg")))
