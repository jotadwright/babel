(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")


;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


(defparameter *full-corpus* (append (train-split *ontonotes-annotations*)
                                    (test-split *ontonotes-annotations*)
                                    (dev-split *ontonotes-annotations*)
                                    (train-split *ewt-annotations*)
                                    (test-split *ewt-annotations*)
                                    (dev-split *ewt-annotations*)))

(export-argm-strings *full-corpus* :corpus-path
                     (babel-pathname :directory '("grammars" "propbank-grammar" "raw-data") :name "argm-phrases" :type "json"))


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
     ;:argm-prediction ;; Don't forget to activate the text-to-role-classification server!!!!!
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

(defparameter *test-grammar* nil)

(defparameter *train-corpus* (train-split *ontonotes-annotations*))


(learn-propbank-grammar
                                        ;(subseq *train-corpus* 0 10000)
 *train-corpus*
 :excluded-rolesets '("be.01" "be.02" "be.03"
                      "do.01" "do.02" "do.04" "do.11" "do.12"
                      "have.01" "have.02" "have.03" "have.04"
                      "have.05" "have.06" "have.07" "have.08"
                      "have.09" "have.10" "have.11"
                      "get.03" "get.06" "get.24")
 :cxn-inventory '*propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies-ccl-full-1*
 :fcg-configuration *training-configuration*)




;; TODO ugly solution to store the annotations with cl-store
(defun remove-array-locks! (construction-inventory)
  (let ((matrix-names '(nil gram-sense lex-sense lex-gram))
        (matrix (graph-utils::matrix (fcg::graph (categorial-network construction-inventory)))))
    (loop for name in matrix-names
          do  (setf (graph-utils::array-lock (gethash name matrix)) nil))))

(remove-array-locks! *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies-ccl-full-1*)

(cl-store:store *propbank-ontonotes-learned-cxn-inventory-no-aux-all-strategies-ccl-full-1*
               (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                               :name "propbank-grammar-ontonotes-ccl-full"
                               :type "fcg"))
