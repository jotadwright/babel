;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Learning and evaluating PropBank-based grammars.  ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;

;; Loading the :propbank-grammar system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

(activate-monitor export-categorial-network-to-jsonl)

;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")


;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


;; Restoring grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *grammar-storage-path*
  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank" "stored-propbank-grammars"))
                                  :name "ewt-ontonotes-core-roles"
                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                   *babel-corpora*))

;;(defparameter *restored-grammar* (cl-store:restore *grammar-storage-path*))



;; Learning grammars from the annotated data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *training-configuration*
  `((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)

    (:construction-inventory-processor-mode . :heuristic-search)
    (:search-algorithm . :best-first)
    (:heuristic-value-mode . :sum-heuristics-and-parent)
    (:heuristics :nr-of-applied-cxns :edge-weight :frequency)   ;; Additional heuristics: :prefer-local-bindings :nr-of-units-matched
    (:cxn-supplier-mode . :hashed-categorial-network)
    
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

(defparameter *full-corpus* (append (train-split *ontonotes-annotations*)
                                    (test-split *ontonotes-annotations*)
                                    (dev-split *ontonotes-annotations*)
                                    (train-split *ewt-annotations*)
                                    (test-split *ewt-annotations*)
                                    (dev-split *ewt-annotations*)))

(defparameter *training-set* (shuffle (append (train-split *ontonotes-annotations*)
                                              (train-split *ewt-annotations*))))

(learn-propbank-grammar *training-set*
                        :excluded-rolesets '("be.01" "be.02" "be.03"
                                             "do.01" "do.02" "do.04" "do.11" "do.12"
                                             "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                             "get.03" "get.06" "get.24")
                        :cxn-inventory '*propbank-ontonotes-ewt-train-corpus-all-roles*
                        :model "en_benepar"
                        :fcg-configuration *training-configuration*)



;; Testing learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-element (make-html *propbank-ontonotes-ewt-train-corpus-all-roles*))
;(activate-monitor trace-fcg)

(comprehend-and-extract-frames "The child fed the cat for her mother" :cxn-inventory *propbank-ontonotes-ewt-train-corpus-all-roles*)

(comprehend-and-extract-frames "The walls crumbled to the ground." :cxn-inventory *propbank-ontonotes-ewt-train-corpus-all-roles*)

(comprehend-and-extract-frames "The plumber unclogged the sink with a drain snake." :cxn-inventory *propbank-ontonotes-ewt-train-corpus-all-roles*)

(comprehend-and-extract-frames "You should not sprinkle the cos lettuce during the heat of the sun." :cxn-inventory *propbank-ontonotes-ewt-train-corpus-all-roles*)



;; Cleaning learned grammars on development set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(monitors::deactivate-all-monitors)

(defparameter *development-set* (append (dev-split *ewt-annotations*)
                                        (dev-split *ontonotes-annotations*)))

;; Use learned grammar on the development corpus to gather statistics on spurious construction applications
(defparameter *sorted-cxns*
  (sort-cxns-for-outliers *propbank-ontonotes-ewt-train-corpus-all-roles*
                          (shuffle *development-set*)
                          :timeout 20
                          :nr-of-training-sentences (get-data (blackboard *propbank-ontonotes-ewt-train-corpus-all-roles*) :training-corpus-size)
                          :nr-of-test-sentences 500))

;; Delete constructions from the learned grammar that apply too often
;; (apply-cutoff *propbank-ontonotes-ewt-train-corpus-all-roles* :cutoff 4 :sorted-cxn-list *sorted-cxns*)

;; Delete all constructions for be and have from the grammar
;; (delete-have-and-be-cxns *propbank-ontonotes-ewt-train-corpus-all-roles*)

;; Store the cleaned grammar
;;(cl-store:store *propbank-ontonotes-ewt-train-corpus-all-roles* *grammar-storage-path*)


;; (Optionally store the ranked cxns for future cleaning)
#|(cl-store:store *sorted-cxns*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "sorted-cxns-sbcl"
                                :type "store"))

  (setf *sorted-cxns* (cl-store:restore
                       (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                       :name "sorted-cxns-sbcl"
                                       :type "store")))|#

;; Evaluating learned grammars on the test set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(monitors::deactivate-all-monitors)

(defparameter *test-set* (append (test-split *ewt-annotations*)
                                 (test-split *ontonotes-annotations*)))

(comprehend-and-evaluate *test-set* *propbank-ontonotes-ewt-train-corpus-all-roles*
                         :excluded-rolesets '("be.01" "be.02" "be.03"
                                              "do.01" "do.02" "do.04" "do.11" "do.12"
                                              "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                              "get.03" "get.06" "get.24")
                         :core-roles-only t
                         :include-sentences-with-incomplete-role-constituent-mapping nil)

;; Dummy testing on train corpus:
#|
(loop for i from 0
      for sentence in (subseq *train-corpus* 0 5)
        do (pprint i)
           (comprehend-and-evaluate (list sentence) *propbank-ontonotes-ewt-train-corpus-all-roles*
                         :excluded-rolesets '("be.01" "be.02" "be.03"
                                              "do.01" "do.02" "do.04" "do.11" "do.12"
                                              "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                                              "get.03" "get.06" "get.24")
                         :core-roles-only t
                         :include-sentences-with-incomplete-role-constituent-mapping nil)) |#



