;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Learning and evaluating PropBank-based grammars.  ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;

;; Loading the :propbank-grammar system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)




;; Activating spacy-api locally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")


;; Loading the Propbank annotations (takes a couple of minutes)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-propbank-annotations 'ewt :ignore-stored-data nil) ; *ewt-annotations*
(load-propbank-annotations 'ontonotes :ignore-stored-data nil) ; *ontonotes-annotations*


;; Storing and restoring grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *restored-grammar-sbcl*
  (cl-store:restore
   (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-core-roles-cleaned-sbcl"
                   :type "fcg")))

(defparameter *restored-grammar-lw*
  (cl-store:restore
   (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                   :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                   :type "fcg")))

(cl-store:store *propbank-ewt-ontonotes-learned-cxn-inventory* ;*propbank-ewt-ontonotes-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "propbank-grammar-ontonotes-ewt-core-roles-lw"
                                :type "fcg"))


;; Learning grammars from the annotated data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *training-configuration*
  '((:de-render-mode .  :de-render-constituents-dependents)
    (:node-tests :check-double-role-assignment)
    (:parse-goal-tests :no-valid-children)
    (:max-nr-of-nodes . 100)
    (:node-expansion-mode . :multiple-cxns)
    (:priority-mode . :nr-of-applied-cxns)
    (:queue-mode . :greedy-best-first)
    (:hash-mode . :hash-lemma)
    (:parse-order
     lexical-cxn
     argument-structure-cxn
     argm-phrase-cxn
     argm-leaf-cxn
     word-sense-cxn)
    (:replace-when-equivalent . nil)
    (:learning-modes
     :core-roles
     ;:argm-pp
     ;:argm-sbar
     ;:argm-leaf
     ;:argm-phrase-with-string
     )
    (:cxn-supplier-mode . :propbank-english)))

(defvar *propbank-ewt-ontonotes-learned-cxn-inventory*)

(learn-propbank-grammar
 (append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))
 :selected-rolesets nil
 :excluded-rolesets '("be.01" "be.02" "be.03"
                      "do.01" "do.02" "do.04" "do.11" "do.12"
                      "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11")
 :cxn-inventory '*propbank-ewt-ontonotes-learned-cxn-inventory-no-aux*
 :fcg-configuration *training-configuration*)

(add-element (make-html *propbank-ewt-ontonotes-learned-cxn-inventory-2*))

;; Cleaning learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(monitors::deactivate-all-monitors)
(defparameter *dev-sentences* (append (dev-split *ewt-annotations*)
                                      (dev-split *ontonotes-annotations*)))

;; Use learned grammar on the development corpus to gather statistics on spurious construction applications
(defparameter *sorted-cxns*
  (sort-cxns-for-outliers *propbank-ewt-ontonotes-learned-cxn-inventory*
                          (shuffle *dev-sentences*)
                          :timeout 20
                          :nr-of-training-sentences (get-data (blackboard *propbank-ewt-ontonotes-learned-cxn-inventory*) :training-corpus-size)
                          :nr-of-test-sentences 500))

;; (Optionally store the ranked cxns for future cleaning)
(cl-store:store *sorted-cxns*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "sorted-cxns-lw"
                                :type "store"))

;; Delete constructions from the learned grammar that apply too often
(apply-cutoff *propbank-ewt-ontonotes-learned-cxn-inventory* :cutoff 2000 :sorted-cxn-list *sorted-cxns*)

;; Delete all constructions for be and have from the grammar
(delete-have-and-be-cxns *propbank-ewt-ontonotes-learned-cxn-inventory*)

;; Test whether cleaning worked:
;(deactivate-all-monitors)
(comprehend "Hello world" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory*)
(comprehend "I love you" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory*)
(comprehend "She told them a story" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory*)

;; Testing learned grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(monitors:activate-monitor trace-fcg)

(comprehend "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *restored-propbank-grammar*)

(comprehend-and-extract-frames "Oxygen levels in oceans have fallen 2% in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory*)

(comprehend-and-extract-frames "She sent her mother a dozen roses" :cxn-inventory *restored-propbank-grammar*)

(comprehend-and-extract-frames (sentence-string (nth 0 (train-split *ewt-annotations*))) :cxn-inventory *propbank-ewt-learned-cxn-inventory*)



(comprehend-and-extract-frames "It is feared if far-right candidate becomes French president she will try to destroy the bloc from inside" :cxn-inventory *restored-grammar*)

(comprehend-and-extract-frames "Much of what the far-right Rassemblement National leader does want to do, however implies breaking the EU’s rules, and her possible arrival in the Élysée Palace next weekend could prove calamitous for the 27-member bloc." :cxn-inventory *restored-grammar*)



(comprehend-and-extract-frames "He told him a story" :cxn-inventory *restored-grammar*)
