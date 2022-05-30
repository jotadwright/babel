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

(defparameter *propbank-ewt-learned-cxn-inventory* nil)

(learn-propbank-grammar
 (train-split *ewt-annotations*)
 ;(append (train-split *ontonotes-annotations*) (train-split *ewt-annotations*))
 :selected-rolesets nil
 :cxn-inventory '*propbank-learned-cxn-inventory*
 :fcg-configuration *training-configuration*)

;; Storing and restoring grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                :name "propbank-grammar-ontonotes-ewt-core-roles-sbcl"
                                :type "fcg"))


(defparameter *restored-propbank-grammar* (cl-store:restore  
                                           (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                                                           :name "propbank-grammar-ontonotes-ewt-core-roles-sbcl"
                                                           :type "fcg")))
