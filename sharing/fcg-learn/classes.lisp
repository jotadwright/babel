(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Class definitions for the fcg-learn package ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Constructions ;;
;;;;;;;;;;;;;;;;;;;

(defclass fcg-learn-cxn (fcg-construction)
  ()
  (:documentation "Class that groups all cxns learnt throug the fcg-learn system."))


(defclass holophrastic-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that are direct mappings between form and meaning predicates (no args, no cat)."))


(defclass filler-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that map between meaning predicates and provide a category and arguments to integrate with other cxns."))


(defclass linking-cxn (fcg-learn-cxn)
  ()
  (:documentation "Class for cxns that bind and/or percolate arguments of filler-cxns."))



;; Problems ;;
;;;;;;;;;;;;;;

(defclass fcg-learn-problem (problem)
  ()
  (:documentation "Class that groups fcg-learn problems.
Avoids the need for repair methods to  ever specialise on t - which
causes them to apply twice in notify-learning."))


(defclass gold-standard-not-in-search-space (fcg-learn-problem)
  ()
  (:documentation "Problem to be created when the cip does not contain
a node that matches the gold standard."))


(defclass gold-standard-elsewhere-in-search-space (fcg-learn-problem)
  ()
  (:documentation "Problem to be created when the cip contains a node that matches the gold standard, but where this node
is not the highest ranked solution node (best-solution)."))



;; Diagnostics ;;
;;;;;;;;;;;;;;;;;

(defclass fcg-learn-diagnostic (diagnostic)
  ()
  (:documentation "Class that groups fcg-learn diagnostics"))


(defclass diagnose-cip-against-gold-standard (fcg-learn-diagnostic)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Diagnostic called on cip after routine processing, checking solutions
against the gold standard and diagnosing gold-standard-not-in-search-space and gold-standard-not-in-search-space problems."))



;; Repairs ;;
;;;;;;;;;;;;;

(defclass fcg-learn-repair (repair)
  ()
  (:documentation "Class that groups fcg-learn repairs"))


(defclass repair-learn-holophrastic-cxn (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates a holophrastic construction - always works."))


(defclass repair-through-anti-unification (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates filler and/or linking cxns through anti-unification."))


(defclass repair-add-categorial-link (fcg-learn-repair)
  ((trigger :initform 'routine-processing-finished))
  (:documentation "Repair that creates categorial links only."))



;; Fixes ;;
;;;;;;;;;;;

(defclass cxn-inventory-fix (fix)
  ((fixed-cars
    :accessor fixed-cars
    :initform nil
    :initarg :fixed-cars
    :type list
    :documentation "Slot that holds a list of construction application results that lead to a solution.")
   (speech-act
    :accessor speech-act
    :initform nil
    :initarg :speech-act
    :documentation "The speech act based on which that fix was created."))
  (:documentation "When applying sequentially the fix-cxns to the initial
transient structure given the categorial links, a solution should be found"))


(defclass holophrastic-fix (cxn-inventory-fix)
  ((fix-constructions
    :accessor fix-constructions
    :initform nil
    :initarg :fix-constructions
    :type list
    :documentation "Slot that holds the holophrastic cxn that was created."))
  (:documentation "Class for fixes created by repair-learn-holophrastic-cxn."))


(defclass anti-unification-fix (cxn-inventory-fix)
  ((fix-constructions
    :accessor fix-constructions
    :initform nil
    :initarg :fix-constructions
    :type list
    :documentation "Slot that holds the cxns that were created.")
   (fix-categories
    :accessor fix-categories
    :initform nil
    :initarg :fix-categories
    :type list
    :documentation "Slot that holds the categories that were created.")
   (fix-categorial-links
    :accessor fix-categorial-links
    :initform nil
    :initarg :fix-categorial-links
    :type alist
    :documentation "Slot that holds the categorial links that were created.")
   (base-cxns
    :initarg :base-cxns
    :accessor base-cxns
    :type list
    :documentation "Constructions used in the anti-unification process."))
  (:documentation "Class for fixes created by repair-through-anti-unification."))


(defclass categorial-link-fix (cxn-inventory-fix)
  ((fix-categorial-links
    :accessor fix-categorial-links
    :initform nil
    :initarg :fix-categorial-links
    :type alist
    :documentation "Slot that holds the categorial links that were created."))
  (:documentation "Class for fixes created by repair-add-categorial-link."))


;; Speech act ;;
;;;;;;;;;;;;;;;;

(defclass speech-act (blackboard)
  ((form
    :type string
    :accessor form
    :initform nil
    :initarg :form)
   (meaning
    :type list
    :accessor meaning
    :initform nil
    :initarg :meaning)
   (situation
    :accessor situation
    :initform nil
    :initarg :situation))
  (:documentation "Class that represents a speech act during a communicative interaction."))

(defmethod copy-object ((speech-act speech-act))
  speech-act)

