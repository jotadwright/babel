;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
;(load-pb-data :store-data t :ignore-stored-data nil)
;(length *pb-data*)

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data t :ignore-stored-data nil)
;(length (train-split *propbank-annotations*))


(defun all-rolesets-for-framenet-frame (framenet-frame-name)
  (loop for predicate in *pb-data*
        for rolesets = (rolesets predicate)
        for rolesets-for-framenet-frame = (loop for roleset in rolesets
                                                    when (find framenet-frame-name (aliases roleset) :key #'framenet :test #'member)
                                                    collect (id roleset))
        when rolesets-for-framenet-frame
        collect it))

;; (all-rolesets-for-framenet-frame 'opinion)


(defun all-sentences-annotated-with-roleset (roleset)
  (loop for sentence in (train-split *propbank-annotations*)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'string=)
        collect sentence))

;; (length (all-sentences-annotated-with-roleset "believe.01"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test learning based on Propbank sentences.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Select a sentence object
(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

;;Create an empty cxn inventory
(defparameter *propbank-learned-cxn-inventory* nil)

(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents))
  :visualization-configurations ((:show-constructional-dependencies . nil))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :cxn-inventory *propbank-learned-cxn-inventory*)


;; Activate FCG monitor and start Penelope (if using Spacy API locally)
;(activate-monitor trace-fcg)
;(setf nlp-tools::*penelope-host* "http://localhost:5000")

;; Learn a construction based on the selected sentence
(learn-cxn-from-propbank-annotation *believe-sentence* "believe.01" *propbank-learned-cxn-inventory*)

;;Try out the learned construction in comprehension
(comprehend-and-extract-frames (sentence-string *believe-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)



;;Try out the same for multiple sentences of a given roleset
;;----------------------------------------------------------
(defparameter *believe-sentences* (loop for i from 1 to 20
                                        collect (nth i (all-sentences-annotated-with-roleset "believe.01"))))

(loop for sentence in *believe-sentences*
      do (learn-cxn-from-propbank-annotation sentence "believe.01" *propbank-learned-cxn-inventory*))

(loop for sentence in *believe-sentences*
      do (comprehend-and-extract-frames (sentence-string sentence) :cxn-inventory *propbank-learned-cxn-inventory*))