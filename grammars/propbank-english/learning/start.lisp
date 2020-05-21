;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                 ;;
;; Learning grammars from propbank-annotated data. ;;
;;                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;

;; Loading the :propbank-english sytem
;;(ql:quickload :propbank-english)
(in-package :propbank-english)

;; Loading the Propbank frames (takes a few seconds)
(load-pb-data :store-data t :ignore-stored-data nil)
;(length *pb-data*)

;; Loading the Propbank annotations (takes a minute)
(load-propbank-annotations :store-data nil :ignore-stored-data nil)
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


(defun all-sentences-annotated-with-roleset (roleset &key (split #'train-split)) ;;or #'dev-split
  (loop for sentence in (funcall split *propbank-annotations*)
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'string=)
        collect sentence))

;; Retrieve all sentences in training set for a given roleset:
;; (all-sentences-annotated-with-roleset "believe.01")

;; Retrieve all sentences in de development set for a given roleset (for evaluation):
;; (length (all-sentences-annotated-with-roleset "believe.01" :split #'dev-split)) ;;call #'length for checking number


(defun print-propbank-sentences-with-annotation (roleset &key (split #'train-split))
  "Print the annotation of a given roleset for every sentence of the
split to the output buffer."
  (loop for sentence in (funcall split *propbank-annotations*)
        for sentence-string = (sentence-string sentence)
        for selected-frame = (loop for frame in (propbank-frames sentence)
                                   when (string= (frame-name frame) roleset)
                                   return frame)
        when selected-frame ;;only print if selected roleset is present in sentence
        do (let ((roles-with-indices (loop for role in (frame-roles selected-frame)
                                       collect (cons (role-type role) (role-string role)))))
             (format t "~a ~%" sentence-string)
             (loop for (role-type . role-string) in roles-with-indices
                   do (format t "~a: ~a ~%" role-type role-string)
                   finally (format t "~%")))))


(print-propbank-sentences-with-annotation "believe.01")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test learning based on Propbank sentences.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Select a sentence object
(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

;;Create an empty cxn inventory
(defparameter *propbank-learned-cxn-inventory* nil)

(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents)
                       (:node-tests :restrict-nr-of-nodes :restrict-search-depth))
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
(defparameter *believe-sentences* (loop with all-sentences-annotated-with-roleset  = (all-sentences-annotated-with-roleset "believe.01")
                                        for i from 1 to 40
                                        collect (nth i all-sentences-annotated-with-roleset)))

(loop for sentence in *believe-sentences*
      do (learn-cxn-from-propbank-annotation sentence "believe.01" *propbank-learned-cxn-inventory*))

;; OR

(learn-propbank-grammar *believe-sentences* '("believe.01"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate a grammar on the propbank sentences .  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Evaluating the learned grammar:
(evaluate-propbank-sentences *believe-sentences* *propbank-learned-cxn-inventory* '("believe.01"))


;;Evaluating the written grammar:
(evaluate-propbank-sentences *believe-sentences* *fcg-constructions* '("believe.01"))
