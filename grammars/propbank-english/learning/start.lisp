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
        when (find roleset (propbank-frames sentence) :key #'frame-name :test #'equalp)
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


;; (print-propbank-sentences-with-annotation "believe.01")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test learning based on Propbank sentences.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Select a sentence object
(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

;;Create an empty cxn inventory
(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation) ;;:de-render-constituents-dependents-without-tokenisation
                       (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
                       (:node-expansion-mode . :multiple-cxns)
                       (:priority-mode . :nr-of-applied-cxns)
                       (:queue-mode . :greedy-best-first)
                       (:hash-mode . :hash-lemma)
                       (:cxn-supplier-mode . :hashed-simple-queue))
  :visualization-configurations ((:show-constructional-dependencies . nil))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :cxn-inventory *propbank-learned-cxn-inventory*
  :hashed t)

;; Activate FCG monitor and start Penelope (if using Spacy API locally)
;(activate-monitor trace-fcg)
;(setf nlp-tools::*penelope-host* "http://localhost:5000")

;; Learn a construction based on the selected sentence
(learn-cxn-from-propbank-annotation *believe-sentence* "believe.01" *propbank-learned-cxn-inventory*)

;;Try out the learned construction in comprehension
(comprehend-and-extract-frames (sentence-string *believe-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)





;;Try out the same for multiple sentences of a given roleset
;;----------------------------------------------------------
(defparameter *opinion-sentences* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *opinion-sentences-dev* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(length *opinion-sentences-dev*)

(learn-propbank-grammar *opinion-sentences*
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :selected-rolesets '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                        :silent t
                        :tokenize? nil)

(evaluate-propbank-sentences
 (subseq *opinion-sentences* 0 20)
 *propbank-learned-cxn-inventory*
 ;:selected-rolesets  '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
 )



(setf *selected-sentence* (find "Do n't think of it as a literary competition ."
                                *opinion-sentences* :key #'sentence-string :test #'string=))

(learn-cxn-from-propbank-annotation *selected-sentence* "think.01" *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames (sentence-string *selected-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)

(evaluate-propbank-sentences
 *opinion-sentences-dev*
 *propbank-learned-cxn-inventory*
 :selected-rolesets  '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
 :silent t)

;; FREQUENTLY OCCURRING PROBLEMS 
;;------------------------------

(activate-monitor trace-fcg)


;; lang
(defparameter *selected-sentence* (find "In a way presidents do n't normally pay as much attention to Northern Ireland as Clinton has , so he has actually paid the Irish question probably too much attention for a President , and what that means is that the people involved in the peace process here believe they are more important than they actually are" *opinion-sentences* :key #'sentence-string :test #'equalp))
In a way presidents do n't normally pay as much attention to Northern Ireland as Clinton has , so he has actually paid the Irish question probably too much attention for a President , and what that means is that the people involved in the peace process here believe they are more important than they actually are


;; OPGELOST
;;Probleem met quotes 
(defparameter *selected-sentence* (find "`` You people here think this is Russian music , '' she said with disdain , and called over to the waitress : `` Could you turn it off ? ''" *opinion-sentences* :key #'sentence-string :test #'equalp))
(learn-cxn-from-propbank-annotation *selected-sentence* "think.01" *propbank-learned-cxn-inventory*)
(spacy-benepar-compatible-annotation *selected-sentence* "think.01" :tokenize? nil)
(spacy-benepar-compatible-annotation *selected-sentence* "think.01" :tokenize? t)
(comprehend-and-extract-frames (sentence-string *selected-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)
(evaluate-propbank-sentences `(,*selected-sentence*) *propbank-learned-cxn-inventory* :selected-rolesets '("think.01"))

;; OPGELOST
;;Hier zie ik niet waarom de F1 score niet 100% is
;;((:NR-OF-CORRECT-PREDICTIONS . 24) (:NR-OF-PREDICTIONS . 25) (:NR-OF-GOLD-STANDARD-PREDICTIONS . 26))
(setf *selected-sentence* (find "First , I think the arrival of the wolves as %pw , description and appraisal , is , I think , a very good appraisal ." *opinion-sentences* :key #'sentence-string :test #'string=))
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? t)
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? nil)
(learn-cxn-from-propbank-annotation *selected-sentence* "think.01" *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames (sentence-string *selected-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)
(evaluate-propbank-sentences `(,*selected-sentence*) *propbank-learned-cxn-inventory* :selected-rolesets '("think.01"))

;; OPGELOST
;;Kan dit iets te maken hebben met het feit dat er 3 ARGM-DIS zijn??
;;((:NR-OF-CORRECT-PREDICTIONS . 15) (:NR-OF-PREDICTIONS . 21) (:NR-OF-GOLD-STANDARD-PREDICTIONS . 21))
(setf *selected-sentence* (find "Ay Today , Wendao , so when you mentioned court , I thought of this kind of controversy over the Qiu Xinghua court case ." *opinion-sentences* :key #'sentence-string :test #'string=))
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? t)
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? nil)
(learn-cxn-from-propbank-annotation *selected-sentence* "think.01" *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames (sentence-string *selected-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)
(evaluate-propbank-sentences `(,*selected-sentence*) *propbank-learned-cxn-inventory* :selected-rolesets '("think.01"))


;;Onoplosbaar: Argm-prp is geen constituent (houden we NIL of beter niet toevoegen aan frame?)
(setf *selected-sentence* (find "He wants to enhance Russia 's standing in the world and to do that he believes that Moscow must assume a greater role in international affairs ." *opinion-sentences* :key #'sentence-string :test #'string=))
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? t)
(spacy-benepar-compatible-annotation *selected-sentence* :tokenize? nil)
(learn-cxn-from-propbank-annotation *selected-sentence* "believe.01" *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames (sentence-string *selected-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)
(evaluate-propbank-sentences `(,*selected-sentence*) *propbank-learned-cxn-inventory* :selected-rolesets '("believe.01"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate a grammar on the propbank sentences .  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Evaluating the learned grammar:
(evaluate-propbank-sentences (all-sentences-annotated-with-roleset "believe.01" :split #'dev-split)
 *propbank-learned-cxn-inventory*
                             :selected-rolesets  '("believe.01"))


;;Evaluating the written grammar:
(evaluate-propbank-sentences ;(all-sentences-annotated-with-roleset "believe.01" :split #'dev-split)
 *believe-sentences*
 
                             *fcg-constructions*
                            ; :selected-rolesets nil
                             :selected-rolesets '("stop.01")
                             )
(length (all-sentences-annotated-with-roleset "believe.01" :split #'dev-split))