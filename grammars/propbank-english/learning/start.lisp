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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test learning based on Propbank sentences.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Select a sentence object
(defparameter *believe-sentence* (third (all-sentences-annotated-with-roleset "believe.01")))

;;Create an empty cxn inventory
(def-fcg-constructions propbank-learned-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents-without-tokenisation) ;;:de-render-constituents-dependents-without-tokenisation
                       (:node-tests :check-double-role-assignment :restrict-nr-of-nodes)
                       (:parse-goal-tests :no-valid-children)
                       (:max-nr-of-nodes . 100)
                       (:parse-order multi-argument-with-lemma multi-argument-without-lemma single-argument-with-lemma single-argument-without-lemma)
                       (:node-expansion-mode . :multiple-cxns)
                       (:priority-mode . :nr-of-applied-cxns)
                       (:queue-mode . :greedy-best-first)
                       (:hash-mode . :hash-lemma)
                       (:cxn-supplier-mode . :hashed-scored-labeled)
                       (:equivalent-cxn-fn . fcg::equivalent-propbank-construction)
                       (:equivalent-cxn-key . identity)
                       (:learning-mode ;:multi-argument-with-lemma :multi-argument-without-lemma
                        :single-argument-with-lemma
                        ))
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
(learn-cxn-from-propbank-annotation *believe-sentence* "believe.01" *propbank-learned-cxn-inventory* :single-argument-with-lemma)

;;Try out the learned construction in comprehension
(activate-monitor trace-fcg)
(comprehend-and-extract-frames (sentence-string *believe-sentence*) :cxn-inventory *propbank-learned-cxn-inventory*)




;;Try out the same for multiple sentences of a given roleset
;;----------------------------------------------------------
(defparameter *opinion-sentences* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *opinion-sentences-dev* (shuffle (loop for roleset in '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(length *believe-sentences-dev*)

(defparameter *believe-sentences* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'train-split))))

(defparameter *believe-sentences-dev* (shuffle (loop for roleset in '("BELIEVE.01")
                                                 append (all-sentences-annotated-with-roleset roleset :split #'dev-split))))

(learn-propbank-grammar *believe-sentences*
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :selected-rolesets '("BELIEVE.01")
                        :silent t
                        :tokenize? nil)

;;for storing learned grammar
(cl-store:store *propbank-learned-cxn-inventory*
                (babel-pathname :directory '("grammars" "propbank-english" "learning")
                                :name "learned-grammar"
                                :type "fcg"))

;;and later restoring it
(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "learning")
                           :name "learned-grammar"
                           :type "fcg")))

(evaluate-propbank-sentences
 *believe-sentences-dev*
 *propbank-learned-cxn-inventory*
 :selected-rolesets  '("BELIEVE.01")
 :silent t
 )

(deactivate-all-monitors)

(add-element (make-html *propbank-learned-cxn-inventory* ))

(defun fcg::equivalent-propbank-construction  (cxn-1 cxn-2)
  (cond ((eq 'fcg::processing-construction (type-of cxn-1))
         (and ;(equalp (name cxn-1) (name cxn-2))
              (= (length (right-pole-structure cxn-1)) (length (right-pole-structure cxn-2)))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2))))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (unit-body unit) :key #'first)))
                                                   (right-pole-structure cxn-2)))
                               )))
  ((eq (type-of cxn-1) 'fcg-construction)
   (and (= (length (conditional-part cxn-1)) (length (conditional-part cxn-2)))
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lex-class (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'phrase-type (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )
              (equalp (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-1)))
                               (remove nil (mapcar #'(lambda (unit)
                                                       (second (find 'lemma (comprehension-lock unit) :key #'first)))
                                                   (conditional-part cxn-2)))
                               )))))






(setf *selected-sentence* (find "Investors here still expect Ford Motor Co. or General Motors Corp. to bid for Jaguar ."
                                *opinion-sentences-dev* :key #'sentence-string :test #'string=))

(learn-cxn-from-propbank-annotation *selected-sentence* "expect.01" *propbank-learned-cxn-inventory* :single-argument-with-lemma)
(learn-propbank-grammar (list *selected-sentence*)
                        :cxn-inventory '*propbank-learned-cxn-inventory*
                        :selected-rolesets '("expect.01")
                        :silent t
                        :tokenize? nil)

(set-configuration *propbank-learned-cxn-inventory* :parse-goal-tests '(:no-valid-children))


(activate-monitor trace-fcg)
(comprehend-and-extract-frames (sentence-string (fifth *believ) :cxn-inventory *propbank-learned-cxn-inventory*)

(with-activated-monitor trace-fcg
  (comprehend-and-extract-frames (sentence-string (fifth *believe-sentences*)) :cxn-inventory *restored-grammar*))

)
(evaluate-propbank-sentences
 *opinion-sentences-dev*
 *propbank-learned-cxn-inventory*
 :selected-rolesets  '("FIGURE.01" "FEEL.02" "THINK.01" "BELIEVE.01" "EXPECT.01")
 :silent t)


(add-cxn *saved-cxn* *restored-grammar*)


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