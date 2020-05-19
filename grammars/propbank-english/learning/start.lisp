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
(defparameter *believe-sentences* (loop with all-sentences-annotated-with-roleset  = (all-sentences-annotated-with-roleset "believe.01")
                                        for i from 1 to 40
                                        collect (nth i all-sentences-annotated-with-roleset)))

(loop for sentence in *believe-sentences*
      do (learn-cxn-from-propbank-annotation sentence "believe.01" *propbank-learned-cxn-inventory*))

(evaluate-propbank-sentences *believe-sentences* *propbank-learned-cxn-inventory* '("believe.01"))


(comprehend-and-extract-frames "Would you believe that some liberal Democrats might support President Bush 's Supreme Court Nominee Samuel Alito ?")


(setf *S* (find "Well , at the same time , uh , this editorial , uh , also believed , saying that the hostage issue and the North Korea nuclear issue were in fact two totally unrelated issues , so it did not make any sense that Japan raised this issue ."
      (all-sentences-annotated-with-roleset "believe.01")
      :key #'sentence-string
      :test #'string-equal))




(learn-cxn-from-propbank-annotation *S* "believe.01" *propbank-learned-cxn-inventory*)
(comprehend-and-extract-frames "Well , at the same time , uh , this editorial , uh , also believed , saying it , so it works  ." :cxn-inventory *propbank-learned-cxn-inventory*)

(activate-monitor trace-fcg-search-process)

;; (learn-propbank-grammar (all-sentences-annotated-with-roleset "believe.01") '("believe.01"))


;; (add-element (make-html *propbank-learned-cxn-inventory*))



(setf s-1 (left-pole-structure fcg::*x*))

(setf s-2 (left-pole-structure fcg::*y*))

(print s-1)




(fcg::equivalent-coupled-feature-structures-aux 
 (make-instance 'coupled-feature-structure
                :left-pole s-1
                :right-pole '((root)))
 (make-instance 'coupled-feature-structure
                :left-pole *t2*
                :right-pole '((root))))






(setf *t1*
      '((#:|.-1422| (NODE-TYPE LEAF) (STRING ".") (SPAN (23 24)) (PARENT #:S-4920) (HEAD #:WORKS-7) (DEPENDENTS NIL) (LEMMA \.) (LEX-CLASS \.) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:WORKS-7 (NODE-TYPE LEAF) (STRING "works") (SPAN (22 23)) (PARENT #:S-4921) (HEAD #:BELIEVED-405) (DEPENDENTS (#:IT-609 #:|.-1422|)) (LEMMA WORK) (LEX-CLASS VBZ) (DEPENDENCY-LABEL CONJ) (NAMED-ENTITY-TYPE NIL)) (#:IT-609 (NODE-TYPE LEAF) (STRING "it") (SPAN (21 22)) (PARENT #:S-4921) (HEAD #:WORKS-7) (DEPENDENTS NIL) (LEMMA -PRON-) (LEX-CLASS PRP) (DEPENDENCY-LABEL NSUBJ) (NAMED-ENTITY-TYPE NIL)) (#:S-4921 (NODE-TYPE PHRASE) (STRING "it works") (SPAN (21 23)) (PARENT #:SBAR-2355) (CONSTITUENTS (#:IT-609 #:WORKS-7)) (PHRASE-TYPE (S)) (WORD-ORDER ((ADJACENT #:IT-609 #:WORKS-7) (PRECEDES #:IT-609 #:WORKS-7)))) (#:SO-215 (NODE-TYPE LEAF) (STRING "so") (SPAN (20 21)) (PARENT #:SBAR-2355) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA SO) (LEX-CLASS IN) (DEPENDENCY-LABEL CC) (NAMED-ENTITY-TYPE NIL)) (#:SBAR-2355 (NODE-TYPE PHRASE) (STRING "so it works") (SPAN (20 23)) (PARENT #:S-VP-1365) (CONSTITUENTS (#:SO-215 #:S-4921)) (PHRASE-TYPE (SBAR)) (WORD-ORDER ((ADJACENT #:SO-215 #:S-4921) (PRECEDES #:SO-215 #:S-4921)))) (#:\,-1698 (NODE-TYPE LEAF) (STRING ",") (SPAN (19 20)) (PARENT #:S-VP-1365) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:IT-608 (NODE-TYPE LEAF) (STRING "it") (SPAN (18 19)) (PARENT #:S-VP-1365) (HEAD #:SAYING-45) (DEPENDENTS NIL) (LEMMA -PRON-) (LEX-CLASS PRP) (DEPENDENCY-LABEL DOBJ) (NAMED-ENTITY-TYPE NIL)) (#:SAYING-45 (NODE-TYPE LEAF) (STRING "saying") (SPAN (17 18)) (PARENT #:S-VP-1365) (HEAD #:BELIEVED-405) (DEPENDENTS (#:IT-608)) (LEMMA SAY) (LEX-CLASS VBG) (DEPENDENCY-LABEL ADVCL) (NAMED-ENTITY-TYPE NIL)) (#:S-VP-1365 (NODE-TYPE PHRASE) (STRING "saying it , so it works") (SPAN (17 23)) (PARENT #:S-4920) (CONSTITUENTS (#:SAYING-45 #:IT-608 #:\,-1698 #:SBAR-2355)) (PHRASE-TYPE (S VP)) (WORD-ORDER ((ADJACENT #:SAYING-45 #:IT-608) (ADJACENT #:IT-608 #:\,-1698) (ADJACENT #:\,-1698 #:SBAR-2355) (PRECEDES #:SAYING-45 #:IT-608) (PRECEDES #:SAYING-45 #:\,-1698) (PRECEDES #:IT-608 #:\,-1698) (PRECEDES #:SAYING-45 #:SBAR-2355) (PRECEDES #:IT-608 #:SBAR-2355) (PRECEDES #:\,-1698 #:SBAR-2355)))) (#:\,-1697 (NODE-TYPE LEAF) (STRING ",") (SPAN (16 17)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:BELIEVED-405 (NAMED-ENTITY-TYPE NIL) (DEPENDENCY-LABEL ROOT) (LEX-CLASS VBD) (LEMMA BELIEVE) (DEPENDENTS (#:WELL-133 #:\,-1692 #:AT-126 #:\,-1693 #:UH-535 #:\,-1694 #:EDITORIAL-45 #:\,-1695 #:UH-536 #:\,-1696 #:ALSO-103 #:\,-1697 #:SAYING-45 #:\,-1698 #:SO-215 #:WORKS-7)) (HEAD #:BELIEVED-405) (PARENT #:S-4920) (SPAN (15 16)) (STRING "believed") (NODE-TYPE LEAF) (FRAME-EVOKING +) (MEANING ((FRAME-ELEMENT ARGM-DIS #:?F-56548 #:WELL-133) (FRAME-ELEMENT ARGM-TMP #:?F-56548 #:PP-3316) (FRAME-ELEMENT ARGM-DIS #:?F-56548 #:UH-535) (FRAME-ELEMENT ARG0 #:?F-56548 #:NP-8730) (FRAME-ELEMENT ARGM-DIS #:?F-56548 #:UH-536) (FRAME-ELEMENT ARGM-ADV #:?F-56548 #:ALSO-103) (FRAME BELIEVE.01 #:?F-56548) (FRAME-ELEMENT ARGM-PRD #:?F-56548 #:S-VP-1365))) (ARGS ((:ARGM-DIS #:WELL-133) (:ARGM-TMP #:PP-3316) (:ARGM-DIS #:UH-535) (:ARG0 #:NP-8730) (:ARGM-DIS #:UH-536) (:ARGM-ADV #:ALSO-103) (REFERENT #:?F-56548) (:ARGM-PRD #:S-VP-1365))) (FOOTPRINTS ("ARGM-DIS:RB+ARGM-TMP:PP+ARGM-DIS:UH+ARG0:NP+ARGM-DIS:UH+ARGM-ADV:RB+V:VBD+ARGM-PRD:SVP-cxn"))) (#:ALSO-103 (NODE-TYPE LEAF) (STRING "also") (SPAN (14 15)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA ALSO) (LEX-CLASS RB) (DEPENDENCY-LABEL ADVMOD) (NAMED-ENTITY-TYPE NIL)) (#:\,-1696 (NODE-TYPE LEAF) (STRING ",") (SPAN (13 14)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:UH-536 (NODE-TYPE LEAF) (STRING "uh") (SPAN (12 13)) (PARENT #:INTJ-84) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA UH) (LEX-CLASS UH) (DEPENDENCY-LABEL INTJ) (NAMED-ENTITY-TYPE NIL)) (#:\,-1695 (NODE-TYPE LEAF) (STRING ",") (SPAN (11 12)) (PARENT #:INTJ-84) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:EDITORIAL-45 (NODE-TYPE LEAF) (STRING "editorial") (SPAN (10 11)) (PARENT #:NP-8730) (HEAD #:BELIEVED-405) (DEPENDENTS (#:THIS-388)) (LEMMA EDITORIAL) (LEX-CLASS NN) (DEPENDENCY-LABEL NSUBJ) (NAMED-ENTITY-TYPE NIL)) (#:THIS-388 (NODE-TYPE LEAF) (STRING "this") (SPAN (9 10)) (PARENT #:NP-8730) (HEAD #:EDITORIAL-45) (DEPENDENTS NIL) (LEMMA THIS) (LEX-CLASS DT) (DEPENDENCY-LABEL DET) (NAMED-ENTITY-TYPE NIL)) (#:NP-8730 (NODE-TYPE PHRASE) (STRING "this editorial") (SPAN (9 11)) (PARENT #:INTJ-84) (CONSTITUENTS (#:THIS-388 #:EDITORIAL-45)) (PHRASE-TYPE (NP)) (WORD-ORDER ((ADJACENT #:THIS-388 #:EDITORIAL-45) (PRECEDES #:THIS-388 #:EDITORIAL-45)))) (#:\,-1694 (NODE-TYPE LEAF) (STRING ",") (SPAN (8 9)) (PARENT #:INTJ-84) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:UH-535 (NODE-TYPE LEAF) (STRING "uh") (SPAN (7 8)) (PARENT #:INTJ-84) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA UH) (LEX-CLASS UH) (DEPENDENCY-LABEL INTJ) (NAMED-ENTITY-TYPE NIL)) (#:INTJ-84 (NODE-TYPE PHRASE) (STRING "uh , this editorial , uh") (SPAN (7 13)) (PARENT #:S-4920) (CONSTITUENTS (#:UH-535 #:\,-1694 #:NP-8730 #:\,-1695 #:UH-536)) (PHRASE-TYPE (INTJ)) (WORD-ORDER ((ADJACENT #:UH-535 #:\,-1694) (ADJACENT #:\,-1694 #:NP-8730) (ADJACENT #:NP-8730 #:\,-1695) (ADJACENT #:\,-1695 #:UH-536) (PRECEDES #:UH-535 #:\,-1694) (PRECEDES #:UH-535 #:NP-8730) (PRECEDES #:\,-1694 #:NP-8730) (PRECEDES #:UH-535 #:\,-1695) (PRECEDES #:\,-1694 #:\,-1695) (PRECEDES #:NP-8730 #:\,-1695) (PRECEDES #:UH-535 #:UH-536) (PRECEDES #:\,-1694 #:UH-536) (PRECEDES #:NP-8730 #:UH-536) (PRECEDES #:\,-1695 #:UH-536)))) (#:\,-1693 (NODE-TYPE LEAF) (STRING ",") (SPAN (6 7)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:TIME-94 (NODE-TYPE LEAF) (STRING "time") (SPAN (5 6)) (PARENT #:NP-8729) (HEAD #:AT-126) (DEPENDENTS (#:THE-2505 #:SAME-39)) (LEMMA TIME) (LEX-CLASS NN) (DEPENDENCY-LABEL POBJ) (NAMED-ENTITY-TYPE NIL)) (#:SAME-39 (NODE-TYPE LEAF) (STRING "same") (SPAN (4 5)) (PARENT #:NP-8729) (HEAD #:TIME-94) (DEPENDENTS NIL) (LEMMA SAME) (LEX-CLASS JJ) (DEPENDENCY-LABEL AMOD) (NAMED-ENTITY-TYPE NIL)) (#:THE-2505 (NODE-TYPE LEAF) (STRING "the") (SPAN (3 4)) (PARENT #:NP-8729) (HEAD #:TIME-94) (DEPENDENTS NIL) (LEMMA THE) (LEX-CLASS DT) (DEPENDENCY-LABEL DET) (NAMED-ENTITY-TYPE NIL)) (#:NP-8729 (NODE-TYPE PHRASE) (STRING "the same time") (SPAN (3 6)) (PARENT #:PP-3316) (CONSTITUENTS (#:THE-2505 #:SAME-39 #:TIME-94)) (PHRASE-TYPE (NP)) (WORD-ORDER ((ADJACENT #:THE-2505 #:SAME-39) (ADJACENT #:SAME-39 #:TIME-94) (PRECEDES #:THE-2505 #:SAME-39) (PRECEDES #:THE-2505 #:TIME-94) (PRECEDES #:SAME-39 #:TIME-94)))) (#:AT-126 (NODE-TYPE LEAF) (STRING "at") (SPAN (2 3)) (PARENT #:PP-3316) (HEAD #:BELIEVED-405) (DEPENDENTS (#:TIME-94)) (LEMMA AT) (LEX-CLASS IN) (DEPENDENCY-LABEL PREP) (NAMED-ENTITY-TYPE NIL)) (#:PP-3316 (NODE-TYPE PHRASE) (STRING "at the same time") (SPAN (2 6)) (PARENT #:S-4920) (CONSTITUENTS (#:AT-126 #:NP-8729)) (PHRASE-TYPE (PP)) (WORD-ORDER ((ADJACENT #:AT-126 #:NP-8729) (PRECEDES #:AT-126 #:NP-8729)))) (#:\,-1692 (NODE-TYPE LEAF) (STRING ",") (SPAN (1 2)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA \,) (LEX-CLASS \,) (DEPENDENCY-LABEL PUNCT) (NAMED-ENTITY-TYPE NIL)) (#:WELL-133 (NODE-TYPE LEAF) (STRING "Well") (SPAN (0 1)) (PARENT #:S-4920) (HEAD #:BELIEVED-405) (DEPENDENTS NIL) (LEMMA WELL) (LEX-CLASS RB) (DEPENDENCY-LABEL INTJ) (NAMED-ENTITY-TYPE NIL)) (#:S-4920 (NODE-TYPE PHRASE) (STRING "Well , at the same time , uh , this editorial , uh , also believed , saying it , so it works .") (SPAN (0 24)) (PARENT NIL) (CONSTITUENTS (#:WELL-133 #:\,-1692 #:PP-3316 #:\,-1693 #:INTJ-84 #:\,-1696 #:ALSO-103 #:BELIEVED-405 #:\,-1697 #:S-VP-1365 #:|.-1422|)) (PHRASE-TYPE (S)) (WORD-ORDER ((ADJACENT #:WELL-133 #:\,-1692) (ADJACENT #:\,-1692 #:PP-3316) (ADJACENT #:PP-3316 #:\,-1693) (ADJACENT #:\,-1693 #:INTJ-84) (ADJACENT #:INTJ-84 #:\,-1696) (ADJACENT #:\,-1696 #:ALSO-103) (ADJACENT #:ALSO-103 #:BELIEVED-405) (ADJACENT #:BELIEVED-405 #:\,-1697) (ADJACENT #:\,-1697 #:S-VP-1365) (ADJACENT #:S-VP-1365 #:|.-1422|) (PRECEDES #:WELL-133 #:\,-1692) (PRECEDES #:WELL-133 #:PP-3316) (PRECEDES #:\,-1692 #:PP-3316) (PRECEDES #:WELL-133 #:\,-1693) (PRECEDES #:\,-1692 #:\,-1693) (PRECEDES #:PP-3316 #:\,-1693) (PRECEDES #:WELL-133 #:INTJ-84) (PRECEDES #:\,-1692 #:INTJ-84) (PRECEDES #:PP-3316 #:INTJ-84) (PRECEDES #:\,-1693 #:INTJ-84) (PRECEDES #:WELL-133 #:\,-1696) (PRECEDES #:\,-1692 #:\,-1696) (PRECEDES #:PP-3316 #:\,-1696) (PRECEDES #:\,-1693 #:\,-1696) (PRECEDES #:INTJ-84 #:\,-1696) (PRECEDES #:WELL-133 #:ALSO-103) (PRECEDES #:\,-1692 #:ALSO-103) (PRECEDES #:PP-3316 #:ALSO-103) (PRECEDES #:\,-1693 #:ALSO-103) (PRECEDES #:INTJ-84 #:ALSO-103) (PRECEDES #:\,-1696 #:ALSO-103) (PRECEDES #:WELL-133 #:BELIEVED-405) (PRECEDES #:\,-1692 #:BELIEVED-405) (PRECEDES #:PP-3316 #:BELIEVED-405) (PRECEDES #:\,-1693 #:BELIEVED-405) (PRECEDES #:INTJ-84 #:BELIEVED-405) (PRECEDES #:\,-1696 #:BELIEVED-405) (PRECEDES #:ALSO-103 #:BELIEVED-405) (PRECEDES #:WELL-133 #:\,-1697) (PRECEDES #:\,-1692 #:\,-1697) (PRECEDES #:PP-3316 #:\,-1697) (PRECEDES #:\,-1693 #:\,-1697) (PRECEDES #:INTJ-84 #:\,-1697) (PRECEDES #:\,-1696 #:\,-1697) (PRECEDES #:ALSO-103 #:\,-1697) (PRECEDES #:BELIEVED-405 #:\,-1697) (PRECEDES #:WELL-133 #:S-VP-1365) (PRECEDES #:\,-1692 #:S-VP-1365) (PRECEDES #:PP-3316 #:S-VP-1365) (PRECEDES #:\,-1693 #:S-VP-1365) (PRECEDES #:INTJ-84 #:S-VP-1365) (PRECEDES #:\,-1696 #:S-VP-1365) (PRECEDES #:ALSO-103 #:S-VP-1365) (PRECEDES #:BELIEVED-405 #:S-VP-1365) (PRECEDES #:\,-1697 #:S-VP-1365) (PRECEDES #:WELL-133 #:|.-1422|) (PRECEDES #:\,-1692 #:|.-1422|) (PRECEDES #:PP-3316 #:|.-1422|) (PRECEDES #:\,-1693 #:|.-1422|) (PRECEDES #:INTJ-84 #:|.-1422|) (PRECEDES #:\,-1696 #:|.-1422|) (PRECEDES #:ALSO-103 #:|.-1422|) (PRECEDES #:BELIEVED-405 #:|.-1422|) (PRECEDES #:\,-1697 #:|.-1422|) (PRECEDES #:S-VP-1365 #:|.-1422|))))))
















(setf *T1* (print s-1))



(setf *T2* (print s-2))
 