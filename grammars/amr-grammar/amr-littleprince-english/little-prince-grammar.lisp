;;AMR grammar developed by Martina Galletti (Spring 2019)
;;-------------------------------------------------------
;; Sentences covered so far:
;;  1."But I had never drawn a sheep"
;;  2. "Here is a copy of the drawing"
;;-------------------------------------------------------
;; Nominal constructions covered so far:
;; NP : noun and article construction  
;; PP : prepositional phrase construction
;; complex NP : :ARG1 (t2 / thing :ARG2-of (c / copy-01  :ARG1 (p / picture :ARG1-of (d / draw-01))))
;;-------------------------------------------------------
;; Verbal constructions covered so far:
;; Perfect : auxiliaries and main verb 
;; Subject-verb : Subject Verb order
;; Here is X : Presentative construction
;; Active transitive : Transitive verb construction (subject= agent)
;; but-initial : contrastive clause
;;--------------------------------------------------------
;; Single lemma constructions covered so far:
;; Locational adverbs "Here",
;; Frequency adverbs "Never"
;; Auxialiares "Is", "Had", 
;; Verbs "Drawn"
;; Articles "a", "The"
;; Nouns "Copy", "Drawing", "Sheep"
;; Prepositions "Of"
;; Pronouns "I"
;;--------------------------------------------------------
;(ql:quickload :amr-grammar)
(in-package :amr-grammar)
(activate-monitor trace-fcg)

;; Grammar
 
(def-fcg-constructions amr-little-prince-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first))

  ;;here
  (def-fcg-cxn here-cxn
               ((?here-unit
                 (referent ?h)
                 (sem-cat (sem-class place-adverb))
                 (syn-cat (lex-class adverb)
                          (phrase-type ADVP)))
                <-
                (?here-unit
                 (HASH meaning ((here ?h)))
                 --
                 (HASH form ((string ?here-unit "here"))))))

 
  ;;is       
  (def-fcg-cxn is-cxn
               ((?is-unit
                 (referent ?is)
                 (syn-cat (lex-class verb)
                          (is-copular +))
                 (sem-valence (arg0 ?arg0)
                              (arg1 ?arg1)))
                <-
                (?is-unit
                 (HASH meaning ((be-located-at-91 ?is)
                                (:arg0 ?is ?arg0)
                                (:arg1 ?is ?arg1)))
                 --
                 (HASH form ((string ?is-unit "is"))))))

  ;;a
  (def-fcg-cxn a-cxn
               (<-
                (?a-unit
                 (syn-cat (lex-class article)
                          (definite -)
                          (number sg))
                 --
                 (HASH form ((string ?a-unit "a")))))
               :cxn-set morph)
              
  ;;copy
  (def-fcg-cxn copy-cxn
               ((?copy-unit
                 (referent ?c)
                 (syn-cat (lex-class noun)
                          (number ?nb)
                          (person 3))
                 (lex-id copy-01)
                 (sem-cat (sem-class object)))
                <-
                (?copy-unit
                 (HASH meaning ((copy-01 ?c)))
                 --
                 (HASH form ((string ?copy-unit "copy"))))))

  ;;of
  (def-fcg-cxn of-cxn
               ((?of-unit
                 (syn-cat (lex-class preposition)
                          (phrase-type PP)))
                <-
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))))
             

  ;;the
  (def-fcg-cxn the-cxn
               (<-
                (?the-unit
                 (syn-cat (lex-class article)
                          (definite +)
                          (number ?number)) 
                 --
                 (HASH form ((string ?the-unit "the"))))))
  
  ;;drawing
  (def-fcg-cxn drawing-cxn
               ((?drawing-unit
                 (referent ?p)
                 (syn-cat (lex-class noun)
                          (number sg)
                          (person 3))
                 (sem-cat (sem-class object))
                 (sem-valence (arg1-of ?d)))
                <-
                (?drawing-unit
                 (HASH meaning ((picture ?p)
                                (:arg1-of ?p ?d)
                                (draw-01 ?d)))
                 --
                 (HASH form ((string ?drawing-unit "drawing"))))))

  ;;NP
  (def-fcg-cxn NP-cxn
               ((?NP-unit
                 (referent ?ref)
                 (syn-cat (phrase-type NP)
                          (number ?n)
                          (person 3))
                 (sem-cat (sem-class referring-expression))
                 (subunits (?article-unit ?noun-unit))
                 (boundaries (rightmost-unit ?noun-unit)
                             (leftmost-unit ?article-unit)))
                <-
                (?article-unit
                 (referent ?ref)
                 --
                 (syn-cat (lex-class article)
                          (definite ?def)
                          (number ?n)))
                (?noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (number ?n)))
                (?NP-unit
                 --
                 (HASH form ((meets ?article-unit ?noun-unit))))))

  ;;a copy of the drawing
  (def-fcg-cxn complex-NP-cxn
               ((?thing-unit
                 (referent ?t)
                 (syn-cat (phrase-type NP)
                          (number ?nb)
                          (person 3))
                 (sem-cat (sem-class referring-expression))
                 (meaning ((thing ?t)
                           (:arg2-of ?t ?c)))
                 (subunits (?a-copy-unit ?of-unit ?the-drawing-unit)))
                (?a-copy-unit
                 (meaning ((:arg1 ?c ?p))))
                <-
                (?thing-unit
                 --
                 (HASH form ((meets ?copy-unit ?of-unit)
                             (meets ?of-unit ?the-unit))))
                (?a-copy-unit
                 --
                 (referent ?c)
                 (syn-cat (phrase-type NP)
                          (number ?nb))
                 (boundaries (rightmost-unit ?copy-unit)))
                (?of-unit
                 --
                 (syn-cat (lex-class preposition)))
                (?the-drawing-unit
                 --
                 (referent ?p)
                 (boundaries (leftmost-unit ?the-unit))
                 (syn-cat (phrase-type NP)))))

  ;;here is X
  (def-fcg-cxn presentative-cxn 
               ((?clause-unit
                 (subunits (?here-unit ?located-at-unit ?thing-unit)))
                (?located-at-unit
                 (meaning ((:arg2 ?ev ?loc)))
                 (sem-valence (arg2 ?loc)))
                <-
                (?clause-unit
                 --
                 (HASH form ((meets ?here-unit ?located-at-unit)
                             (meets ?located-at-unit ?leftmost-object-unit))))
                (?here-unit
                 --
                 (referent ?loc)
                 (sem-cat (sem-class place-adverb))
                 (syn-cat (lex-class adverb)))
                (?located-at-unit
                 --
                 (referent ?ev)
                 (syn-cat (lex-class verb))
                 (sem-valence (arg0 ?arg0)
                              (arg1 ?ref)))
                (?thing-unit
                 --
                 (referent ?ref)
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (number ?nb)
                          (person 3)
                          (phrase-type NP))
                 (boundaries (leftmost-unit ?leftmost-object-unit)))))

  ;;I
  (def-fcg-cxn I-cxn
               ((?I-unit
                 (referent ?I)
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (lex-class proper-noun)
                          (phrase-type NP)))
                <-
                (?I-unit
                 (HASH meaning ((I ?I)))
                 --
                 (HASH form ((string ?I-unit "I"))))))

  ;;had
  (def-fcg-cxn had-cxn
               ((?had-unit
                 (syn-cat (lex-class aux)
                          (finite ?fin)
                          (lemma have)))
                <-
                (?had-unit
                 --
                 (HASH form ((string ?had-unit "had"))))))
                          

  ;;drawn
  (def-fcg-cxn drawn-cxn
               ((?drawn-unit
                 (referent ?d)
                 (syn-cat (lex-class verb)
                          (finite -))
                 (sem-valence (arg0 ?arg0)
                              (arg1 ?arg1)))
                <-
                (?drawn-unit
                 (HASH meaning ((drawn-01 ?d)
                                (:arg0 ?d ?arg0)
                                (:arg1 ?d ?arg1)))
                 --
                 (HASH form ((string ?drawn-unit "drawn"))))))

  ;;a
  (def-fcg-cxn a-cxn
               (<-
                (?a-unit
                 (syn-cat (lex-class article)
                          (definite -)
                          (number sg))
                 --
                 (HASH form ((string ?a-unit "a")))))
               :cxn-set morph)

  ;;sheep
  (def-fcg-cxn sheep-cxn
               ((?sheep-unit
                 (referent ?sh)
                 (syn-cat (lex-class noun)
                          (number ?nb)
                          (person 3))
                 (sem-cat (sem-class object)))
                <-
                (?sheep-unit
                 (HASH meaning ((sheep ?sh)))
                 --
                 (HASH form ((string ?sheep-unit "sheep"))))))


  ;;perfect 
  (def-fcg-cxn perfect-vp-cxn
               ((?vp-unit
                 (subunits (?aux-unit ?main-verb-unit))
                 (referent ?d)
                 (syn-cat (phrase-type VP))
                 (sem-cat (sem-class event))
                 (sem-valence (arg0 ?arg0)
                              (arg1 ?arg1))
                 (boundaries (rightmost-unit ?main-verb-unit)
                             (leftmost-unit ?aux-unit)))
                <-
                (?aux-unit
                 --
                 (syn-cat (lex-class aux)
                          (lemma have)))
                (?main-verb-unit
                 --
                 (referent ?d)
                 (syn-cat (lex-class verb)
                          (finite -))
                 (sem-valence (arg0 ?arg0)
                              (arg1 ?arg1)))
                (?vp-unit
                 --
                 (HASH form ((precedes ?aux-unit ?main-verb-unit))))))

  ;;subject-verb unit
  (def-fcg-cxn subject-verb-cxn
               ((?vp-unit
                 (syn-valence (subject-unit ?subject-unit)))
                <-
                (?vp-unit
                 --
                 (syn-cat (phrase-type VP))
                 (boundaries (leftmost-unit ?verb-unit)))
                (?subject-unit
                 --
                 (syn-cat (phrase-type NP))
                 (HASH form ((precedes ?subject-unit ?verb-unit))))))

  ;;active transitive
  (def-fcg-cxn active-transitive-cxn ;;subject = agent
               ((?transitive-clause-unit
                 (subunits (?vp-unit ?agent-unit ?patient-unit))
                 (syn-cat (phrase-type clausal))
                 (referent ?ref))
                <-
                (?vp-unit
                 --
                 (referent ?ref)
                 (syn-cat (phrase-type VP))
                 (syn-valence (subject-unit ?agent-unit)) ;;link subject to agent
                 (sem-valence (arg0 ?agent)
                              (arg1 ?patient)))
                (?agent-unit
                 --
                 (referent ?agent)
                 (syn-cat (phrase-type NP)))
                (?patient-unit
                 --
                 (referent ?patient)
                 (syn-cat (phrase-type NP)))))
  ;;never 
  (def-fcg-cxn never-cxn
               ((?event-unit
                 (sem-valence (time ?time)
                              (polarity -))
                 (subunits (?never-unit)))
                <-
                (?event-unit
                 (HASH meaning ((:time ?ev ?time)
                                (:polarity ?ev -)))
                 --
                 (referent ?ev)
                 (sem-cat (sem-class event))
                 (syn-cat (phrase-type VP)))
                (?never-unit
                 (HASH meaning ((ever ?time)))
                 --
                 (HASH form ((string ?never-unit "never"))))))

  ;;but-initial
  (def-fcg-cxn but-initial-cxn 
               ((?contrastive-clause-unit
                 (referent ?c)
                 (syn-cat (phrase-type clausal)
                          (discourse-function contrast))
                 (subunits (?but-unit ?clausal-unit)))
                (?but-unit
                 (referent ?c)
                 (sem-valence (arg2 ?ev)))
                <-
                (?but-unit
                 (HASH meaning ((contrast ?c)
                                (:arg2 ?c ?ev)))
                 --
                 (HASH form ((string ?but-unit "but")
                             (first ?but-unit))))
                (?clausal-unit
                 --
                 (syn-cat (phrase-type clausal))
                 (referent ?ev))))

)

;(comprehend "here is a copy of the drawing" "but I had never drawn a sheep")
(print (predicates->penman (comprehend "here is a copy of the drawing" "but I had never drawn a sheep")


;; AMR and predicate notations of the sentences covered :
                   
(print (penman->predicates '(c / city :name (n / name :op1 "Zintan"))))

#|
;;Here is a copy of the drawing
;;in predicate
((AMR-GRAMMAR::BE-LOCATED-AT-91 AMR-GRAMMAR::B) (AMR-GRAMMAR::THING AMR-GRAMMAR::T2) (AMR-GRAMMAR::COPY-01 AMR-GRAMMAR::C) (AMR-GRAMMAR::PICTURE AMR-GRAMMAR::P) (AMR-GRAMMAR::DRAW-01 AMR-GRAMMAR::D) (AMR-GRAMMAR::HERE AMR-GRAMMAR::H) (:ARG1 AMR-GRAMMAR::B AMR-GRAMMAR::T2) (:ARG2 AMR-GRAMMAR::B AMR-GRAMMAR::H) (:ARG2-OF AMR-GRAMMAR::T2 AMR-GRAMMAR::C) (:ARG1 AMR-GRAMMAR::C AMR-GRAMMAR::P) (:ARG1-OF AMR-GRAMMAR::P AMR-GRAMMAR::D))
;;in AMR:
(b / be-located-at-91
      :ARG1 (t2 / thing
            :ARG2-of (c / copy-01
                  :ARG1 (p / picture
                        :ARG1-of (d / draw-01))))
      :ARG2 (h / here)))



;;But I had never drawn a sheep
;;in predicate
;;((CONTRAST-01 C) (DRAW-01 D) (I I) (SHEEP S) (EVER E) (:ARG2 C D) (:ARG0 D I) (:ARG1 D S) (:TIME D E) (:POLARITY D -))
;; in AMR:
(c / contrast-01
   :ARG2 (d / draw-01
            :ARG0 (i / i)
            :ARG1 (s / sheep)
            :time (e / ever)
            :polarity -))|#