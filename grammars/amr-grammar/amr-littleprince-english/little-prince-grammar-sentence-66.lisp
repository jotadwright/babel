;; But I had never drawn a sheep (lpp_1943.66)

;(ql:quickload :amr-grammar)
(in-package :amr-grammar)
(activate-monitor trace-fcg)

(def-fcg-constructions amr-grammarMartina
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first))

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

  ;;Had
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

  ;;NP

  (def-fcg-cxn article-noun-cxn
               ((?NP-unit
                 (referent ?ref)
                 (syn-cat (phrase-type NP))
                 (subunits (?article-unit ?noun-unit))
                 (boundaries (rightmost-unit ?noun-unit)
                             (leftmost-unit ?article-unit)))
                <-
                (?article-unit
                 (referent ?ref)
                 --
                 (syn-cat (lex-class article)
                          (definite ?def)
                          (number ?numb)))
                (?noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (number ?numb)))
                (?NP-unit
                 --
                 (HASH form ((meets ?article-unit ?noun-unit))))))


  ;;VP
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

  ;;transitive
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


;; But I had never drawn a sheep:
;; ((CONTRAST-01 C) (DRAW-01 D) (I I) (SHEEP S) (EVER E) (:ARG2 C D) (:ARG0 D I) (:ARG1 D S) (:TIME D E) (:POLARITY D -))
;; in AMR:
#|
(c / contrast-01
   :ARG2 (d / draw-01
            :ARG0 (i / i)
            :ARG1 (s / sheep)
            :time (e / ever)
            :polarity -))
|#

(comprehend "but I had never drawn a sheep")
(print (predicates->penman (comprehend "but I had never drawn a sheep")))

;;for inspection:
*fcg-constructions*