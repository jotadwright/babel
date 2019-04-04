;; Here is a copy of the drawing (lpp_1943.4)

;(ql:quickload :amr-grammar)
(in-package :amr-grammar)
(activate-monitor trace-fcg)



;; Grammar
 
(def-fcg-constructions amr-grammar
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

  ;; here is X
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
  )

;;target meaning:
;;(BE-LOCATED-AT-91 B) (THING T2) (COPY-01 C) (PICTURE P) (DRAW-01 D) (HERE H) (:ARG1 B T2) (:ARG2 B H) (:ARG2-OF T2 C) (:ARG1 C P) (:ARG1-OF P D))

;(comprehend "here is a copy of the drawing")
;(print (predicates->penman (comprehend "here is a copy of the drawing")))

#|Here is a copy of the drawing . (lpp_1943.4)
(b / be-located-at-91
      :ARG1 (t2 / thing
            :ARG2-of (c / copy-01
                  :ARG1 (p / picture
                        :ARG1-of (d / draw-01))))
      :ARG2 (h / here)))|#


