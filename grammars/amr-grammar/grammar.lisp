;;AMR grammar Banarescu Corpus developed by Martina Galletti (Spring 2019)
;;-------------------------------------------------------
;; Lexical constructions covered so far:
;; - investor-cxn
;; - bond-cxn
;; - Zintan-cxn
;; - city-cxn
;; - President-cxn
;; - president-cxn
;; - Obama-cxn
;; - Mollie-cxn
;; - Brown-cxn
;; - atom-cxn
;; - atomic-cxn
;; - bomb-cxn
;; - history-cxn
;; - professor-cxn
;; - teacher-cxn
;; - spy-cxn
;; - attractive-cxn
;; - edible-cxn
;; - sandwich-cxn
;; - fund-cxn
;; - taxable-cxn
;; - the-cxn
;; - boy-cxn
;; - cannot-cxn
;; - go-cxn
;; - girl-cxn
;; - what-cxn
;; - opined-cxn
;; - opinion-cxn
;; - marble-cxn
;; - is-cxn
;; - white-cxn
;; - pleasing-cxn
;; - girls-cxn
;; - though-cxn
;; - comment-cxn
;; - innappropriate-cxn
;; - works-cxn
;; - hard-cxn
;;--------------------------------------------------------
;; Grammatical constructions covered so far:
;; - compound-noun+nominalised-verb-cxn
;; - named-entity-title-person-cxn
;; - adjective-noun-nominalisation-cxn
;; - pertainym-adjective-noun-cxn
;; - compound-noun-noun-cxn
;; - compound-noun-noun-topic-cxn
;; - named-entity-city-cxn
;; - named-entity-title-person-cxn
;; - NP-cxn
;; - adjective-noun-arg1of-morethanpossibility-cxn
;; - adjective-noun-unit-arg0of-cxn
;; - adjective-noun-arg1of-cxn
;; - article-adjective-cxn
;; - adjective-noun-arg1of-morethanpossibility-cxn
;; - vp-modal-cxn
;; - subject-verb-modal-cxn

(in-package :amr-grammar)

;; Grammar
(def-fcg-constructions amr-Banarescu-grammar 
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first)
                       (:parse-goal-tests :no-strings-in-root :no-applicable-cxns))
  
  (def-fcg-cxn investor-cxn 
               ((?investor-unit
                 (referent ?p)
                 (meaning ((person ?p)
                           (invest-01 ?i)
                           (:arg0-of ?p ?i)))
                 (sem-valence (arg0-of ?i)) ;;a person or thing that does something
                 (syn-cat (lex-class noun)
                          (nominalisation +)
                          (number sg)
                          (syn-function ?func))
                 (sem-cat (sem-class person)))
                <-
                (?investor-unit
                 --
                 (HASH form ((string ?investor-unit "investor"))))))

  (def-fcg-cxn bond-cxn
               ((?bond-unit
                 (referent ?b)
                 (meaning ((bond ?b)))
                 (syn-cat (lex-class noun)
                          (nominalisation -)
                          (number sg)
                          (syn-function ?func))
                 (sem-cat (sem-class object)))
                <-
                (?bond-unit
                 --
                 (HASH form ((string ?bond-unit "bond"))))))

  (def-fcg-cxn compound-noun+nominalised-verb-cxn ;;bond investor
               ((?compound-noun-unit
                 (referent ?ref)
                 (meaning ((:arg1 ?event ?type)))
                 (sem-cat (sem-class ?class))
                 (syn-cat (lex-class noun)
                          (nominalisation +)
                          (compound +)
                          (number ?numb)
                          (syn-function ?func))
                 (subunits (?first-noun-unit ?second-noun-unit)))
                <-
                (?first-noun-unit
                 --
                 (referent ?type)
                 (syn-cat (lex-class noun)))
                (?second-noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (nominalisation +)
                          (number ?numb)
                          (syn-function ?func))
                 (sem-valence (arg0-of ?event))
                 (sem-cat (sem-class ?class)))
                (?compound-noun-unit
                 --
                 (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))

  (def-fcg-cxn small-cxn
               ((?small-unit
                 (referent ?size)
                 (meaning ((small ?size)))
                 (syn-cat (lex-class adjective)
                          (syn-function ?func))
                 (sem-cat (sem-class size)))
                <-
                (?small-unit
                 --
                 (HASH form ((string ?small-unit "small"))))))
  
  (def-fcg-cxn adjective-noun-nominalisation-cxn ;;small investor
               ((?nominal-unit
                 (referent ?ref)
                 (meaning ((:manner ?event ?type)))
                 (sem-cat (sem-class ?class))
                 (syn-cat (syn-function ?func)
                          (nominalisation +)
                          (number ?numb))
                 (subunits (?adjective-unit ?noun-unit)))
                <-
                (?adjective-unit
                 --
                 (referent ?type)
                 (syn-cat (lex-class adjective)
                          (syn-function adjectival))) ;;merge
                (?noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (nominalisation +)
                          (number ?numb)
                          (syn-function ?func))
                 (sem-valence (arg0-of ?event))
                 (sem-cat (sem-class ?class)))
                (?nominal-unit
                 --
                 (HASH form ((meets ?adjective-unit ?noun-unit))))))

 (def-fcg-cxn Zintan-cxn
               ((?Zintan-unit
                 (referent ?c)
                 (syn-cat (lex-class proper-noun)
                          (syn-function nominal)
                          (phrase-type NP))
                 (sem-cat (sem-class location)))
                <-
                (?Zintan-unit
                 (HASH meaning ((city ?c)
                                (name ?n)
                                (:name ?c ?n)
                                (:op1 ?n "Zintan")))
                 --
                 (HASH form ((string ?Zintan-unit "Zintan"))))))
 
 (def-fcg-cxn named-entity-city-cxn
               ((?named-entity-city-unit
                 (subunits (?city-unit ?of-unit ?Zintan-unit ?the-unit))
                 (syn-cat (phrase-type NP)
                          (named-entity-type city)))
                <-
                (?the-unit
                 --
                 (HASH form ((string ?the-unit "the"))))
                (?city-unit
                 --
                (HASH form ((string ?city-unit "city"))))
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))
                (?Zintan-unit
                 
                 --
                 (sem-cat (sem-class location)))
                (?named-entity-city-unit
                 --
                 (HASH form ((meets ?the-unit ?city-unit)
                             (meets ?of-unit ?Zintan-unit))))))
 
 (def-fcg-cxn president-capitalized-cxn
              ((?president-unit
                (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                          (syn-function nominal))
                (sem-cat (sem-class title))
                (sem-valence (name ?n)))
               <-
               (?president-unit
                --
                 (HASH form ((string ?president-unit "President"))))))
 
 (def-fcg-cxn president-cxn
              ((?president-unit
                (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                         (syn-function nominal))
                (sem-cat (sem-class title))
                (sem-valence (name ?n)))
                <-
                (?president-unit
                 --
                 (HASH form ((string ?president-unit "president"))))))

 (def-fcg-cxn Obama-cxn
              ((?Obama-unit
                (referent ?n)
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal))
                (sem-cat (sem-class person))
                (meaning ((name ?n)
                          (:op1 ?n "Obama"))))
               <-
               (?Obama-unit
                --
                (HASH form ((string ?Obama-unit "Obama"))))))
 
 (def-fcg-cxn named-entity-title-person-cxn ;; president Obama
              ((?named-entity-unit
                (referent ?p)
                (meaning ((:name ?p ?n)))
                (subunits (?nominal-unit-1 ?nominal-unit-2))
                (syn-cat (phrase-type NP)
                         (named-entity-type person)))
               <-
               (?nominal-unit-1
                --
                (referent ?p)
                (sem-valence (name ?n))
                (syn-cat (syn-function nominal))
                (sem-cat (sem-class title)))
               (?nominal-unit-2
                --
                (referent ?n)
                (syn-cat (syn-function nominal))
                (sem-cat (sem-class person)))
               (?named-entity-unit
                --
                (HASH form ((meets ?nominal-unit-1 ?nominal-unit-2))))))
 
 (def-fcg-cxn the-cxn
              (<-
               (?the-unit
                (syn-cat (lex-class article)
                          (definite +)
                          (number ?number)
                          (syn-function ?func)) 
                --
                (HASH form ((string ?the-unit "the"))))))

 (def-fcg-cxn named-entity-title-article-person-cxn ;; Obama the president 
              ((?named-entity-article-person-unit
                (referent ?p)
                (meaning ((:name ?p ?n)))
                (subunits (?nominal-unit-11 ?nominal-unit-22 ?article-unit))
                (syn-cat (phrase-type NP)
                         (named-entity-type person))
                (sem-cat (sem-class ?class)))
               <-
               (?nominal-unit-11
                --
                (referent ?n)
                 (syn-cat (syn-function nominal))
                 (sem-cat (sem-class person)))
               (?article-unit
                --
                (syn-cat (lex-class article)
                         (definite +)))
               (?nominal-unit-22
                --
                (referent ?p)
                (sem-valence (name ?n))
                (syn-cat (syn-function nominal))
                (sem-cat (sem-class title)))
               (?named-entity-article-person-unit
                --
                 (HASH form ((precedes ?nominal-unit-11 ?article-unit)
                             (precedes ?article-unit ?nominal-unit-22))))))
 
 (def-fcg-cxn bomb-cxn
              ((?bomb-unit
                (referent ?b)
                (meaning ((bomb ?b)))
                 (sem-cat (sem-class object))
                 (syn-cat (lex-class noun)
                          (number sg)
                          (nominalisation -)
                          (syn-function nominal)))
               <-
               (?bomb-unit
                --
                (HASH form ((string ?bomb-unit "bomb"))))))

 (def-fcg-cxn atomic-cxn
              ((?atomic-unit
                (referent ?a)
                (meaning ((atom ?a)))
                (syn-cat (lex-class adjective)
                         (syn-function adjectival))
                (sem-cat (sem-class pertainym)))
               <-
               (?atomic-unit
                --
                (HASH form ((string ?atomic-unit "atomic"))))))
 
 (def-fcg-cxn pertainym-adjective-noun-cxn ;;atomic bomb
              ((?pertainym-adjective-noun-unit
                (referent ?ref)
                (meaning ((:mod ?ref ?type)))
                (syn-cat (syn-function nominal)
                         (number ?nb))
                (subunits (?adjective-unit ?noun-unit)))
                <-
                (?adjective-unit
                 --
                 (referent ?type)
                 (syn-cat (lex-class adjective))
                 (sem-cat (sem-class pertainym)))
                (?noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (number ?nb)
                          (syn-function nominal)))
                (?pertainym-adjective-noun-unit
                 --
                 (HASH form ((meets ?adjective-unit ?noun-unit))))))
 
 (def-fcg-cxn atom-cxn
              ((?atom-unit
                (referent ?a)
                (meaning ((atom ?a)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (nominalisation -)
                          (syn-function adjectival))
                (sem-cat (sem-class object)))
               <-
               (?atom-unit
                --
                (HASH form ((string ?atom-unit "atom"))))))
  
 (def-fcg-cxn compound-noun-noun-cxn ;;atom bomb
              ((?compound-noun-noun-unit
                (referent ?ref)
                (meaning ((:mod ?ref ?type)))
                (syn-cat (lex-class noun)
                         (compound +)
                         (number ?numb)
                         (syn-function nominal))
                (subunits (?first-noun-unit ?second-noun-unit)))
               <-
               (?first-noun-unit
                --
                (referent ?type)
                (syn-cat (lex-class noun)
                         (number sg)
                         (nominalisation -)
                         (syn-function adjectival))
                (sem-cat (sem-class object)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         (number ?numb)
                         (syn-function nominal)))
               (?compound-noun-noun-unit
                --
                (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))

 (def-fcg-cxn Mollie-cxn
              ((?Mollie-unit
                (referent ?p)
                (meaning ((person ?p)
                          (name ?n)
                          (:name ?p ?n)
                          (:op1 ?n "Mollie")))
                (sem-valence (name ?n))
                (sem-cat (sem-class person))
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)))
               <-
               (?Mollie-unit
                --
                (HASH form ((string ?Mollie-unit "Mollie"))))))

 (def-fcg-cxn Brown-last-name-cxn
              ((?named-entity-unit
                (subunits (?brown-unit ?first-name-unit))
                (referent ?p)
                 (syn-cat (phrase-type NP)
                          (named-entity-type person)))
               (?brown-unit
                (referent ?m)
                (meaning ((:op2 ?m "Brown")))
                (sem-cat (sem-class person))
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)))
               <-
               (?first-name-unit
                --
                (referent ?p)
                (sem-valence (name ?m))
                (sem-cat (sem-class person)))
               (?brown-unit
                --
                (HASH form ((string ?brown-unit "Brown"))))
               (?named-entity-unit
                --
                (HASH form ((meets ?first-name-unit ?brown-unit))))))
 
 (def-fcg-cxn teacher-cxn
              ((?teacher-unit
                (referent ?t)
                (meaning ((person ?p)
                          (teach-01 ?t)
                   (:arg0-of ?p ?t)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class person)))
               <-
               (?teacher-unit
                --
                (HASH form ((string ?teacher-unit "teacher"))))))
 
 (def-fcg-cxn professor-cxn
     ((?professor-unit
       (referent ?t)
       (meaning ((person ?p)
                 (teach-01 ?t)
                 (:arg0-of ?p ?t)))
       (syn-cat (lex-class noun)
                (number sg)
                (syn-function ?func))
       (sem-cat (sem-class person)))
      <-
      (?professor-unit
       --
       (HASH form ((string ?professor-unit "professor"))))))
 
 (def-fcg-cxn history-cxn
              ((?history-unit
                (referent ?h)
                (meaning ((history ?h)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class topic)))
               <-
               (?history-unit
                --
                (HASH form ((string ?history-unit "history"))))))
 
 (def-fcg-cxn compound-noun-noun-topic-cxn ;;history teacher
              ((?compound-noun-unit
                (referent ?ref)
                (meaning ((:arg1 ?ref ?topic)))
                (sem-cat (sem-class ?class))
                (syn-cat (lex-class noun)
                         (compound +)
                         (number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class ?class))
                (subunits (?first-noun-unit ?second-noun-unit)))
               <-
               (?first-noun-unit
                --
                (referent ?topic)
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class topic)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         (number ?numb)
                         (syn-function nominal)))
               (?compound-noun-unit
                --
                (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))
      
 (def-fcg-cxn spy-cxn
              ((?spy-unit
                (referent ?s)
                (meaning ((spy ?s)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class person)))
               <-
               (?spy-unit
                --
                (HASH form  ((string ?spy-unit "spy"))))))
  
 (def-fcg-cxn attractive-cxn
              ((?attractive-unit
                (referent ?a)
                (meaning ((attract-01 ?a)))
                (syn-cat (lex-class adjective)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class quality)))
               <-
               (?attractive-unit
                --
                (HASH form  ((string ?attractive-unit "attractive"))))))
 
 (def-fcg-cxn adjective-noun-unit-arg0of-cxn ;;attractive spy
              ((?adjective-noun-unit 
                (referent ?ref)
                (meaning ((:arg0-of ?ref ?quality)))
                (sem-cat (sem-class ?class))
                (syn-cat (phrase-type NP)
                         (syn-function ?func)
                         (number ?numb))
                (subunits (?adjective-unit ?noun-unit)))
               <-
               (?adjective-unit
                --
                (referent ?quality)
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func))
                (sem-cat (sem-class quality)))
               (?noun-unit
                --
                (referent ?ref)
                (syn-cat  (lex-class noun)
                          (number ?numb)
                          (syn-function nominal)))
               (?adjective-noun-unit 
                --
                (HASH form ((meets ?adjective-unit ?noun-unit))))))
 
 (def-fcg-cxn an-cxn
              (<-
               (?an-unit
                (syn-cat (lex-class article)
                         (definite -)
                         (number sg))
                --
                (HASH form ((string ?an-unit "an"))))))
 
 (def-fcg-cxn edible-cxn
              ((?edible-unit
                (referent ?e)
                (meaning ((eat-01 ?e)
                          (possible ?p)
                          (:domain-of ?e ?p)))
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func))
                (sem-cat (sem-class possibility)))
               <-
               (?edible-unit
                --
                (HASH form ((string ?edible-unit "edible"))))))

 (def-fcg-cxn sandwich-cxn
              ((?sandwich-unit
                (referent ?s)
                (meaning ((sandwich ?s)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func)))
               <-
               (?sandwich-unit
                --
                (HASH form ((string ?sandwich-unit "sandwich"))))))
             
 (def-fcg-cxn adjective-noun-arg1of-cxn ;; edible sandwich 
              ((?adjective-noun-arg1of-unit
                (referent ?s)
                (meaning ((:arg1-of ?ref ?p)))
                (syn-cat (phrase-type NP)
                         (number ?numb))
                (subunits (?adjective-unit-1 ?noun-unit-1)))
               <-
               (?adjective-unit-1
                --
                (referent ?p)
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func))
                (sem-cat (sem-class possibility)))
               (?noun-unit-1
                --
                (referent ?ref)
                (syn-cat  (lex-class noun)
                          (number ?numb)
                          (syn-function ?func)))
               (?adjective-noun-arg1of-unit 
                --
                (HASH form ((meets ?adjective-unit-1 ?noun-unit-1))))))
 
 (def-fcg-cxn a-cxn
              (<-
               (?a-unit
                (syn-cat (lex-class article)
                                   (definite -)
                                   (number sg))
                --
                (HASH form ((string ?a-unit "a"))))))
 
 (def-fcg-cxn fund-cxn
              ((?fund-unit
                (referent ?f)
                (meaning ((fund ?f)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func)))
               <-
               (?fund-unit
                --
                (HASH form  ((string ?fund-unit "fund"))))))
 
 (def-fcg-cxn taxable-cxn
              ((?taxable-unit
                (referent ?t)
                (meaning ((tax-01 ?t)))
                (syn-cat (lex-class adjective)
                         (number sg)
                         (syn-function ?func))
                (sem-cat (sem-class possibility)))
               <-
               (?taxable-unit
                --
                (HASH form  ((string ?taxable-unit "taxable"))))))
 
 (def-fcg-cxn article-adjective-cxn
              ((?article-adjective-unit
                (referent ?ref)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3))
                (subunits (?article-unit ?adjective-unit))
                (boundaries (rightmost-unit ?adjective-unit)
                            (leftmost-unit ?article-unit)))
               <-
               (?article-unit
                (referent ?ref)
                --
                (syn-cat (lex-class article)
                         (definite ?def)
                         (number ?n)))
               (?adjective-unit
                --
                (referent ?ref)
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func)))
               (?article-adjective-unit
                --
                (HASH form ((meets ?article-unit ?adjective-unit))))))
 
 (def-fcg-cxn adjective-noun-arg1of-morethanpossibility-cxn ;;taxable
              ((?adjective-noun-arg1of-morethanpossibility-unit
                (referent ?ref)
                (meaning ((:arg1-of ?ref ?possibility)))
                (sem-cat (sem-class possibility))
                (syn-cat (phrase-type NP)
                         (syn-function ?func)
                         (number ?numb))
                (subunits (?adjective11-unit ?noun-unit22)))
               <-
               (?adjective-unit11
                --
                (referent ?possibility)
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func)
                (sem-cat (sem-class possibility))))
               (?noun-unit22
                --
                (referent ?ref)
                (syn-cat  (lex-class noun)
                          (number ?numb)
                          (syn-function ?func)))
               (?adjective-noun-arg1of-morethanpossibility-unit
                            --
               (HASH form ((meets ?adjective-unit11 ?noun-unit22))))))
 
 (def-fcg-cxn NP-cxn
              ((?NP-unit
                (referent ?ref)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3)
                         (syn-function ?func))
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

 (def-fcg-cxn boy-cxn
              ((?boy-unit
                (referent ?b)
                (meaning ((boy ?b)))
                (syn-cat (phrase-type NP)
                         (lex-class noun)
                         (number sg)
                         (person 3)
                         (syn-function ?func))
                (sem-cat (sem-class person)))
               <-
               (?boy-unit
                (HASH meaning ((boy ?b)))
                --
                (HASH form ((string ?boy-unit "boy"))))))

 (def-fcg-cxn go-cxn
              ((?go-unit
                (referent ?g)
                (syn-cat (lex-class verb)
                         (finite -))
                (sem-valence (arg0 ?b))
                (meaning ((go-01 ?g)
                          (:arg0 ?g ?b))))
               <-
               (?go-unit
                --
                (HASH form ((string ?go-unit "go"))))))
 
 (def-fcg-cxn cannot-cxn
              ((?cannot-unit
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal +)
                         (phrase-type VP))
                (sem-valence (:domain ?g))
                (meaning ((possible ?p)
                          (:polarity ?p -)
                          (:domain ?p ?g))))
               <-
               (?cannot-unit
                --
                (HASH form ((string ?cannot-unit "cannot"))))))
 
 (def-fcg-cxn vp-modal-cxn ;;cannot go 
              ((?vp-modal-unit
                (referent ?c)
                (syn-cat (phrase-type VP)
                         (finite +))
                (sem-cat (sem-class event))
                (sem-valence (:arg0 ?arg0))
                (subunits (?modal-unit ?infinitif-unit))
                (boundaries (rightmost-unit ?infinitif-unit)
                            (leftmost-unit ?modal-unit)))
               <-
               (?modal-unit
                --
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal +)
                         (phrase-type VP))
                (sem-valence (:domain ?c)))
               (?infinitif-unit
                --
                (referent ?c)
                (sem-valence (:arg0 ?arg0))
                (syn-cat (lex-class verb)
                         (finite -)))
               (?vp-modal-unit
                --
                (HASH form ((meets ?modal-unit ?infinitif-unit))))))
 
 (def-fcg-cxn subject-verb-modal-cxn
              ((?subject-verb-modal-unit
                (referent ?b)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3))
                (subunits (?vp-modal-unit ?np-unit))
                (boundaries (rightmost-unit ?vp-modal-rightmost-unit)
                            (leftmost-unit ?np-leftmost-unit)))
               <-
               (?vp-modal-unit
                --
                (referent ?g)
                (syn-cat (phrase-type VP)
                         (finite +))
                (sem-valence (:arg0 ?b))
                (boundaries
                 (rightmost-unit ?vp-modal-rightmost-unit)
                 (leftmost-unit ?vp-modal-leftmost-unit)))
               (?np-unit
                --
                (referent ?b)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3)
                         (syn-function ?func))
                (boundaries
                 (leftmost-unit ?np-leftmost-unit)
                 (rightmost-unit ?np-rightmost-unit)))
               (?subject-verb-modal-unit
                --
                (HASH form ((meets ?np-rightmost-unit ?vp-modal-leftmost-unit))))))

(def-fcg-cxn what-cxn 
             ((?what-unit
               (referent ?t)
               (meaning ((thing ?t)))
               (syn-cat (lex-class pronoun)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class object)))
              <-
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))))
 
(def-fcg-cxn girl-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
             (sem-cat (sem-class possessor)))
              <-
              (?girl-unit
               (HASH meaning ((girl ?g)))
               --
               (HASH form ((string ?girl-unit "girl"))))))
              
(def-fcg-cxn opined-cxn
             ((?opined-unit
               (referent ?o)
               (meaning ((opine-01 ?o)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (past-simple +)
                        (phrase-type VP)))
               <-
               (?opined-unit
                --
                (HASH form ((string ?opined-unit "opined"))))))

(def-fcg-cxn subject-verb-cxn
             ((?subject-verb-unit
               (meaning ((:arg0 ?o ?g)))
               (subunits (?vp-unit ?subject-unit))
               (referent ?o)
               (syn-cat (phrase-type VP)))
              <-
              (?vp-unit
               --
               (referent ?o)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (past-simple +)
                        (phrase-type VP)))
              (?subject-unit
               --
               (referent ?g)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?subject-leftmost-unit)
                (rightmost-unit ?subject-rightmost-unit)))
              (?subject-verb-unit
               --
               (HASH form ((precedes ?subject-unit-rightmost-unitt ?vp-unit))))))

(def-fcg-cxn arg1of-before-transitive-verb-cxn
             ((?arg1of-before-transitive-verb-unit
               (meaning ((:arg1-of ?t ?o)))
               (subunits (?object-unit ?vp-unit)))
              <-
              (?object-unit
               --
               (referent ?t)
               (syn-cat (lex-class pronoun)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class object)))
               (?vp-unit
               --
               (referent ?o)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (past-simple +)
                        (phrase-type VP)))
              --
              (?arg1of-before-transitive-verb-unit
               (HASH form ((precedes ?object-unit ?vp-unit))))))

(def-fcg-cxn opinion-cxn
             ((?opinion-unit
               (referent ?o)
               (meaning ((opine-01 ?o)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
              (sem-class (sem-cat possessed)))
               <-
               (?opinion-unit
                --
                (HASH form ((string ?opinion-unit "opinion"))))))

(def-fcg-cxn s-possessive-cxn
             ((?s-possessive-unit
               (referent ?g)
               (syn-cat (syn-function ?func))
               (sem-cat (sem-class possess)))
               <-
               (?s-possessive-unit
                --
                (HASH form ((string ?s-unit "'s"))))))

;;the girl's opinion + the opinion of the girl  ((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G))

(def-fcg-cxn marble-cxn
             ((?marble-unit
               (referent ?m)
               (meaning ((marble ?m)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)))
              <-
              (?marble-unit
               --
               (HASH form ((string ?marble-unit "marble"))))))

(def-fcg-cxn is-cxn
             ((?is-unit
               (referent ?is)
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (phrase-type VP)))
              <-
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))))

(def-fcg-cxn white-cxn
             ((?white-unit
               (referent ?w)
               (meaning ((white ?w)))
               (syn-cat (lex-class adjective)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class colour)))
              <-
              (?white-unit
              --
              (HASH form ((string ?white-unit "white"))))))


(def-fcg-cxn comment-cxn
             ((?comment-unit
               (referent ?c)
               (meaning ((comment ?c)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)))
              <-
              (?comment-unit
               --
               (HASH form ((string ?comment-unit "comment"))))))

(def-fcg-cxn appropriate-cxn
            ((?appropriate-unit
              (referent ?a)
              (meaning ((appropriate ?a)
                        (polarity ?a -)))
              (syn-cat (lex-class adjective)
                       (number ?numb)
                       (syn-function ?func))
             (sem-cat (sem-class quality)))
             <-
             (?appropriate-unit
              --
              (HASH form ((string ?appropriate-unit "inappropriate"))))))

(def-fcg-cxn predicative-cxn 
             ((?predicative-unit
               (subunits (?vp-unit ?adjective-predicative-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?adj ?referring)))
               (referent ?ref))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (phrase-type VP)))
              (?adjective-predicative-unit
               --
               (referent ?adj)
               (syn-cat (lex-class adjective)
                          (number ?numb)
                          (syn-function ?func)))
              (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (lex-class noun)
                        (number sg)
                          (syn-function ?func)))
              (?predicative-unit
               --
               (HASH form ((precedes ?referring-noun-unit ?vp-unit )
                           (precedes ?vp-unit ?adjective-predicative-unit))))))

(def-fcg-cxn pleasing-cxn
             ((?pleasing-unit
               (referent ?p)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (gerund +)
                        (syn-function ?func)))
               <-
               (?pleasing-unit
                --
                (HASH form ((string ?pleasing-unit "pleasing"))))))
                  
(def-fcg-cxn girls-cxn
             ((?girls-unit
               (referent ?g)
               (meaning ((girls ?g)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function ?func))
               (sem-cat (sem-class arg1)))
              <-
              (?girl-unit
               --
               (HASH form ((string ?girls-unit "girls"))))))

(def-fcg-cxn tough-cxn
             ((?tough-unit
               (referent ?t)
               (meaning ((tough ?t)))
               (syn-cat (lex-class adjective)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class quality)))
              <-
              (?tough-unit
              --
              (HASH form ((string ?tough-unit "tough"))))))

(def-fcg-cxn arg1-gerund-cxn
             ((?arg1-gerund-unit
               (referent ?ref)
               (meaning ((:arg1 ?ger ?ref)))
               (syn-cat (syn-function ?arg1))
               (subunits (?gerund-unit ?arg1-unit)))
               <-
               (?gerund-unit
                (referent ?ger)
                --
                (syn-cat (lex-class noun)
                         (number sg)
                         (gerund +)
                         (syn-function ?func)))
               (?arg1-unit
                (referent ?ref)
                --
                (syn-cat (lex-class noun)
                         (number pl)
                         (syn-function ?func))
                (sem-cat (sem-class arg1)))
               (?arg1-gerund-unit
                --
                (HASH form ((precedes ?gerund-unit ?arg1-unit))))))

(def-fcg-cxn predicative-gerund-referring-entity-cxn 
             ((?predicative-gerund-referring-entity
               (subunits (?vp-unit ?adjective-predicative-unit ?gerund-referring-entity))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?adj ?referring)))
               (referent ?ref))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (phrase-type VP)))
              (?adjective-predicative-unit
               --
               (referent ?adj)
               (syn-cat (lex-class adjective)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class quality)))
              (?gerund-referring-entity
               --
               (referent ?referring)
               (syn-cat (lex-class noun)
                        (number sg)
                        (gerund +)
                        (syn-function ?func)))
              (?predicative-gerund-referring-entity
               --
               (HASH form ((precedes ?gerund-referring-entity ?vp-unit )
                           (precedes ?vp-unit ?adjective-predicative-unit))))))

(def-fcg-cxn work-cxn
             ((?work-unit
               (referent ?w)
               (meaning ((work-01 ?w)))
               (syn-cat (lex-class verb)
                        (finite +)
                         (phrase-type VP)))
              <-
              (?work-unit
               --
               (HASH form ((string ?work-unit "works"))))))

(def-fcg-cxn hard-cxn
             ((?hard-unit
               (referent ?h)
               (meaning ((hard ?h)))
               (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
              <-
              (?hard-unit
              --
              (HASH form ((string ?hard-unit "hard"))))))

(def-fcg-cxn adverb-manner-cxn
             ((?adverb-manner-unit
               (meaning ((:manner ?adv ?verb)))
               (subunits (?vp-unit ?adverb-unit))
               (syn-cat (phrase-type clausal)))
               <-
               (?vp-unit
                --
                (referent ?verb)
                (syn-cat (lex-class verb)
                         (finite +)
                          (phrase-type VP)))
               (?adverb-unit
               --
               (referent ?adv)
                (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
               (?adverb-manner-unit
                --
                (HASH form ((meets ?vp-unit ?adverb-unit))))))

(def-fcg-cxn subject-verb-adverb-cxn
             ((?subject-verb-adverb-unit
               (meaning ((:arg0 ?verb ?subject)))
               (subunits (?vp-unit ?subject-unit))
               (referent ?subject)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?subject-leftmost-unit)))
              <-
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        (finite +)
                        (phrase-type VP))
               (boundaries
                 (rightmost-unit ?vp-unit-rightmost)
                 (leftmost-unit ?vp-unit-leftmost)))
              (?subject-unit
               --
               (referent ?subject)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (lex-class noun)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?subject-unit-leftmost)
                (rightmost-unit ?subject-unit-rightmost)))
              (?subject-verb-adverb-unit
               --
               (HASH form ((precedes ?subject-unit-leftmost-unit ?vp-unit-rightmost-unit))))))

)

#|

  (def-fcg-cxn subject-verb-modal-cxn
              ((?subject-verb-modal-unit
                (referent ?b)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3))
                (subunits (?vp-modal-unit ?np-unit))
                (boundaries (rightmost-unit ?vp-modal-rightmost-unit)
                            (leftmost-unit ?np-leftmost-unit)))
               <-
               (?vp-modal-unit
                --
                (referent ?g)
                (syn-cat (phrase-type VP)
                         (finite +))
                (sem-valence (:arg0 ?b))
                (boundaries
                 (rightmost-unit ?vp-modal-rightmost-unit)
                 (leftmost-unit ?vp-modal-leftmost-unit)))
               (?np-unit
                --
                (referent ?b)
                (syn-cat (phrase-type NP)
                         (number ?n)
                         (person 3)
                         (syn-function ?func))
                (boundaries
                 (leftmost-unit ?np-leftmost-unit)
                 (rightmost-unit ?np-rightmost-unit)))
               (?subject-verb-modal-unit
                --
                (HASH form ((meets ?np-rightmost-unit ?vp-modal-leftmost-unit))))))

 ((WORK-01 W) (BOY B) (HARD H) (:ARG0 W B) (:MANNER W H)) 
 (def-fcg-cxn genitive-possessor-cxn
             ((?genitive-possessor-unit
               (referent ?g)
               (meaning ((:arg0 ?o ?g)))
               (syn-cat (syn-function ?func))
               (sem-cat (sem-class genitive-possess))
               (subunits (?possessor-unit ?possessed-unit))
               (boundaries (rightmost-unit ?possessor-rightmost-unit)
                           (leftmost-unit ?possessed-unit)))
              <-
              (?possessor-unit
               (referent ?g)
               --
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries (leftmost-unit ?possessor-leftmost-unit)
                           (rightmost-unit ?possessor-rightmost-unit))
               (sem-cat (sem-class possessor)))
              (?possessed-unit
               (referent ?o)
               --
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
               (sem-class (sem-cat possessed)))
              (?genitive-possessor-unit
               --
               (HASH form ((precedes ?possessor-rightmost-unit ?possessed-unit))))))



 (def-fcg-cxn genitive-possessor-cxn
             ((?genitive-possessor-unit
               (referent ?g)
               (meaning ((:arg0 ?o ?g)))
               (syn-cat (syn-function ?func))
               (sem-cat (sem-class genitive-possess))
               (subunits (?possessor-unit ?s-possessive-genitive-unit))
               (boundaries (rightmost-unit ?possessor-rightmost-unit ?s-possessive-genitive-unit)))
               <-
               (?possessor-unit
                --
               (referent ?g)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries (leftmost-unit ?possessor-leftmost-unit)
                           (rightmost-unit ?possessor-rightmost-unit)))
              (?s-possessive-genitive-unit
              --
              (referent ?g)
              (syn-cat (syn-function ?func))
              (sem-cat (sem-class genitive-possess)))
              --
              (?genitive-possessor-unit
              (HASH form ((precedes (?possessor-unit ?s-possessive-unit)))))))
 
 (def-fcg-cxn genitive-cxn
             ((?genitive-unit
               (referent ?g)
               (meaning ((:arg1-of ?t ?o)
                         (:arg0 ?o ?g)))
               (syn-cat (syn-function ?func))
               (sem-cat (sem-class possess))
               (subunits (?possessor-unit ?s-possessive-unit))
               (boundaries (rightmost-unit ?possessor-rightmost-unit ?s-possessive-unit)))
               <-
               (?possessor-unit
                --
               (referent ?g)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?possessor-leftmost-unit)
                (rightmost-unit ?possessor-rightmost-unit)))
              (?s-possessive-unit
              --
              (referent ?g))
              (?genitive-unit
              --
              (HASH form ((precedes (?possessor-unit ?s-possessive-unit)))))))
 
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

(def-fcg-cxn possession-cxn
           ((?possession-unit
             (referent ?ref)
             (meaning ((:arg1-of ?object ?o)
                       (:arg0 ?o ?person)))
             (subunits (?thing-possessed ?possessor))
             (sem-cat (sem-class possession))
             (sem-valence (:arg1-of ?arg1-of)
                          (:arg0 ?arg0)))
             <-
             (?thing-possessed-unit
              --
              (syn-cat (lex-class noun)
                       (number sg)
                       (syn-function ?func))
              (sem-cat (sem-class thing-possessed)))
             (?possessor-unit
             --
             (syn-cat (lex-class noun)
                      (number sg)
                      (syn-function ?func))
             (sem-cat (sem-class possessor)))
             (?possession-unit
             --
             (HASH form ((meets ?possessor-unit ?thing-possessed-unit))))))
                          

(def-fcg-cxn active-transitive-cxn ;;subject = agent
             ((?transitive-clause-unit
               (subunits (?vp-unit ?agent-unit ?patient-unit))
               (syn-cat (phrase-type clausal))
               (referent ?ref)
               (meaning )
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

 (def-fcg-cxn verb-pronoun-object-cxn
             ((?verb-pronoun-object-unit
               (referent ?ref)
               (subunits (?vp-unit ?pronoun-unit))
               (meaning ((:arg1-of ?t ?o))))
              <-
              (?vp-unit
               --
               (referent ?ref)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (past-simple +)
                        (phrase-type VP))
               (sem-valence (:arg0 ?arg0)
                            (:arg1 ?arg1)))
              (?pronoun-unit
               --
              (syn-cat (lex-class pronoun)
                       (number ?numb)
                       (syn-function ?func)))
              <-
              (?verb-pronoun-object-unit
               --
               (HASH form ((meets ?vp-unit ?pronoun-unit))))))


         
|#
            
