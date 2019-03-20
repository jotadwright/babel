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
;; - soldier-cxn
;; - feared-cxn
;; - battle-cxn
;; - woman-cxn
;; - lawyer-cxn
;; - who-cxn
;; - slew-cxn
;; - orcs-cxn
;; - orc-cxn
;; - slaying-cxn
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
;; - vp-modal-cxn
;; - subject-verb-modal-cxn
;; - subject-verb-modal-cxn
;; - arg1of-before-transitive-verb-cxn
;; - genitive-possessor-cxn
;; - predicative-cxn 
;; - arg1-gerund-cxn
;; - predicative-gerund-referring-entity-cxn 
;; - adverb-manner-cxn
;; - active-transitive-cxn 
;; - np-np-predicative-cxn 
;; - arg0-verb-cxn
;; - V-to-infinitive-cxn
;; - arg0-of-verb-cxn 
;; - active-arg1-cxn 

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
                          (syn-function nominal))
                  (sem-cat (sem-class object)))
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
                          (syn-function nominal))
                  (sem-cat (sem-class object)))
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
                (sem-cat (sem-class type)))
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
                (sem-cat (sem-class type)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         (number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class object)))
               (?compound-noun-noun-unit
                --
                (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))

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
                         (syn-function ?func)
                         (compound ?comp)
                         (subject-quantifier -))
                (subunits (?article-unit ?noun-unit))
                (boundaries (rightmost-unit ?noun-unit)
                            (leftmost-unit ?article-unit))
                (sem-cat (sem-class ?class)))
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
                         (syn-function ?func)
                         (compound -)
                         (subject-quantifier -))
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
                (sem-valence (:domain ?p))
                (meaning ((go-01 ?g))))
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
                (sem-valence (:domain ?p))
                (meaning ((possible ?p)
                          (:polarity ?p -))))
              <-
              (?cannot-unit
               --
               (HASH form ((string ?cannot-unit "cannot"))))))

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
              
(def-fcg-cxn opined-cxn
             ((?opined-unit
               (referent ?o)
               (meaning ((opine-01 ?o)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?opined-unit
                --
                (HASH form ((string ?opined-unit "opined"))))))

(def-fcg-cxn opinion-cxn
             ((?opinion-unit
               (referent ?o)
               (meaning ((thing ?t)
                         (opine-01 ?o)
                         (:arg1-of ?t ?o)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (phrase-type nominal)
                        (syn-function ?func))
              (sem-cat (sem-class possessed)))
               <-
               (?opinion-unit
                --
                (HASH form ((string ?opinion-unit "opinion"))))))

(def-fcg-cxn s-possessive-cxn
             ((?s-possessive-unit
              (syn-cat (syn-function possessive-form)))
              <-
              (?s-possessive-unit
               --
               (HASH form ((string ?s-possessive-unit "'s"))))))

(def-fcg-cxn of-cxn
               ((?of-unit
                 (syn-cat (lex-class preposition)
                          (phrase-type PP)))
                <-
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))))

(def-fcg-cxn y-of-x-cxn
             ((?y-of-x-unit
               (referent ?g)
               (meaning ((:arg0 ?o ?g)))
                (subunits (?np-x2-unit ?np-y2-unit ?of-preposition-unit))
                (boundaries (leftmost-unit ?np-x2-leftmost-unit)
                            (rightmost-unit ?np-y2-rightmost-unit)))
               <-
               (?np-x2-unit
               --
               (referent ?o)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?np-x2-leftmost-unit)
                (rightmost-unit ?np-x2-rightmost-unit)))
               (?np-y2-unit
               --
               (referent ?g)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?np-y2-leftmost-unit)
                (rightmost-unit ?np-y2-rightmost-unit)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition)
                          (phrase-type PP))
                (form ((string ?of-preposition-unit "of"))))
               (?y-of-x-unit
                --
                (HASH form ((precedes ?np-x2-rightmost-unit ?of-preposition-unit)
                            (precedes ?of-preposition-unit ?np-y2-leftmost-unit))))))
 
 (def-fcg-cxn x-s-y-cxn
              ((?x-s-y-unit
                (referent ?g)
                (meaning ((:arg0 ?o ?g)))
                (subunits (?np-x-unit ?np-y-unit ?possessive-unit))
                (boundaries (leftmost-unit ?np-y-leftmost-unit)
                            (rightmost-unit ?np-x-rightmost-unit)))
               <-
               (?np-x-unit
                --
                (referent ?o)
                (syn-cat (lex-class noun)
                         (number sg)
                         (person 3)
                         (syn-function nominal))
                (sem-cat (sem-class possessed)))
               (?np-y-unit
                --
                (referent ?g)
                (syn-cat (phrase-type NP)
                         (number sg)
                         (person 3)
                         (syn-function ?func))
                (sem-cat (sem-class possessor))
                (boundaries
                 (leftmost-unit ?np-y-leftmost-unit)
                 (rightmost-unit ?np-y-rightmost-unit)))
               (?possessive-unit
                --
                (form ((string ?possessive-unit "'s")))
                (syn-cat (syn-function possessive-form)))
               (?x-s-y-unit
                --
                (HASH form ((precedes ?np-y-rightmost-unit ?possessive-unit)
                            (precedes ?possessive-unit ?np-x-leftmost-unit))))))

(def-fcg-cxn girl-lex-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)
                        (number ?numb)
                        (syn-function ?func)))
              <-
              (?girl-unit
               (lex-id girl))))
 
(def-fcg-cxn girl-morph-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (lex-id girl)
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function nominative)
                        (compound -))
             (sem-cat (sem-class subject)))
              <-
              (?girl-unit
               --
               (HASH form ((string ?girl-unit "girl"))))))
                  
(def-fcg-cxn girls-morph-cxn
             ((?girls-unit
               (referent ?g)
               (lex-id girl)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function ?func)
                        (subject-quantifier -))
               (sem-cat (sem-class arg1)))
              <-
              (?girls-unit
               --
               (HASH form ((string ?girls-unit "girls"))))))

(def-fcg-cxn marble-cxn
             ((?marble-unit
               (referent ?m)
               (meaning ((marble ?m)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
                (sem-cat (sem-class subject)))
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

(def-fcg-cxn inappropriate-cxn
            ((?inappropriate-unit
              (referent ?a)
              (meaning ((appropriate ?a)
                        (:polarity ?a -)))
              (syn-cat (lex-class adjective)
                       (number ?numb)
                       (syn-function ?func))
             (sem-cat (sem-class quality)))
             <-
             (?inappropriate-unit
              --
              (HASH form ((string ?inappropriate-unit "inappropriate"))))))

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

(def-fcg-cxn please-cxn
             ((?please-unit
               (referent ?p)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (gerund +)
                        (syn-function ?func)))
               <-
               (?pleasing-unit
                --
                (HASH form ((string ?please-unit "please"))))))

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
                        (transitive -)
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
               (meaning ((:manner ?w ?h)))
               (subunits (?vp-unit ?adverb-unit))
               (referent ?w)
               (syn-cat (phrase-type clausal))
               (boundaries
                 (rightmost-unit ?vp-unit-leftmost)
                 (leftmost-unit ?adverb-unit-rightmost)))
               <-
               (?vp-unit
                --
                (referent ?w)
                (syn-cat (lex-class verb)
                         (finite +)
                          (phrase-type VP)))
               (?adverb-unit
               --
               (referent ?h)
                (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
               (?adverb-manner-unit
                --
                (HASH form ((precedes ?vp-unit ?adverb-unit))))))

 (def-fcg-cxn subject-verb-adverb-cxn
             ((?subject-verb-adverb-unit
               (meaning ((:arg0 ?w ?b)))
               (subunits (?adverb-manner-unit ?subject-unit))
               (boundaries (rightmost-unit ?adverb-manner-rightmost-unit)
                           (leftmost-unit ?subject-leftmost-unit)))
              <-
              (?adverb-manner-unit
               --
               (referent ?w)
               (syn-cat (phrase-type clausal))
               (boundaries
                 (rightmost-unit ?adverb-manner-rightmost-unit)
                 (leftmost-unit ?adverb-manner-leftmost-unit)))
              (?subject-unit
               --
               (referent ?b)
               (referent ?subject)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?subject-leftmost-unit)
                (rightmost-unit ?subject-rightmost-unit)))
              (?subject-verb-adverb-unit
               --
               (HASH form ((precedes ?subject-unit-leftmost-unit ?adverb-manner-rightmost-unit))))))


(def-fcg-cxn soldier-cxn
             ((?soldier-unit
               (referent ?s)
               (meaning ((soldier ?s)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)
                        (compound -)
                        (subject-quantifier -))
               (sem-cat (sem-class agent)))
              <-
              (?soldier-unit
               --
               (HASH form ((string ?soldier-unit "soldier"))))))

(def-fcg-cxn feared-cxn
             ((?feared-unit
               (referent ?f)
               (meaning ((fear-01 ?f)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?feared-unit
                --
                (HASH form ((string ?feared-unit "feared"))))))

(def-fcg-cxn battle-cxn
             ((?battle-unit
               (referent ?b)
               (meaning ((battle-01 ?b)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)
                        (phrase-type nominal))
               (sem-cat (sem-class direct-object)))
              <-
              (?battle-unit
               --
               (HASH form ((string ?battle-unit "battle"))))))

(def-fcg-cxn woman-cxn
             ((?woman-unit
               (referent ?w)
               (meaning ((woman ?w)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
               (sem-cat (sem-class agent)))
               <-
               (?woman-unit
                --
                (HASH form ((string ?woman-unit "woman"))))))

(def-fcg-cxn lawyer-cxn
             ((?lawyer-unit
               (referent ?l)
               (meaning ((lawyer ?l)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)
                        (person 3))
              (sem-cat (sem-class predicative)))
               <-
               (?lawyer-unit
                --
                (HASH form ((string ?lawyer-unit "lawyer"))))))

(def-fcg-cxn np-np-predicative-cxn 
             ((?np-np-predicative-unit
               (subunits (?vp-unit ?np-a-unit ?np-pr-unit))
               (meaning ((:domain ?l ?w)))
               (boundaries (leftmost-unit ?leftmost-np-a-unit)
                           (rightmost-unit ?rightmost-np-pr-unit)))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (phrase-type VP)))
              (?np-a-unit
               --
               (referent ?w)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (sem-cat (sem-class agent))
               (boundaries
                (leftmost-unit ?leftmost-np-a-unit)
                (rightmost-unit ?rightmost-np-a-unit)))
              (?np-pr-unit
               --
               (referent ?l)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (sem-cat (sem-class predicative))
               (boundaries
                (rightmost-unit ?rightmost-np-pr-unit)
                (leftmost-unit ?leftmost-np-pr-unit)))
              (?np-np-predicative-unit
               --
               (HASH form ((precedes ?rightmost-np-a-unit ?vp-unit )
                           (precedes ?vp-unit ?leftmost-np-pr-unit))))))


(def-fcg-cxn want-cxn
             ((?want-unit
               (referent ?w)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (phrase-type VP)
                        (to-infinitif +)
                        (relative -)
                        (transitive +))
               (meaning ((want-01 ?w))))
               <-
              (?want-unit
               --
               (HASH form ((string ?want-unit "wants"))))))

(def-fcg-cxn go-cxn
             ((?go-unit
               (referent ?g)
               (syn-cat (lex-class verb)
                        (finite -)
                        (modal -)
                        (phrase-type VP)
                        (infinitif +))
               (sem-valence (arg0 ?b)
                            (arg2 ?p))
               (meaning ((go-01 ?g))))
              <-
              (?go-unit
               --
               (HASH form ((string ?go-unit "go"))))))

(def-fcg-cxn V-infinitive-cxn
             ((?V-infinitive-unit
              (meaning ((:arg1 ?verb ?inf)))
              (subunits (?finite-verb-unit ?infinitif-unit))
              (referent ?verb)
              (syn-cat (phrase-type VP))
              (boundaries
               (leftmost-unit ?finite-verb-unit)
               (rightmost-unit ?infinitif-unit)))
              <-
              (?finite-verb-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (to-infinitif +)
                        (phrase-type VP)))
              (?infinitif
              --
              (referent ?inf)
              (syn-cat (lex-class verb)
                        (finite -)
                        (infinitif +)
                        (phrase-type VP)))
              (?V-infinitive-unit
              --
              (HASH form ((precedes ?finite-verb-unit ?infinitif))))))
              
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
                        (named-entity-type person))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
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

(def-fcg-cxn orc-lex-cxn
             ((?orc-unit
               (referent ?o)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (number ?numb)
                        (syn-function ?func)))
              <-
              (?orc-unit
               (lex-id orc))))

(def-fcg-cxn orc-morph-cxn
             ((?orc-unit
               (referent ?o)
               (lex-id orc)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
              (sem-cat (sem-class arg1)))
              <-
              (?orc-unit
               --
               (HASH form ((string ?orc-unit "orc"))))))

(def-fcg-cxn orcs-morph-cxn
             ((?orcs-unit
               (referent ?o)
               (lex-id orc)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function ?func)
                        (phrase-type nominal))
               (sem-cat (sem-class direct-object)))
              <-
              (?orcs-unit
               --
               (HASH form ((string ?orcs-unit "orcs"))))))

(def-fcg-cxn arg0-of-verb-cxn 
             ((?arg0-of-verb-unit
               (meaning ((:arg0-of ?p ?verb)))
               (subunits (?vp-unit ?named-entity-unit))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?vp-unit))
               (referent ?p))
              <-
               (?named-entity-unit
               --
               (referent ?p)
               (syn-cat (phrase-type NP)
                        (named-entity-type person))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        ;;(finite +)
                        (modal -)
                        ;;(past-simple +)
                        (phrase-type VP)))
              (?arg0-of-verb-unit
               --
               (HASH form ((precedes ?named-entity-leftmost-unit ?vp-unit))))))

(def-fcg-cxn active-transitive-cxn 
             ((?active-transitive-unit
               (subunits (?vp-unit ?direct-object-unit))
               (meaning ((:arg1 ?verb ?o)))
               (boundaries (rightmost-unit ?direct-object-unit)
                           (leftmost-unit ?vp-unit))
               (referent ?verb))
              <-
              (?vp-unit
                --
                (referent ?verb)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal -)
                         (transitive +)
                         (phrase-type VP)))
              (?direct-object-unit
               --
               (referent ?o)
               (syn-cat (lex-class noun)
                        (number ?numb)
                        (phrase-type nominal)
                        (syn-function ?func))
               (sem-cat (sem-class direct-object)))
              (?active-transitive-unit
               --
              (HASH form ((precedes ?vp-unit ?direct-object-unit))))))

(def-fcg-cxn slay-lex-cxn
             ((?slay-unit
               (referent ?s)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)
                        (number ?numb)
                        (syn-function ?func)))
              <-
              (?slay-unit
               (lex-id slay))))

(def-fcg-cxn slew-morph-cxn
             ((?slew-unit
               (referent ?s)
               (lex-id slay)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (phrase-type VP)))
              <-
              (?slew-unit
               --
               (HASH form ((string ?slew-unit "slew"))))))

(def-fcg-cxn slaying-morph-cxn
             ((?slaying-unit
               (referent ?s)
               (lex-id slay)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)
                        (modal -)
                        (present-participle +)
                        (phrase-type VP)
                        (syn-function ?func)))
              <-
              (?slaying-unit
               --
               (HASH form ((string ?slaying-unit "slaying"))))))

(def-fcg-cxn arg1-present-participle-cxn
             ((?arg1-present-participle-unit
               (referent ?o)
               (meaning ((:arg1 ?s ?o)))
               (syn-cat (syn-function ?arg1))
               (subunits (?present-participle-unit ?arg1-NP-unit))
               (boundaries (?arg1-NP-unit-rightmost-unit ?present-participle-unit)))
               <-
               (?present-participle-unit
                (referent ?s)
                --
                (syn-cat (lex-class verb)
                         (modal -)
                         (present-participle +)
                         (phrase-type VP))
                         (syn-function ?func))
               (?arg1-NP-unit
               (referent ?o)
                --
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?arg1-NP-unit-leftmost-unit)
                (rightmost-unit ?arg1-NP-unit-rightmost-unit))
                (sem-cat (sem-class arg1)))
               (?arg1-present-participle-unit
                --
                (HASH form ((precedes ?arg1-NP-unit-rightmost-unit ?present-participle-unit))))))

(def-fcg-cxn college-cxn
             ((?college-unit
               (referent ?c)
               (meaning ((college ?c)))
               (syn-cat (phrase-type nominal)
                        (lex-class noun)
                        (number sg)
                        (nominalisation -)
                        (syn-function adjectival)
                        (person 3)
                        (compound +))
              (sem-cat (sem-class age)))
               <-
               (?college-unit
                --
                (HASH form ((string ?college-unit "college"))))))

 (def-fcg-cxn compound-noun-noun-type-cxn ;;type
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
                (sem-cat (sem-class type)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         (number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class object)))
               (?compound-noun-noun-unit
                --
                (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))
 
 (def-fcg-cxn compound-noun-noun-source-cxn ;;source
              ((?compound-noun-noun-source-unit
                (referent ?ref)
                (meaning ((:source ?ref ?c)))
                (syn-cat (lex-class noun)
                         (compound +)
                         (number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class age))
                (subunits (?np-unit ?second-noun-unit))
                (boundaries (leftmost-unit ?np-age-unit-leftmost-unit)
                            (rightmost-unit ?second-noun-unit)))
               <-
               (?np-age-unit
                --
                (referent ?c)
                (syn-cat (phrase-type np)
                         (number sg)
                         (person 3)
                         (syn-function adjectival))
                (sem-cat (sem-class age))
                (boundaries
                (leftmost-unit ?np-age-unit-leftmost-unit)
                (rightmost-unit ?np-age-unit-rightmost-unit)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         (number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class person)))
               (?compound-noun-noun-source-unit
                --
                (HASH form ((precedes ?np-age-unit-rightmost-unit ?second-noun-unit))))))

 (def-fcg-cxn sang-cxn
             ((?sang-unit
               (referent ?s)
               (meaning ((sing-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive -)
                        (past-simple +)
                        (relative +)
                        (phrase-type VP)))
               <-
               (?sang-unit
                --
                (HASH form ((string ?sang-unit "sang"))))))

(def-fcg-cxn arg0-of-compound-noun-relative-cxn 
             ((?arg0-of-compound-noun-relative-unit
               (meaning ((:arg0-of ?b ?s)))
               (syn-cat (relative +))
               (subunits (?compound-noun-unit ?relative-unit ?verb-unit))
               (boundaries (leftmost-unit ?compound-noun-unit-leftmost-unit)
                           (rightmost-unit ?verb-unit))
               (referent ?b))
              <-
               (?compound-noun-unit
               --
               (referent ?b)
               (syn-cat (lex-class noun)
                        (compound +)
                        (number sg)
                        (syn-function nominal))
               (sem-cat (sem-class age))
               (boundaries (leftmost-unit ?compound-noun-leftmost-unit)
                           (rightmost-unit ?compound-noun-rightmost-unit)))
              (?relative-unit
              --
              (syn-cat (lex-class pronoun)
                       (number ?numb)
                       (syn-function ?func))
              (sem-cat (sem-class subject)))
              (?verb-unit
               --
               (referent ?s)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (past-simple +)
                        (transitive -)
                        (relative +)
                        (phrase-type VP)))
              (?arg0-of-compound-noun-relative-unit
               --
               (HASH form ((precedes ?compound-noun-rightmost-unit ?relative-unit)
                           (precedes ?relative-unit ?verb-unit))))))

(def-fcg-cxn who-cxn 
             ((?who-unit
               (referent ?b)
               (syn-cat (lex-class pronoun)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class subject)))
              <-
              (?who-unit
               --
               (HASH form ((string ?who-unit "who"))))))


(def-fcg-cxn increased-cxn
             ((?increased-unit
               (referent ?i)
               (meaning ((increase-01 ?i)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive -)
                        (past-simple +)
                        (phrase-type VP)
                        (subject-quantifier +)))
              <-
              (?increased-unit
               --
               (HASH form ((string ?increased-unit "increased"))))))

(def-fcg-cxn panda-lex-cxn
             ((?panda-unit
               (referent ?p)
               (meaning ((panda ?p)))
               (syn-cat (lex-class noun)
                        (number ?numb)
                        (syn-function ?func)))
              <-
              (?panda-unit
               (lex-id panda))))

(def-fcg-cxn panda-morph-cxn
             ((?panda-unit
               (referent ?p)
               (meaning ((panda ?p)))
               (lex-id panda)
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function nominative)
                        (compound -)))
              <-
              (?panda-unit
               --
               (HASH form ((string ?panda-unit "panda"))))))
                  
(def-fcg-cxn pandas-morph-cxn
             ((?pandas-unit
               (referent ?p)
               (lex-id panda)
               (meaning ((panda ?p)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (person 3)
                        (compound -)
                        (subject-quantifier +)
                        (syn-function nominal))
               (sem-cat (sem-class quantified)))
              <-
              (?pandas-unit
               --
               (HASH form ((string ?pandas-unit "pandas"))))))

(def-fcg-cxn number-cxn
             ((?number-unit
               (referent ?n)
               (meaning ((number ?n)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (compound -)
                        (subject-quantifier +))
               (sem-cat (sem-class quantifier)))
              <-
               (?number-unit
                --
                (HASH form ((string ?number-unit "number"))))))

(def-fcg-cxn quantifier-of-cxn
             ((?quantifier-of-unit
               (referent ?p)
               (meaning ((:quant-of ?n ?p)))
               (subunits (?quantifier-number-unit ?of-preposition-unit ?quantified-unit))
               (boundaries (leftmost-unit ?quanitifier-number-leftmost-unit)
                           (rightmost-unit ?quantified-unit)))
               <-
               (?quantifier-number-unit
                --
                (referent ?n)
                (syn-cat (phrase-type np)
                         (lex-class noun)
                         (person 3)
                         (number sg)
                         (compound -)
                         (syn-func ?func))
               (boundaries (leftmost-unit ?quantifier-number-leftmost-unit)
                            (rightmost-unit ?quantifier-number-rightmost-unit)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition)
                          (phrase-type PP))
               (form ((string ?of-preposition-unit "of"))))
               (?quantified-unit
               --
               (referent ?p)
               (syn-cat (lex-class noun)
                        (number pl)
                        (person 3)
                        (subject-quantifier +)))
               (?quantifier-of-unit
                --
               (HASH form ((precedes ?quantifier-number-rightmost-unit ?of-preposition-unit)
                           (precedes ?of-preposition-unit ?quantified-unit))))))

(def-fcg-cxn did-cxn
             ((?did-unit
               (syn-cat (lex-class aux)
                        (finite ?fin)
                        (lemma do)))
              <-
              (?did-unit
               --
               (HASH form ((string ?did-unit "did"))))))

(def-fcg-cxn not-cxn
             ((?not-unit
               ;;(syn-cat (positive -)
               (syn-cat (lex-class adverb)))
               <-
              (?not-unit
               --
               (HASH form ((string ?not-unit "not"))))))
 
 (def-fcg-cxn machine-cxn
             ((?machine-unit
               (referent ?m)
               (meaning ((machine ?m)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (phrase-type NP)
                        (syn-function accusative))
               (sem-cat (sem-class direct-object)))
               <-
               (?machine-unit
                --
                (HASH form ((string ?machine-unit "machine"))))))

 (def-fcg-cxn adjusted-cxn
             ((?adjusted-unit
               (referent ?a)
               (meaning ((adjust-01 ?a)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (relative -)
                        (transitive +)
                        (past-simple +)
                        (phrase-type VP)))
               <-
               (?adjusted-unit
                --
                (HASH form ((string ?adjusted-unit "adjusted"))))))

 
(def-fcg-cxn judge-cxn
             ((?judge-unit
               (referent ?j)
               (meaning ((judge ?j)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)
                        (subject-quantifier -)))
              <-
              (?judge-unit
               --
                (HASH form ((string ?judge-unit "judge"))))))

(def-fcg-cxn saw-cxn
             ((?saw-unit
               (referent ?s)
               (meaning ((see-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?saw-unit
                --
                (HASH form ((string ?saw-unit "saw"))))))

(def-fcg-cxn explosion-cxn
             ((?explosion-unit
               (referent ?e)
               (meaning ((explode-01 ?e)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (phrase-type NP)
                        (syn-function accusative))
               (sem-cat (sem-class direct-object)))
              <-
              (?explosion-unit
               --
               (HASH form ((string ?explosion-unit "explosion"))))))

(def-fcg-cxn proposal-cxn
             ((?proposal-unit
               (referent ?t)
               (meaning ((thing ?t)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (phrase-type NP)
                        (syn-function accusative))
               (sem-cat (sem-class direct-object)))
              <-
              (?proposal-unit
               --
               (HASH form ((string ?proposal-unit "proposal"))))))

(def-fcg-cxn read-cxn
             ((?read-unit
               (referent ?r)
               (meaning ((read-01 ?r)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple -)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?read-unit
                --
                (HASH form ((string ?read-unit "read"))))))

(def-fcg-cxn are-cxn
             ((?are-unit
               (referent ?are)
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (phrase-type VP)
                        (finite +)
                        (modal -)
                        (relative -)
                        (transitive -)
                        (phrase-type VP)))
              <-
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))))

(def-fcg-cxn must-cxn
             ((?must-unit
               (referent ?p)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal +)
                        (polarity +)
                        (phrase-type VP))
               (sem-valence (:arg2 ?arg2))
               (meaning ((obligate-01 ?p))))
              <-
              (?must-unit
               --
               (HASH form ((string ?must-unit "must"))))))

(def-fcg-cxn need-cxn
             ((?need-unit
               (referent ?p)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal +)
                        (phrase-type VP))
               (sem-valence (:arg2 ?arg2))
               (meaning ((obligate-01 ?p)
                         (:polarity ?p -))))
              <-
              (?need-unit
               --
               (HASH form ((string ?need-unit "need"))))))

(def-fcg-cxn subject-infinitif-cxn
             ((?subject-infinitif-unit
               (meaning ((:arg0 ?inf ?b)))
               (referent ?b)
               (subunits (?infinitif-unit ?np-unit))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?infinitif-unit)))
              <-
              (?infinitif-unit
               --
              (referent ?inf)
              (syn-cat (lex-class verb)
                       (finite -)
                        (infinitif +)
                        (phrase-type VP)))
              (?np-unit
               --
               (referent ?b)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?np-leftmost-unit)
                (rightmost-unit ?np-rightmost-unit)))
              (?subject-infinitif-unit
               --
               (HASH form ((precedes ?np-rightmost-unit ?infinitif-unit))))))
               
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

(def-fcg-cxn please-cxn
             ((?please-unit
               (referent ?p)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class verb)
                        (finite -)
                        (phrase-type VP)
                        (modal -)
                        (gerund -)
                        (infinitif +)
                        (syn-function ?func)))
               <-
               (?please-unit
                --
                (HASH form ((string ?please-unit "please"))))))

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

(def-fcg-cxn domain-adjective-infinitif-cxn
             ((?domain-adjective-infinitif-unit
               (referent ?p)
               (subunits (?adjective-unit ?infinitif-unit))
               (meaning ((:domain ?t ?p))))
               <-
               (?adjective-unit
                --
                (referent ?t)
                (syn-cat (lex-class adjective)
                         (number ?numb)
                         (syn-function ?func))
                (sem-cat (sem-class quality)))
               (?infinitif-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                        (finite -)
                        (modal -)
                        (gerund -)
                        (phrase-type VP)
                        (infinitif +)
                        (syn-function ?func)))
               (?domain-adjective-infinitif-unit
                --
                (HASH form ((precedes ?modal-unit ?infinitif-unit))))))

(def-fcg-cxn auxiliar-infinitif-negative-cxn
             ((?auxiliar-infinitif-negative-unit
               (referent ?g)
               (subunits (?aux-unit ?not-unit ?infinitif-unit))
               (meaning ((:polarity ?g -)))
               (boundaries (rightmost-unit ?aux-unit)
                           (leftmost-unit ?infinitif-unit)))
               <-
               (?aux-unit
                --
                (syn-cat (lex-class aux)))
                (?not-unit
                --
                (syn-cat (lex-class adverb))
                (form ((string ?not-unit "not"))))
                (?infinitif-unit
                 --
                 (referent ?g)
                 (syn-cat (lex-class verb)
                          (finite -)
                          (modal -)
                          (phrase-type VP)
                          (infinitif +)))
                (?auxiliar-infinitif-negative-unit
                 --
                 (HASH form ((precedes ?aux-unit ?not-unit)
                             (precedes ?not-unit ?infinitif-unit))))))

(def-fcg-cxn not-appropriate-cxn
            ((?appropriate-unit
              (referent ?a)
              (meaning ((appropriate ?a)
                        (:polarity ?a -)))
              (syn-cat (lex-class adjective)
                       (number ?numb)
                       (syn-function ?func))
             (sem-cat (sem-class quality)))
             <-
             (?appropriate-unit
              --
              (HASH form ((string ?appropriate-unit "appropriate"))))))

(def-fcg-cxn jar-cxn
             ((?jar-unit
               (referent ?j)
               (meaning ((jar ?j)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func))
               (sem-cat (sem-class location)))
              <-
              (?jar-unit
               --
               (HASH form ((string ?jar-unit "jar"))))))

(def-fcg-cxn in-cxn
             ((?in-unit
               (syn-cat (lex-class preposition)
                        (phrase-type PP)))
              <-
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))))

(def-fcg-cxn in-location-np-cxn
             ((?in-location-np-unit
               (referent ?j)
               (meaning ((:location ?m ?j)))
               (subunits (?np-subject-unit ?in-unit ?np-location-unit))
               (boundaries (leftmost-unit ?np-subject-leftmost-unit)
                           (rightmost-unit ?np-location-rightmost-unit)))
               <-
               (?in-unit
                --
                (syn-cat (lex-class preposition)
                         (phrase-type PP))
                (form ((string ?in-unit "in"))))
               (?np-subject-unit
                --
                (referent ?m)
                (syn-cat (phrase-type NP)
                         (number sg)
                         (person 3)
                         (syn-function ?func))
               (boundaries
                (leftmost-unit ?np-subject-leftmost-unit)
                (rightmost-unit ?np-subject-rightmost-unit)))
               (?np-location-unit
                --
                (referent ?j)
                (syn-cat (phrase-type NP)
                         (number sg)
                         (person 3)
                         (syn-function ?func))
                (boundaries
                 (leftmost-unit ?np-location-leftmost-unit)
                 (rightmost-unit ?np-location-rightmost-unit)))
               (?in-location-np-unit
                --
                (HASH form ((precedes ?np-subject-rightmost-unit ?in-unit)
                            (precedes ?in-unit ?np-location-leftmost-unit))))))

(def-fcg-cxn in-time-cxn
             ((?in-time-unit
               (referent ?d)
               (meaning ((:time ?d ?d2)))
               (subunits (?vp-unit ?in-unit ?time-unit))
               (boundaries (leftmost-unit ?vp-unit)
                           (rightmost-unit ?time-unit)))
               <-
               (?in-unit
                --
                (syn-cat (lex-class preposition)
                         (phrase-type PP))
                (form ((string ?in-unit "in"))))
               (?vp-unit
                --
               (referent ?d)
                (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive -)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               (?time-unit
                --
                (referent ?d2)
                (syn-cat (lex-class noun)
                         (phrase-type nominal)
                         (number sg)
                         (person 3)
                         (syn-function ?func))
                (sem-cat (sem-role date-entity)))
               (?in-time-unit
                --
                (HASH form ((precedes ?vp-unit ?in-unit)
                            (precedes ?in-unit ?time-unit))))))
(def-fcg-cxn nation-cxn
             ((?nation-unit
               (referent ?n)
               (meaning ((nation ?n)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function accusative)
                        (compound -))
               (sem-cat (sem-class patient)))
              <-
              (?nation-unit
               --
               (HASH form ((string ?nation-unit "nation"))))))

(def-fcg-cxn june-cxn
             ((?june-unit
               (referent ?d2)
               (meaning ((:date-entity ?d2)
                         (:month ?d2 6)))
               (syn-cat (lex-class noun)
                        (phrase-type nominal)
                        (number sg)
                        (syn-function ?func))
               (sem-cat (sem-role date-entity)))
              <-
              (?june-unit
               --
               (HASH form ((string ?june-unit "June"))))))

 (def-fcg-cxn destroyed-cxn
             ((?destroyed-unit
               (referent ?d)
               (meaning ((destroy-01 ?d)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?destroyed-unit
                --
                (HASH form ((string ?destroyed-unit "destroyed"))))))
 
 (def-fcg-cxn room-cxn
             ((?room-unit
               (referent ?r)
               (meaning ((room ?r)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (phrase-type NP)
                        (syn-function accusative)))
               <-
               (?room-unit
                --
                (HASH form ((string ?room-unit "room"))))))

(def-fcg-cxn subject-verb-cxn
             ((?subject-verb-unit
               (meaning ((:arg0 ?verb ?subj)))
               (subunits (?vp-unit ?subject-unit))
               (referent ?verb)
               (boundaries (rightmost-unit ?vp-unit)
                           (leftmost-unit ?subject-unit))
               (syn-cat (phrase-type VP)
                        (compound -)
                        (relative -)
                        (subject-quantifier -)))
              <-
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal ?mod)
                        (relative -)
                        (transitive ?trans)
                        (phrase-type VP)))
              (?subject-unit
               --
               (referent ?subj)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (compound -)
                        (subject-quantifier -)
                        (syn-function ?func))
               (sem-cat (sem-class subject))
               (boundaries
                (leftmost-unit ?subject-leftmost-unit)
                (rightmost-unit ?subject-rightmost-unit)))
              (?subject-verb-unit
               --
               (HASH form ((precedes ?subject-rightmost-unit ?vp-unit))))))

  (def-fcg-cxn patient-cxn
              ((?patient-unit
                (referent ?patient)
                (subunits (?patient-arg2-unit ?verb-unit))
                (meaning ((:arg1 ?verb ?patient)))
                (boundaries (leftmost-unit ?verb-unit)
                            (rightmost-unit ?patient-arg2-rightmost-unit)))
               <-
               (?patient-arg2-unit
                --
                (referent ?patient)
                (syn-cat (phrase-type np)
                         (number ?numb)
                         (person ?person)
                         (syn-function accusative)
                         (compound ?compound))
                (boundaries (leftmost-unit ?patient-arg2-leftmost-unit)
                            (rightmost-unit ?patient-arg2-rightmost-unit)))
               (?verb-unit
                --
                (referent ?verb)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal ?mod)
                         (past-simple ?pastsimple)
                         (transitive ?transitive)
                         (relative -)
                         (phrase-type VP)))
               (?patient-unit
                --
                (HASH form ((precedes ?verb-unit ?patient-arg2-rightmost-unit))))))

(def-fcg-cxn defaulted-cxn
             ((?defaulted-unit
               (referent ?d)
               (meaning ((default-01 ?d)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive -)
                        (past-simple +)
                        (relative -)
                        (passive +)
                        (phrase-type VP)))
               <-
               (?defaulted-unit
                --
                (HASH form ((string ?defaulted-unit "defaulted"))))))
  
(def-fcg-cxn arg2-modal-infinitif-cxn
             ((?arg2-modal-infinitif-unit
               (referent ?p)
               (subunits (?modal-unit ?infinitif-unit))
               (meaning ((:arg2 ?p ?g)))
               (boundaries (rightmost-unit ?infinitif-unit)
                           (leftmost-unit ?modal-unit)))
               <-
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                        (finite +)
                        (modal +)
                        (phrase-type VP))
                (sem-valence (:arg2 ?arg2)))
               (?infinitif-unit
                --
                (referent ?g)
                (syn-cat (lex-class verb)
                        (finite -)
                        (modal -)
                        (phrase-type VP)
                        (infinitif +)))
               (?arg2-modal-infinitif-unit
                --
                (HASH form ((precedes ?modal-unit ?infinitif-unit))))))

(def-fcg-cxn domain-modal-infinitif-cxn
             ((?domain-modal-infinitif-unit
               (referent ?p)
               (subunits (?modal-unit ?infinitif-unit))
               (meaning ((:domain ?p ?g)))
               (boundaries (rightmost-unit ?infinitif-unit)
                           (leftmost-unit ?modal-unit)))
               <-
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                        (finite +)
                        (modal +)
                        (phrase-type VP))
                (sem-valence (:domain ?p)))
               (?infinitif-unit
                --
                (referent ?g)
                (syn-cat (lex-class verb)
                        (finite -)
                        (modal -)
                        (phrase-type VP)
                        (infinitif +)))
               (?domain-modal-infinitif
                --
                (HASH form ((precedes ?modal-unit ?infinitif-unit))))))

(def-fcg-cxn modal-negative-infinitif
             ((?modal-negative-infinitif
               (referent ?g)
               (meaning ((:polarity ?inf -)))
               (subunits (?not-unit ?modal-unit ?infinitif-unit))
               (boundaries (rightmost-unit ?infinitif-unit)
                           (leftmost-unit ?did-unit)))
              <-
              (?infinitif-negative-unit
               --
               (referent ?inf)
               (syn-cat (lex-class verb)
                         (finite -)
                         (modal -)
                         (phrase-type vp)
                         (infinitif +)))
              (?not-unit
               --
               (syn-cat (lex-class adverb))
               (form ((string ?not-unit "not"))))
              (?modal-unit
               --
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal +)
                         (polarity +)
                         (phrase-type VP)))
              (?modal-negative-infinitif
               --
               (HASH form ((precedes ?modal-unit ?not-unit)
                           (precedes ?not-unit ?infinitif-unit))))))

(def-fcg-cxn answer-cxn
             ((?answer-unit
               (referent ?a)
               (meaning ((answer ?a)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (phrase-type NP)
                        (syn-function accusative))
               (sem-cat (sem-class direct-object)))
               <-
               (?answer-unit
                --
                (HASH form ((string ?answer-unit "answer"))))))

(def-fcg-cxn looked-up-cxn
             ((?looked-up-unit
               (referent ?l)
               (meaning ((look-05 ?l)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (past-simple +)
                        (relative -)
                        (phrase-type VP)))
               <-
               (?looked-up-unit
                --
                (HASH form ((string ?looked-up-unit "looked"))))))

(def-fcg-cxn find-cxn
             ((?find-unit
               (referent ?f)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (phrase-type VP)
                        (relative -)
                        (past-simple -)
                        (interrogative +)
                        (transitive +))
               (meaning ((find-01 ?f))))
               <-
              (?find-unit
               --
               (HASH form ((string ?find-unit "find"))))))

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

(def-fcg-cxn worker-cxn
             ((?worker-unit
               (referent ?w)
               (meaning ((worker ?w)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function ?func)
                        (person 3))
              (sem-cat (sem-class predicative)))
               <-
               (?worker-unit
                --
                (HASH form ((string ?worker-unit "worker"))))))

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

(def-fcg-cxn adverb-manner-cxn
             ((?adverb-manner-unit
               (meaning ((:manner ?w ?h)))
               (subunits (?vp-unit ?adverb-unit))
               (referent ?w)
               (syn-cat (phrase-type clausal))
               (boundaries
                 (rightmost-unit ?vp-unit-leftmost)
                 (leftmost-unit ?adverb-unit-rightmost)))
               <-
               (?vp-unit
                --
                (referent ?w)
                (syn-cat (lex-class verb)
                         (finite +)
                          (phrase-type VP)))
               (?adverb-unit
               --
               (referent ?h)
                (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
               (?adverb-manner-unit
                --
                (HASH form ((precedes ?vp-unit ?adverb-unit))))))

(def-fcg-cxn adverb-manner-adjective-predicative-cxn
             ((?adverb-manner-unit
               (meaning ((:manner ?w ?h)))
               (subunits (?adverb-unit ?noun-unit))
               (referent ?w)
               (boundaries
                 (rightmost-unit ?noun-unit)
                 (leftmost-unit ?adverb-unit)))
               <-
               (?noun-unit
                --
                (referent ?w)
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function ?func)
                         (person 3))
                (sem-cat (sem-class predicative)))
               (?adverb-unit
               --
               (referent ?h)
                (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
               (?adverb-manner-unit
                --
                (HASH form ((precedes ?adverb-unit ?noun-unit))))))

(def-fcg-cxn to-cxn
             ((?to-unit
               (syn-cat (lex-class preposition)
                        (phrase-type PP)))
              <-
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))))

(def-fcg-cxn patient-infinitif-cxn
             ((?patient-infinitif-unit
               (meaning ((:arg1 ?inf ?g)))
               (referent ?g)
               (subunits (?infinitif-unit ?to-unit ?noun-unit)))
              <-
              (?infinitif-unit
               --
              (referent ?inf)
              (syn-cat (lex-class verb)
                       (finite -)
                       (modal -)
                       (gerund -)
                       (infinitif +)
                       (syn-function ?func)
                       (phrase-type VP)))
              (?to-unit
               --
               (syn-cat (lex-class preposition)
                        (phrase-type PP))
               (form ((string ?to-unit "to"))))
              (?noun-unit
               --
               (referent ?g)
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function ?func)
                        (subject-quantifier -)))
              (?patient-infinitif-unit
               --
               (HASH form ((precedes ?noun-unit ?to-unit)
                           (precedes ?to-unit ?infinitif-unit))))))

(def-fcg-cxn patient-infinitif-order-different-cxn
             ((?patient-infinitif-order-different-unit
               (meaning ((:arg1 ?inf ?g)))
               (referent ?g)
               (subunits (?infinitif-unit ?to-unit ?noun-unit)))
              <-
              (?infinitif-unit
               --
              (referent ?inf)
              (syn-cat (lex-class verb)
                       (finite -)
                       (modal -)
                       (gerund -)
                       (infinitif +)
                       (syn-function ?func)
                       (phrase-type VP)))
              (?to-unit
               --
               (syn-cat (lex-class preposition)
                        (phrase-type PP))
               (form ((string ?to-unit "to"))))
              (?noun-unit
               --
               (referent ?g)
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function ?func)
                        (subject-quantifier -)))
              (?patient-infinitif-order-different-unit
               --
               (HASH form ((precedes ?to-unit ?infinitif-unit)
                           (precedes ?infinitif-unit ?girls-unit))))))

)

#|
it is tough to please girls	((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)) 

|#
            
