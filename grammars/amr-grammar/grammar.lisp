;;AMR grammar Banarescu Corpus developed by Martina Galletti (Spring 2019)
;;-------------------------------------------------------
;; Sentences covered so far:
;; 1. Zintan
;; 40. City of Zintan
;; 6. Mollie Brown
;; 7. President Obama
;; 8. Obama, the president 
;;-------------------------------------------------------
;; Nominal constructions covered so far:
;; 1.named entities for person 
;; 2.named entities for city
;; 3. Brown last name 
;;-------------------------------------------------------
;; Verbal constructions covered so far:
;;--------------------------------------------------------
;; Single lemma constructions covered so far:
;; 1. Investor
;; 2. Zintan
;; 3. Bond
;; 4. Small
;; 5. Atom
;; 6. Atomic
;; 7. City 
;;--------------------------------------------------------
;(ql:quickload :amr-grammar)
(in-package :amr-grammar)
(activate-monitor trace-fcg)

;; Grammar
(def-fcg-constructions amr-Banarescu-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first))

  ;; 1. Investor 
  ;; (PERSON P) (INVEST-01 I) (:ARG0-OF P I))
  (def-fcg-cxn investor-cxn 
               ((?investor-unit
                 (referent ?i)
                 (sem-valence (name ?m))
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class person))
                 (number ?numb))
                <-
                (?investor-unit
                 (HASH meaning ((person ?p)
                                (:arg0-of ?p ?i))
                       --
                       (HASH form ((string ?invest-unit "investor")))))))
;)



  ;;2. Zintan and 41.Zintan the city of Zintan
  ;;((CITY C) (NAME N) (:NAME C N) (:OP1 N "Zintan"))
  (def-fcg-cxn city-cxn
               ((?city-unit
                 (referent ?c)
                 (sem-valence (name ?n))
                 (syn-cat (lex-class noun)
                          (syn-function nominal))
                 (sem-cat (sem-class city)))
                <-
                (?city-unit
                 (HASH meaning ((city ?c)
                                (:name ?c ?n)))
                 --
                 (HASH form ((string ?city-unit "city"))))))

  ;;Zintan
  (def-fcg-cxn Zintan-cxn
               ((?Zintan-unit
                 (referent ?n)
                 (syn-cat (lex-class proper-noun)
                          (syn-function nominal))
                 (sem-cat (sem-class city)))
                <-
                (?Zintan-unit
                 (HASH meaning ((name ?n)
                                (:op1 ?n "Zintan")))
                 --
                 (HASH form ((string ?Zintan-unit "Zintan"))))))

  ;;8.President Obama + 13.Obama the president
  ;; same AMR representation, no article in AMR, no construction for "the"? 
  ;;((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")) 

  ;;president
  (def-fcg-cxn president-capitalized-cxn
               ((?president-unit
                 (referent ?p)
                 (sem-valence (name ?n))
                 (syn-cat (lex-class noun)
                          (syn-function nominal))
                 (sem-cat (sem-class person)))
                <-
                (?president-unit
                 (HASH meaning ((president ?p)
                                (:name ?p ?n)))
                 --
                 (HASH form ((string ?president-unit "President"))))))

  (def-fcg-cxn president-cxn
               ((?president-unit
                 (referent ?p)
                 (sem-valence (name ?n))
                 (syn-cat (lex-class noun)
                          (syn-function nominal))
                 (sem-cat (sem-class person)))
                <-
                (?president-unit
                 (HASH meaning ((president ?p)
                                (:name ?p ?n)))
                 --
                 (HASH form ((string ?president-unit "president"))))))

  ;;Obama
  (def-fcg-cxn Obama-cxn
               ((?Obama-unit
                 (referent ?n)
                 (syn-cat (lex-class proper-noun)
                          (syn-function nominal))
                 (sem-cat (sem-class person)))
                <-
                (?Obama-unit
                 (HASH meaning ((name ?n)
                                (:op1 ?n "Obama")))
                 --
                 (HASH form ((string ?Obama-unit "Obama"))))))


  ;;named entities for person 
  (def-fcg-cxn named-entity-function-cxn
               ((?named-entity-unit
                 (referent ?p)
                 (subunits (?nominal-unit-1 ?nominal-unit-2))
                 (syn-cat (phrase-type NP)
                          (named-entity-type person)))
                <-
                (?nominal-unit-1
                 --
                 (referent ?p)
                 (sem-valence (name ?n))
                 (syn-cat (syn-function nominal))
                 (sem-cat (sem-class person)))
                (?nominal-unit-2
                 --
                 (referent ?n)
                 (syn-cat (syn-function nominal))
                 (sem-cat (sem-class person)))
               
                (?named-entity-unit
                 --
                 (HASH form ((meets ?nominal-unit-1 ?nominal-unit-2))))))

  ;;named entities for city 
  (def-fcg-cxn named-entity-function-cxn
               ((?named-entity-unit
                 (referent ?c)
                 (subunits (?nominal-unit-1 ?nominal-unit-2))
                 (syn-cat (phrase-type NP)
                          (named-entity-type city)))
                <-
                (?nominal-unit-1
                 --
                 (referent ?n)
                 (sem-valence (name ?n))
                 (syn-cat (syn-function nominal))
                 (sem-cat (sem-class city)))
                (?nominal-unit-2
                 --
                 (referent ?n)
                 (syn-cat (syn-function nominal))
                 (sem-cat (sem-class city)))
               
                (?named-entity-unit
                 --
                 (HASH form ((meets ?nominal-unit-1 ?nominal-unit-2))))))

  ;;7.Mollie Brown 
  ;;((PERSON P) (NAME N) (:NAME P N) (:OP1 N "Mollie") (:OP2 N "Brown"))

  ;;Mollie
  (def-fcg-cxn Mollie-cxn
               ((?Mollie-unit
                 (referent ?p)
                 (sem-valence (name ?m))
                 (sem-cat (sem-class person))
                 (syn-cat (lex-class proper-noun)
                          (syn-function nominal)))
                <-
                (?Mollie-unit
                 (HASH meaning ((person ?p)
                                (:name ?p ?m)
                                (name ?m)
                                (:op1 ?m "Mollie") ;;first name
                                ))
                 --
                 (HASH form ((string ?Mollie-unit "Mollie"))))))

  ;;Brown
  (def-fcg-cxn Brown-last-name-cxn
               ((?named-entity-unit
                 (subunits (?brown-unit ?first-name-unit))
                 (referent ?p)
                 (syn-cat (phrase-type NP)
                          (named-entity-type person)))
                (?brown-unit
                 (referent ?m)
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
                 (HASH meaning ((:op2 ?m "Brown")))
                 --
                 (HASH form ((string ?brown-unit "Brown"))))
                (?named-entity-unit
                 --
                 (HASH form ((meets ?first-name-unit ?brown-unit))))))

  ;;3. bond Investor
  ;;((PERSON P) (INVEST-01 I) (BOND B) (:ARG0-OF P I) (:ARG1 I B))

  ;;bond
  (def-fcg-cxn bond-cxn
               ((?bond-unit
                 (referent ?b)
                 (syn-cat (lex-class noun)))
                <-
                (?bond-unit
                 (HASH meaning ((bond ?b)
                                (:arg1 ?i ?b)))
                 --
                 (HASH form ((string ?bond-unit "bond"))))))

  ;;compound noun construction
  (def-fcg-cxn compound-noun-cxn
               ((?compound-unit
                 (referent ?ref)
                 (syn-cat (phrase-type NP)
                          (number ?numb)
                          (person 3))
                 (subunits (?noun-unit1 ?noun-unit2))
                 (boundaries (leftmost-unit ?noun-unit1)
                             (rightmost-unit ?noun-unit2)))
                <-
                (?noun-unit1
                 (referent ?ref)
                 --
                 (syn-cat (lex-class noun)))
                (?noun-unit2
                 --
                 (referent ?ref)
                 (syn-cat (lex-class noun))
                 (HASH meaning ((person ?p)
                                (:arg0-of ?p ?i))))
                (?compound-unit
                 --
                 (HASH form ((meets ?noun-unit1 ?nound-unit2))))))

  ;;4. small investor

  ;;((PERSON P) (INVEST-01 I) (SMALL S) (:ARG0-OF P I) (:MANNER I S))

  ;;small
  (def-fcg-cxn small-cxn
               ((?small-unit
                 (referent ?s)
                 (syn-cat (lex-class adjective)
                          (number ?numb)))
                <-
                (?small-unit
                 (HASH meaning ((small ?s)
                                (:manner ?i ?s)))
                 --
                 (HASH form ((string ?bond-unit "small"))))))

  ;;((BOMB B) (ATOM A) (:MOD B A))

  ;;bomb
  (def-fcg-cxn bomb-cxn
               ((?bomb-unit
                 (referent ?b)
                 (syn-cat (lex-class noun)
                          (number sing)))
                <-
                (?bomb-unit
                 (HASH meaning ((bomb ?b)))
                 --
                 (HASH form ((string ?bomb-unit "bomb"))))))

  ;;atomic
  (def-fcg-cxn atomic-cxn
               ((?atomic-unit
                 (referent ?a)
                 (syn-cat (lex-class adjective)
                          (number ?numb)))
                <-
                (?atomic-unit
                 (HASH meaning ((atomic ?a)
                                (:mod ?b ?a)))
                 --
                 (HASH form ((string ?atomic-unit"atomic"))))))

  ;;atom
  (def-fcg-cxn atom-cxn
               ((?atom-unit
                 (referent ?a)
                 (syn-cat (lex-class adjective)
                          (number ?numb)))
                <-
                (?atomic-unit
                 (HASH meaning ((atomic ?a)
                                (:mod ?b ?a)))
                 --
                 (HASH form ((string ?atomic-unit"atom"))))))

  )


            
