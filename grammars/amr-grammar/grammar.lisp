;;AMR grammar Banarescu Corpus developed by Martina Galletti (Spring 2019)
;;-------------------------------------------------------
;; Lexical constructions covered so far:
;; - investor-cxn
;; - bond-cxn
;; - zintan-cxn
;;--------------------------------------------------------
;; Grammatical constructions covered so far:
;; - compound-noun+nominalised-verb-cxn

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
                 (referent ?C)
                 (syn-cat (lex-class proper-noun)
                          (syn-function nominal)
                          (phrase-type NP))
                 (sem-cat (sem-class location)))
                <-
                (?Zintan-unit
                 (HASH meaning ((CITY ?C) (NAME ?N) (:NAME ?C ?N) (:OP1 ?N "Zintan")))
                 --
                 (HASH form ((string ?Zintan-unit "Zintan")))))))





#|
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

|#
            
