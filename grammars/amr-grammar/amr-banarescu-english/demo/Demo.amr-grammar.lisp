;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; AMR grammar Banarescu Corpus developed by Martina Galletti (Spring 2019)
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(in-package :amr-grammar)
(def-fcg-constructions amr-Banarescu-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first)
                       (:parse-goal-tests :no-strings-in-root :no-applicable-cxns))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 1. President Obama
;; '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(def-fcg-cxn president-capitalized-cxn
             ((?president-unit
               (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                         (person 3))
                (sem-cat (sem-class title))
                (sem-valence (name ?n)))
              <-
              (?president-unit
               --
               (HASH form ((string ?president-unit "President"))))))

(def-fcg-cxn Obama-cxn
              ((?Obama-unit
                (referent ?n)
                (syn-cat (proper-noun +))
                (meaning ((name ?n)
                          (:op1 ?n "Obama"))))
               <-
               (?Obama-unit
                --
                (HASH form ((string ?Obama-unit "Obama"))))))

(def-fcg-cxn proper-noun-entity-cxn ;; President Obama
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
               (sem-cat (sem-class title)))
              (?nominal-unit-2
               --
               (referent ?n)
               (syn-cat (proper-noun +)))
              (?named-entity-unit
               --
               (HASH form ((meets ?nominal-unit-1 ?nominal-unit-2))))))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 2. The marble is white
;; '((WHITE W) (MARBLE M) (:DOMAIN W M)))
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(def-fcg-cxn the-cxn
              (<-
               (?the-unit
                (syn-cat (lex-class article)
                          (definite +)
                          (number ?number))
                --
                (HASH form ((string ?the-unit "the"))))))

(def-fcg-cxn marble-cxn
             ((?marble-unit
               (referent ?m)
               (meaning ((marble ?m)))
               (syn-cat (lex-class noun)
                        (person 3)
                        (number sg)
                        (syn-function nominal)
                        (part-of-phrase +))
                (sem-cat (sem-class subject))
                (boundaries (leftmost-unit ?marble-leftmost-unit)
                            (rightmost-unit ?marble-rightmost-unit)))
              <-
              (?marble-unit
               --
               (HASH form ((string ?marble-unit "marble"))))))

(def-fcg-cxn is-cxn
             ((?is-unit
               (referent ?is)
               (syn-cat (lex-class verb)
                        (is-copular +)))
              <-
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))))

(def-fcg-cxn white-cxn
             ((?white-unit
               (referent ?w)
               (meaning ((white ?w)))
               (syn-cat (lex-class adjective)
                        (syn-function predicative))
               (boundaries (leftmost-unit ?white-unit)
                           (rightmost-unit ?white-unit)))
              <-
              (?white-unit
              --
              (HASH form ((string ?white-unit "white"))))))

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase-unit
               (referent ?noun)
               (subunits (?article-unit ?nominal-unit))
               (syn-cat (phrase-type noun-phrase)
                         (part-of-phrase +))
               (boundaries (leftmost-unit ?article-unit)
                           (rightmost-unit ?nominal-rightmost-unit))
               (sem-cat (sem-role ?role)))
               <-
               (?article-unit
               --
               (syn-cat (lex-class article)
                        (definite ?def)
                        (number ?numb)))
               (?nominal-unit
               --
               (referent ?noun)
               (syn-cat (syn-function ?nominal)
                        (part-of-phrase +))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
               (?noun-phrase-unit
                --
                (HASH form ((meets ?article-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn predicative-cxn 
             ((?predicative-clause
               (subunits (?vp-unit ?predicative-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?predicative ?referring)))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?pred-rightmost-unit))
               (referent ?referring))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?predicative-unit
               --
               (referent ?predicative)
               (syn-cat (syn-function predicative))
               (boundaries (leftmost-unit ?pred-leftmost-unit)
                           (rightmost-unit ?pred-rightmost-unit)))
               (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?referring-noun-rightmost-unit)))
              (?predicative-clause
               --
               (HASH form ((meets ?referring-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?pred-leftmost-unit))))))

             
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 3. The boy cannot go 
;; '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(def-fcg-cxn boy-cxn
             ((?boy-unit
               (referent ?b)
               (meaning ((boy ?b)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal)
                        (part-of-phrase +))
               (sem-cat (sem-role agent))
               (boundaries (leftmost-unit ?boy-leftmost-unit)
                           (rightmost-unit ?boy-rightmost-unit)))
              <-
              (?boy-unit
               (HASH meaning ((boy ?b)))
               --
               (HASH form ((string ?boy-unit "boy"))))))

(def-fcg-cxn cannot-cxn
             ((?cannot-unit
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal +)
                         (aux +))
                (sem-valence (:domain ?p))
                (meaning ((possible ?p)
                          (:polarity ?p -))))
              <-
              (?cannot-unit
               --
               (HASH form ((string ?cannot-unit "cannot"))))))

(def-fcg-cxn go-cxn
             ((?go-unit
               (referent ?g)
               (syn-cat (lex-class verb)
                        (infinitive +))
                (meaning ((go-01 ?g))))
              <-
              (?go-unit
                --
                (HASH form ((string ?go-unit "go"))))))

(def-fcg-cxn np-vp-np=arg0-cxn ;; active-intransitive-cxn
             ((?clause-unit
               (meaning ((:arg0 ?verb ?arg0)))
               (subunits (?vp-unit ?agent-unit))
               (referent ?verb)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?agent-leftmost-unit))
               (syn-cat (phrase-type VP)))
              <-
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (syn-function verbal)
                        (part-of-phrase +))
               (boundaries
                (leftmost-unit ?vp-leftmost-unit)
                (rightmost-unit ?vp-rightmost-unit)))
              (?agent-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?agent-leftmost-unit)
                (rightmost-unit ?agent-rightmost-unit))
               (sem-cat (sem-role agent)))
              (?clause-unit
               --
               (HASH form ((meets ?agent-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn aux-VP-cxn
             ((?vp-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type VP)
                        (syn-function verbal)
                        (part-of-phrase +))
               (meaning ((:domain ?ref-aux ?ref-inf)))
               (subunits (?aux ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (aux +)))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((precedes ?aux ?infinitive-verb))))))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 4. "The girl's opinion" and "What the girl opined": same meaning but different constructions
;; '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(def-fcg-cxn girl-lex-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)))
              <-
              (?girl-unit
               (lex-id girl))))
 
(def-fcg-cxn girl-morph-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (lex-id girl)
               (syn-cat (lex-class noun)
                        (person 3)
                        (number sg)
                        (syn-function agent)
                        (part-of-phrase +))
             (boundaries (leftmost-unit ?girl-leftmost-unit)
                         (rightmost-unit ?girl-rightmost-unit)))
              <-
              (?girl-unit
               --
               (HASH form ((string ?girl-unit "girl"))))))

(def-fcg-cxn opinion-cxn
             ((?opinion-unit
               (referent ?o)
               (meaning ((thing ?t)
                         (opine-01 ?o)
                         (:arg1-of ?t ?o)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal)
                        (part-of-phrase +)))
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

(def-fcg-cxn x-s-y-cxn
             ((?x-s-y-unit
               (referent ?g)
               (meaning ((:arg0 ?o ?g)))
               (subunits (?np-x-unit ?noun-y-unit ?possessive-unit))
               (boundaries (leftmost-unit ?np-y-leftmost-unit)
                           (rightmost-unit ?np-x-rightmost-unit)))
              <-
              (?noun-y-unit
               --
               (referent ?o)
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              (?np-x-unit
               --
               (referent ?g)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?np-x-leftmost-unit)
                (rightmost-unit ?np-x-rightmost-unit)))
              (?possessive-unit
               --
               (form ((string ?possessive-unit "'s")))
               (syn-cat (syn-function possessive-form)))
              (?x-s-y-unit
               --
               (HASH form ((precedes ?np-x-rightmost-unit ?possessive-unit)
                           (precedes ?possessive-unit ?noun-y-unit))))))

(def-fcg-cxn what-cxn 
             ((?what-unit
               (referent ?t)
               (meaning ((thing ?t)))
               (syn-cat (lex-class pronoun))
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
                        (transitive +)))
               <-
               (?opined-unit
                --
                (HASH form ((string ?opined-unit "opined"))))))

(def-fcg-cxn VP-cxn
             ((?vp-unit
               (referent ?ref)
               (syn-cat (phrase-type VP)
                        (syn-function verbal)
                        (part-of-phrase +)
                        (transitive ?trans))
               (subunits (?finite-verb))
               (boundaries (rightmost-unit ?finite-verb)
                           (leftmost-unit ?finite-verb)))
               <-
                (?finite-verb
                --
                (referent ?ref)
                (syn-cat (lex-class verb)
                         (finite +)
                         (NOT (aux +))))))

(def-fcg-cxn arg1of-before-transitive-verb-cxn
             ((?vp-unit
               (meaning ((:arg1-of ?t ?o)))
               (subunits (?object-unit)))
              <-
              (?object-unit
               --
               (referent ?t)
               (syn-cat (lex-class pronoun))
               (sem-cat (sem-class object)))
               (?vp-unit
               --
               (referent ?o)
               (syn-cat (phrase-type vp)
                        (transitive +))
               (boundaries
                (leftmost-unit ?vp-leftmost-unit)
                (rightmost-unit ?vp-rightmost-unit))
               (HASH form ((meets ?object-unit ?np-leftmost-unit))))
               (?np-unit
               --
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?np-leftmost-unit)
                (rightmost-unit ?np-rightmost-unit)))))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 5. "A taxable fund" and "An edible sandwich" : two AMR meaning for similar adjectives
;;             1. ((FUND F) (TAX-01 T) (:ARG1-OF F T)))
;;             2. ((SANDWICH S) (EAT-01 E) (POSSIBLE P) (:ARG1-OF S E) (:DOMAIN-OF E P)))
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(def-fcg-cxn a-cxn
             (<-
              (?a-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg)
                        (syn-function ?func))
               --
               (HASH form ((string ?a-unit "a"))))))

(def-fcg-cxn taxable-cxn
             ((?taxable-unit
                (referent ?t)
                (meaning ((tax-01 ?t)))
                (syn-cat (lex-class adjective)
                         (syn-function adjectival))
                (sem-cat (sem-class possibility)))
              <-
              (?taxable-unit
               --
               (HASH form  ((string ?taxable-unit "taxable"))))))

(def-fcg-cxn fund-cxn
             ((?fund-unit
               (referent ?f)
                (meaning ((fund ?f)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (person 3)
                         (syn-function nominal))
                (sem-cat (sem-class inanimate-object))
                (boundaries (leftmost-unit ?fund-leftmost-unit)
                           (rightmost-unit ?fund-rightmost-unit)))
               <-
               (?fund-unit
                --
                (HASH form ((string ?fund-unit "fund"))))))

(def-fcg-cxn an-cxn
             (<-
              (?an-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg)
                        (syn-function ?func))
                --
                (HASH form ((string ?an-unit "an"))))))

(def-fcg-cxn edible-cxn
             ((?edible-unit
               (referent ?e)
               (meaning ((eat-01 ?e)
                          (possible ?p)
                          (:domain-of ?e ?p)))
               (syn-cat (lex-class adjective)
                         (syn-function adjectival))
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
                         (syn-function nominal))
                (sem-cat (sem-class inanimate-object))
                (boundaries (leftmost-unit ?sandwich-leftmost-unit)
                           (rightmost-unit ?sandwich-rightmost-unit)))
               <-
               (?sandwich-unit
                --
                (HASH form ((string ?sandwich-unit "sandwich"))))))

(def-fcg-cxn arg1of-noun-cxn 
             ((?arg1of-noun-unit
               (referent ?noun)
               (meaning ((:arg1-of ?noun ?adj)))
               (syn-cat (phrase-type nominal-phrase)
                        (syn-function nominal)
                        (part-of-phrase +))
               (subunits (?adjective-unit ?noun-unit))
               (boundaries (leftmost-unit ?adjective-unit)
                           (rightmost-unit ?noun-unit)))
              <-
              (?adjective-unit
               --
               (referent ?adj)
               (syn-cat (lex-class adjective)
                        (syn-function adjectival))
               (sem-cat (sem-class possibility)))
               (?noun-unit
                --
                (referent ?noun)
                (syn-cat  (lex-class noun)
                          (number ?numb)
                          (syn-function nominal)))
               (?arg1of-noun-unit
                --
                (HASH form ((meets ?adjective-unit ?noun-unit))))))

)
