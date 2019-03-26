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

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Lexical Constructions
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; ---------------------------------------------------------------------------------------------------
;; Articles
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn the-cxn
              (<-
               (?the-unit
                (syn-cat (lex-class article)
                          (definite +)
                          (number ?number)
                          (syn-function ?func))
                --
                (HASH form ((string ?the-unit "the"))))))

(def-fcg-cxn an-cxn
             (<-
              (?an-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg)
                        (syn-function ?func))
                --
                (HASH form ((string ?an-unit "an"))))))

(def-fcg-cxn a-cxn
             (<-
              (?a-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg)
                        (syn-function ?func))
               --
               (HASH form ((string ?a-unit "a"))))))


;; ---------------------------------------------------------------------------------------------------
;; Adjectives
;; ---------------------------------------------------------------------------------------------------

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

(def-fcg-cxn attractive-cxn
             ((?attractive-unit
               (referent ?a)
                (meaning ((attract-01 ?a)))
                (syn-cat (lex-class adjective)
                         (number sg)
                         (syn-function adjectival))
                (sem-cat (sem-class quality)))
              <-
              (?attractive-unit
               --
               (HASH form  ((string ?attractive-unit "attractive"))))))
 
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

(def-fcg-cxn small-cxn
             ((?small-unit
               (referent ?s)
               (meaning ((small ?s)))
                 (syn-cat (lex-class adjective)
                          (syn-function adjectival))
                 (sem-cat (sem-class manner)))
              <-
              (?small-unit
               --
               (HASH form ((string ?small-unit "small"))))))

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

(def-fcg-cxn white-cxn
             ((?white-unit
               (referent ?w)
               (meaning ((white ?w)))
               (syn-cat (lex-class adjective)
                        (number ?numb)
                        (syn-function predicative))
               (sem-cat (sem-class colour)))
              <-
              (?white-unit
              --
              (HASH form ((string ?white-unit "white"))))))

;; ---------------------------------------------------------------------------------------------------
;; Adverb
;; ---------------------------------------------------------------------------------------------------

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

(def-fcg-cxn not-cxn
             ((?not-unit
               ;;(syn-cat (positive -)
               (syn-cat (lex-class adverb)))
               <-
              (?not-unit
               --
               (HASH form ((string ?not-unit "not"))))))
;; ---------------------------------------------------------------------------------------------------
;; Nouns
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn atom-cxn
             ((?atom-unit
               (referent ?a)
               (meaning ((atom ?a)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (nominalisation -)
                        (syn-function adjectival))
               (sem-cat (sem-class pertainym)))
              <-
               (?atom-unit
                --
                (HASH form ((string ?atom-unit "atom"))))))

(def-fcg-cxn battle-cxn
             ((?battle-unit
               (referent ?b)
               (meaning ((battle-01 ?b)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
              <-
              (?battle-unit
               --
               (HASH form ((string ?battle-unit "battle"))))))

(def-fcg-cxn bomb-cxn
             ((?bomb-unit
               (referent ?b)
               (meaning ((bomb ?b)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal))
               (sem-cat (sem-class inanimate-object)))
              <-
              (?bomb-unit
               --
               (HASH form ((string ?bomb-unit "bomb"))))))

(def-fcg-cxn bond-cxn
             ((?bond-unit
                 (referent ?b)
                 (meaning ((bond ?b)))
                 (syn-cat (lex-class noun)
                          (nominalisation -)
                          (number sg)
                          (syn-function adjectival))
                 (sem-cat (sem-class quality)
                           (sem-role patient)))
              <-
              (?bond-unit
               --
               (HASH form ((string ?bond-unit "bond"))))))

(def-fcg-cxn boy-cxn
             ((?boy-unit
               (referent ?b)
               (meaning ((boy ?b)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal)
                         (part-of-phrase +))
               (sem-cat (sem-class person))
               (boundaries (leftmost-unit ?boy-leftmost-unit)
                           (rightmost-unit ?boy-rightmost-unit)))
              <-
              (?boy-unit
               (HASH meaning ((boy ?b)))
               --
               (HASH form ((string ?boy-unit "boy"))))))

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
                        (syn-function nominal)
                        (part-of-phrase +))
             (boundaries (leftmost-unit ?girl-leftmost-unit)
                         (rightmost-unit ?girl-rightmost-unit)))
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
                        (syn-function nominal)
                        (part-of-phrase +))
               (sem-cat (sem-class arg1))
               (boundaries (leftmost-unit ?girls-leftmost-unit)
                         (rightmost-unit ?girls-rightmost-unit)))
              <-
              (?girls-unit
               --
               (HASH form ((string ?girls-unit "girls"))))))

(def-fcg-cxn history-cxn
             ((?history-unit
               (referent ?h)
               (meaning ((history ?h)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal))
               (sem-cat (sem-class topic)
                        (sem-role patient)))
              <-
              (?history-unit
               --
               (HASH form ((string ?history-unit "history"))))))

(def-fcg-cxn investor-cxn 
             ((?investor-unit
               (referent ?i)
               (meaning ((person ?p)
                         (invest-01 ?i)
                         (:arg0-of ?p ?i)))
               (sem-valence (arg0-of ?i)) ;;a person or thing that does something
               (syn-cat (lex-class noun)
                        (nominalisation +)
                        (number sg)
                        (syn-function nominal))
               (sem-cat (sem-class person)))
              <-
              (?investor-unit
               --
               (HASH form ((string ?investor-unit "investor"))))))

(def-fcg-cxn lawyer-cxn
             ((?lawyer-unit
               (referent ?l)
               (meaning ((lawyer ?l)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function predicative)
                        (person 3)))
               <-
               (?lawyer-unit
                --
                (HASH form ((string ?lawyer-unit "lawyer"))))))

(def-fcg-cxn marble-cxn
             ((?marble-unit
               (referent ?m)
               (meaning ((marble ?m)))
               (syn-cat (lex-class noun)
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
                        (syn-function nominal)))
               <-
               (?opinion-unit
                --
                (HASH form ((string ?opinion-unit "opinion"))))))

(def-fcg-cxn orc-lex-cxn
             ((?orc-unit
               (referent ?o)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
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
                        (syn-function nominal))
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
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
              <-
              (?orcs-unit
               --
               (HASH form ((string ?orcs-unit "orcs"))))))

(def-fcg-cxn president-capitalized-cxn
             ((?president-unit
               (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                         (person 3)
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
                         (person 3)
                         (syn-function nominal))
                (sem-cat (sem-class title))
                (sem-valence (name ?n)))
              <-
              (?president-unit
               --
               (HASH form ((string ?president-unit "president"))))))

(def-fcg-cxn professor-cxn
             ((?professor-unit
               (referent ?t)
               (meaning ((person ?p)
                         (teach-01 ?t)
                         (:arg0-of ?p ?t)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (person 3)
                        (syn-function nominal))
               (sem-cat (sem-class title)))
              <-
              (?professor-unit
               --
               (HASH form ((string ?professor-unit "professor"))))))

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

(def-fcg-cxn soldier-cxn
             ((?soldier-unit
               (referent ?s)
               (meaning ((soldier ?s)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function nominal)
                        (phrase-type +))
               (sem-cat (sem-class agent))
               (boundaries (leftmost-unit ?soldier-leftmost-unit)
                           (rightmost-unit ?soldier-rightmost-unit)))
              <-
              (?soldier-unit
               --
               (HASH form ((string ?soldier-unit "soldier"))))))

(def-fcg-cxn spy-cxn
              ((?spy-unit
                (referent ?s)
                (meaning ((spy ?s)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (syn-function nominal))
                (sem-cat (sem-class person))
                (boundaries (leftmost-unit ?spy-leftmost-unit)
                           (rightmost-unit ?spy-rightmost-unit)))
               <-
               (?spy-unit
                --
                (HASH form  ((string ?spy-unit "spy"))))))

(def-fcg-cxn teacher-cxn
             ((?teacher-unit
               (referent ?t)
               (meaning ((person ?p)
                         (teach-01 ?t)
                         (:arg0-of ?p ?t)))
                (syn-cat (lex-class noun)
                         (number sg)
                         (person 3)
                         (syn-function nominal))
                (sem-cat (sem-class title)))
              <-
              (?teacher-unit
                --
              (HASH form ((string ?teacher-unit "teacher"))))))

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

;; ---------------------------------------------------------------------------------------------------
;; Particular Constructions
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn s-possessive-cxn
             ((?s-possessive-unit
              (syn-cat (syn-function possessive-form)))
              <-
              (?s-possessive-unit
               --
               (HASH form ((string ?s-possessive-unit "'s"))))))

;; ---------------------------------------------------------------------------------------------------
;; Prepositions
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn of-cxn
               ((?of-unit
                 (syn-cat (lex-class preposition)
                          (phrase-type PP)))
                <-
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))))

(def-fcg-cxn to-cxn
               ((?to-unit
                 (syn-cat (lex-class preposition)))
                <-
                (?to-unit
                 --
                 (HASH form ((string ?to-unit "to"))))))

;; ---------------------------------------------------------------------------------------------------
;; Proper Nouns
;; ---------------------------------------------------------------------------------------------------

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
 
(def-fcg-cxn Obama-cxn
              ((?Obama-unit
                (referent ?n)
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)
                         (proper-noun +))
                (sem-cat (sem-class person))
                (meaning ((name ?n)
                          (:op1 ?n "Obama"))))
               <-
               (?Obama-unit
                --
                (HASH form ((string ?Obama-unit "Obama"))))))

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
;; ---------------------------------------------------------------------------------------------------
;; Pronouns
;; ---------------------------------------------------------------------------------------------------

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

(def-fcg-cxn who-cxn 
             ((?who-unit
               (referent ?b)
               (syn-cat (lex-class pronoun)
                        (relative +)))
              <-
              (?who-unit
               --
               (HASH form ((string ?who-unit "who"))))))

;; ---------------------------------------------------------------------------------------------------
;; Verbs
;; ---------------------------------------------------------------------------------------------------

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

(def-fcg-cxn cannot-cxn
             ((?cannot-unit
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (modal +)
                         (main +)
                         (person ?person)
                         (number ?number))
                (sem-valence (:domain ?p))
                (meaning ((possible ?p)
                          (:polarity ?p -))))
              <-
              (?cannot-unit
               --
               (HASH form ((string ?cannot-unit "cannot"))))))

(def-fcg-cxn did-cxn
             ((?did-unit
               (syn-cat (lex-class aux)
                        (finite ?fin)
                        (lemma do)))
              <-
              (?did-unit
               --
               (HASH form ((string ?did-unit "did"))))))
             
(def-fcg-cxn feared-cxn
             ((?feared-unit
               (referent ?f)
               (meaning ((fear-01 ?f)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (simple-past +)))
               <-
               (?feared-unit
                --
                (HASH form ((string ?feared-unit "feared"))))))

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

(def-fcg-cxn opined-cxn
             ((?opined-unit
               (referent ?o)
               (meaning ((opine-01 ?o)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (transitive +)
                        (simple-past +)))
               <-
               (?opined-unit
                --
                (HASH form ((string ?opined-unit "opined"))))))

(def-fcg-cxn pleasing-morph-cxn
             ((?pleasing-unit
               (referent ?p)
               (lex-id please)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (gerund +)
                        (syn-function ?func)))
               <-
               (?pleasing-unit
                --
                (HASH form ((string ?pleasing-unit "pleasing"))))))

(def-fcg-cxn please-morph-cxn
             ((?please-unit
               (referent ?p)
               (lex-id please)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class verb)
                        (modal -)
                        (gerund -)
                        (infinitif +)
                        (syn-function ?func)))
               <-
               (?please-unit
                --
                (HASH form ((string ?please-unit "please"))))))

(def-fcg-cxn please-lex-cxn
             ((?please-unit
               (referent?p)
               (meaning ((please ?p)))
               (syn-cat (lex-class verb)
                        (syn-function ?fun)))
               <-
               (?please-unit
                (lex-id plase))))

(def-fcg-cxn sing-lex-cxn
             ((?sing-unit
               (referent ?s)
               (meaning ((sing-01 ?s)))
               (lex-id sing)
               (syn-cat (lex-class verb)))
               <-
               (?sing-unit
                --
               (lex-id sing))))

(def-fcg-cxn sang-morph-cxn
             ((?sang-unit
               (referent ?s)
               (lex-id sing)
               (meaning ((sing-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (simple-past +)
                        (relative +)))
               <-
               (?sang-unit
                --
                (HASH form ((string ?sang-unit "sang"))))))

(def-fcg-cxn slay-lex-cxn
             ((?slay-unit
               (referent ?s)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)))
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
                        (transitive +)
                        (past-simple +)))
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
                        (finite +)
                        (is-copular -)
                        (transitive +)
                        (present-participle +)))
              <-
              (?slaying-unit
               --
               (HASH form ((string ?slaying-unit "slaying"))))))

(def-fcg-cxn want-lex-cxn
             ((?want-unit
               (referent ?w)
               (lex-id want)
               (syn-cat (lex-class verb))
               (meaning ((want-01 ?w))))
               <-
              (?want-unit
               --
               (lex-id want))))

(def-fcg-cxn wants-morph-cxn
             ((?wants-unit
               (referent ?w)
               (lex-id want)
               (syn-cat (lex-class verb)
                        (to-infinitive +)
                        (person 3)
                        (number sg))
               (meaning ((want-01 ?w))))
               <-
              (?wants-unit
               --
               (HASH form ((string ?want-unit "wants"))))))

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

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Phrasal Constructions
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; ---------------------------------------------------------------------------------------------------
;; Adjectival Phrase Constructions
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase-unit
               (referent ?noun)
               (subunits (?article-unit ?nominal-unit))
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?article-unit)
                           (rightmost-unit ?nominal-rightmost-unit))
               (sem-cat (sem-class ?class)))
               <-
               (?article-unit
               --
               (syn-cat (lex-class article)
                        (definite ?def)
                        (number ?numb)
                        (syn-function ?func)))
               (?nominal-unit
               --
               (referent ?noun)
               (syn-cat (syn-function nominal)
                         (part-of-phrase +))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
               (?noun-phrase-unit
                --
                (HASH form ((meets ?article-unit ?nominal-leftmost-unit))))))

;; ---------------------------------------------------------------------------------------------------
;; Adjective Arg Constructions
;; ---------------------------------------------------------------------------------------------------
  
(def-fcg-cxn adjective-noun-unit-cxn ;; arg0-of ;; match in sem quality
             ((?adjective-noun-unit 
                (referent ?noun)
                (meaning ((:arg0-of ?noun ?quality)))
                (syn-cat (phrase-type nominal-phrase)
                         (syn-function nominal)
                         (part-of-phrase +))
                (subunits (?adjective-unit ?noun-unit))
                (boundaries (leftmost-unit ?adjective-unit)
                           (rightmost-unit ?noun-unit)))
              <-
              (?adjective-unit
               --
               (referent ?quality)
               (syn-cat (lex-class adjective)
                        (syn-function adjectival))
               (sem-cat (sem-class quality)))
              (?noun-unit
               --
               (referent ?noun)
               (syn-cat  (lex-class noun)
                         (number ?numb)
                         (syn-function nominal)))
              (?adjective-noun-unit 
               --
               (HASH form ((meets ?adjective-unit ?noun-unit))))))

(def-fcg-cxn patient-nominal-cxn ;; arg1 of the adjective 
             ((?patient-of-nominal
               (referent ?person)
               (meaning ((:arg1 ?person ?noun)))
               (sem-cat (sem-class ?class)
                        (sem-role patient))
               (syn-cat (phrase-type AP)
                        (syn-function adjectival)
                        (number ?numb)
                        (nominalisation +))
               (subunits (?first-noun-unit ?second-noun-unit)))
              <-
              (?first-noun-unit
               --
               (referent ?noun)
               (syn-cat (lex-class noun))
               (sem-cat (sem-role patient)))
              (?second-noun-unit
               --
              (referent ?person)
              (syn-cat (lex-class ?lex-class)
                       (number ?numb)
                       (syn-function ?func))
              (sem-cat (sem-class ?class)))
              (?patient-of-nominal
               --
               (HASH form ((meets ?first-noun-unit ?second-noun-unit))))))

(def-fcg-cxn adjective-manner-cxn ;; :manner 
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

 (def-fcg-cxn pertainym-adjective-noun-cxn ;; mod
              ((?pertainym-adjective-noun-unit
                (referent ?ref)
                (meaning ((:mod ?ref ?type)))
                (syn-cat (syn-function nominal)
                         (number ?nb))
                (subunits (?adjective-unit ?noun-unit)))
                <-
                (?type-unit
                 --
                 (referent ?type)
                 (syn-cat (lex-class ?lex-class))
                 (sem-cat (sem-class pertainym)))
                (?noun-unit
                 --
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (number ?nb)
                          (syn-function nominal))
                  (sem-cat (sem-class inanimate-object)))
                (?pertainym-adjective-noun-unit
                 --
                 (HASH form ((meets ?adjective-unit ?noun-unit))))))

(def-fcg-cxn arg1of-noun-cxn ;; arg1-of
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

;; ---------------------------------------------------------------------------------------------------
;; Proper Nouns Constructions 
;; ---------------------------------------------------------------------------------------------------
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
               (syn-cat (syn-function nominal))
               (sem-cat (sem-class ?class)))
              (?nominal-unit-2
               --
               (referent ?n)
               (syn-cat (syn-function nominal)
                        (proper-noun +)))
              (?named-entity-unit
               --
               (HASH form ((meets ?nominal-unit-1 ?nominal-unit-2))))))

(def-fcg-cxn proper-noun-article-title-cxn ;; Obama the president 
              ((?proper-noun-article-title-unit
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
               (?proper-noun-article-title-unit
                --
                 (HASH form ((meets ?nominal-unit-11 ?article-unit)
                             (meets ?article-unit ?nominal-unit-22))))))

;; ---------------------------------------------------------------------------------------------------
;; Nominal Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
 (def-fcg-cxn compound-noun-noun-source-cxn ;;source
              ((?compound-noun-noun-source-unit
                (referent ?ref)
                (meaning ((:source ?ref ?c)))
                (syn-cat (lex-class noun)
                         (compound +)
                         (syn-function nominal))
                (sem-cat (sem-class age))
                (subunits (?np-unit ?second-noun-unit))
                (boundaries (leftmost-unit ?np-age-unit-leftmost-unit)
                            (rightmost-unit ?second-noun-unit)))
               <-
               (?np-age-unit
                --
                (referent ?c)
                (syn-cat (phrase-type np))
                (boundaries
                (leftmost-unit ?np-age-unit-leftmost-unit)
                (rightmost-unit ?np-age-unit-rightmost-unit)))
               (?second-noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)
                         ;;(number ?numb)
                         (syn-function nominal))
                (sem-cat (sem-class person)))
               (?compound-noun-noun-source-unit
                --
                (HASH form ((precedes ?np-age-unit-rightmost-unit ?second-noun-unit))))))

;; ---------------------------------------------------------------------------------------------------
;; Verbal Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn VP-cxn
             ((?vp-unit
               (referent ?inf)
               (syn-cat (phrase-type VP)
                        (number ?n)
                        (person ?p)
                        (syn-function verbal))
               (meaning ((:domain ?main ?inf)))
               (subunits (?main-verb ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?main-verb)))
               <-
               (?main-verb
                --
                (referent ?main)
                (syn-cat (main +)))
                (?infinitive-verb
                --
                (referent ?inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((precedes ?main-verb ?infinitive-verb))))))
                                    
;; ---------------------------------------------------------------------------------------------------
;; Adverbial Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn adverb-manner-cxn
             ((?adverb-manner-unit
               (meaning ((:manner ?main ?adv)))
               (subunits (?vp-unit ?adverb-unit))
               (referent ?main)
               (syn-cat (phrase-type AP))
               (boundaries
                 (rightmost-unit ?vp-unit-leftmost)
                 (leftmost-unit ?adverb-unit-rightmost)))
               <-
               (?vp-unit
                --
                (referent ?main)
                (syn-cat (lex-class verb)
                         (finite +)))
               (?adverb-unit
               --
               (referent ?adv)
                (syn-cat (lex-class adverb)
                        (number ?numb)
                        (syn-function ?func))
               (sem-cat (sem-class manner)))
               (?adverb-manner-unit
                --
                (HASH form ((precedes ?vp-unit ?adverb-unit))))))
                        
;; ---------------------------------------------------------------------------------------------------
;; Arg Structure Constructions
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn np-vp-np=arg0-cxn ;; active-intransitive-cxn
             ((?clause-unit
               (meaning ((:arg0 ?verb ?arg0)))
               (subunits (?vp-unit ?agent-unit))
               (referent ?verb)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?agent-leftmost-unit)))
              <-
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (phrase-type vp)
                        (syn-function verbal))
               (boundaries
                (leftmost-unit ?vp-leftmost-unit)
                (rightmost-unit ?vp-rightmost-unit)))
              (?agent-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type np)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?agent-leftmost-unit)
                (rightmost-unit ?agent-rightmost-unit)))
              (?clause-unit
               --
               (HASH form ((precedes ?agent-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn AP-NP-np=arg0-cxn
             ((?adverbialclause-unit
               (meaning ((:arg0 ?main ?arg0)))
               (subunits (?vp-unit ?arg0-unit))
               (referent ?g)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?arg0-leftmost-unit))
               (syn-cat (phrase-type VP)))
              <-
              (?ap-unit
               --
               (referent ?main)
               (syn-cat (phrase-type AP))
               (boundaries
                (leftmost-unit ?ap-leftmost-unit)
                (rightmost-unit ?ap-rightmost-unit)))
              (?arg0-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (phrase-type +))
               (boundaries
                (leftmost-unit ?subject-leftmost-unit)
                (rightmost-unit ?subject-rightmost-unit)))
              (?adverbialclause-unit
               --
               (HASH form ((meets ?subject-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn predicative-cxn 
             ((?predicative-unit
               (subunits (?vp-unit ?nominal-predicative-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?domain ?referring)))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?nominal-predicative-rightmost-unit))
               (referent ?referring))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?nominal-predicative-unit
               --
               (referent ?domain)
               (syn-cat (syn-function predicative)))
               (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (phrase-type ?phrase-type)
                        (number ?numb)
                        (syn-function ?func))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?referring-noun-rightmost-unit)))
              (?predicative-unit
               --
               (HASH form ((meets ?referring-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?nominal-predicative-leftmost-unit))))))

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
                        (syn-function nominal)))
              (?np-y-unit
               --
               (referent ?g)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (syn-function nominal)
                        (compound ?compound))
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
                (syn-cat (transitive +)))
              (?direct-object-unit
               --
               (referent ?o)
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
              (?active-transitive-unit
               --
              (HASH form ((meets ?vp-unit ?direct-object-unit))))))

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
               (syn-cat (phrase-type NP))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        (is-copular -)
                        (finite +)))
              (?arg0-of-verb-unit
               --
               (HASH form ((precedes ?named-entity-leftmost-unit ?vp-unit))))))

(def-fcg-cxn arg0-inverse-role-relative-cxn
             ((?arg0-of-relative-unit
               (meaning ((:arg0-of ?arg0-of ?verb)))
               (subunits (?active-transitive-unit ?relative-unit ?named-entity-unit))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?active-transitive-rightmost-unit))
               (referent ?arg0-of))
              <-
               (?named-entity-unit
               --
               (referent ?arg0-of)
               (syn-cat (phrase-type np))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
                (?relative-unit
                --
                (syn-cat (lex-class pronoun)
                         (relative +)))
                (?active-transitive-unit
                 --
                 (referent ?verb)
                 (boundaries (leftmost-unit ?active-transitive-leftmost-unit)
                             (rightmost-unit ?active-transitive-rightmost-unit)))
                (?arg0-of-relative-unit
                 --
                 (HASH form ((precedes ?named-entity-rightmost-unit ?relative-unit)
                             (precedes ?relative-unit ?active-transitive-leftmost-unit))))))

(def-fcg-cxn auxiliar-infinitive-negative-cxn
             ((?auxiliar-infinitive-negative-unit
               (referent ?g)
               (syn-cat (phrase-type vp))
               (subunits (?aux-unit ?not-unit ?infinitive-unit))
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
                (?infinitive-unit
                 --
                 (referent ?g)
                 (syn-cat (lex-class verb)
                          (infinitive +)))
                (?auxiliar-infinitive-negative-unit
                 --
                 (HASH form ((precedes ?aux-unit ?not-unit)
                             (precedes ?not-unit ?infinitive-unit))))))

(def-fcg-cxn V-to-infinitive-cxn
             ((?V-to-infinitive-unit
               (meaning ((:arg1 ?verb ?inf)))
               (subunits (?finite-verb-unit ?to-unit ?infinitive-unit))
               (referent ?verb)
               (syn-cat (phrase-type VP))
               (boundaries
                (leftmost-unit ?finite-verb-unit)
                (rightmost-unit ?infinitive-unit)))
              <-
              (?finite-verb-unit
               --
               (referent ?verb)
               (syn-cat (lex-class verb)
                        (number ?numb)
                        (person ?pers)
                        (to-infinitive +)))
              (?to-unit
               --
              (form ((string ?to-unit "to"))))
              (?infinitive-unit
               --
               (referent ?inf)
               (syn-cat (lex-class verb)
                        (infinitive +)))
              (?V-to-infinitive-unit
               --
               (HASH form ((precedes ?finite-verb-unit ?to-unit)
                           (precedes ?to-unit ?infinitive-unit))))))

)

#|
 
66 lexical
21 phrasal|arg0
= 87

135
43 phrasal|arg0
|#

