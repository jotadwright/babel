(ql:quickload :fcg)
(in-package :fcg)

(defun my-head-menu () 
  (clear-page)
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (add-element '((h1) "Fillmore's Construction Grammar from 1988"))
  (add-element '((p) "This is an implementation of the examples of constructions present in the following article:"))
  (add-element '((p) "Fillmore, C. J. (1988, October). The mechanisms of \"construction grammar\". In Annual Meeting of the Berkeley Linguistics Society (Vol. 14, pp. 35-55)."))
  (add-element '((p) "In his article, Fillmore describes certain constructions. We examplify the constructions that are drawn and explained by Fillmore and some that are only mentioned. We divide this demonstration into the following parts:"))
  (add-element '((h3)  ((a :href "#determination") "I. The determination construction")))
  (add-element '((h3)  ((a :href "#unique-role") "II. The unique role construction")))
  (add-element '((h3)  ((a :href "#fronting") "III. The \"fronting to that\" construction")))
  (add-element '((h3)  ((a :href "#non-max-v") "IV. Non maximal V-phrase")))
  (add-element '((h3)  ((a :href "#subj-pred") "V. The subject-predicate construction")))
  (add-element '((h3)  ((a :href "#cousin-removed") "VI. The x-cousin-y-removed construction")))
  (add-element '((h3)  ((a :href "#give") "VII. To give")))
  (add-element '((h3)  ((a :href "#inversion") "VIII. Inversion of maximal V-phrase")))
  (add-element '((h3)  ((a :href "#wh-phrase") "IX. Wh-phrases")))
  (add-element '((h3)  ((a :href "#contribute") "X. To contribute")))
  (add-element '((h3)  ((a :href "#worth") "XI. Worth-ing")))
  (add-element '((h3)  ((a :href "#do-ones-best") "XII. Do one's best")))
  (add-element '((h3)  ((a :href "#correlative") "XIII. Correlative conditional")))
  (add-element '((p :style "color:darkred") "DISCLAIMER: It is recommended to use Firefox or Safari to optimally explore the contents of this page.")))

(my-head-menu)

(defvar *fillmores-cxns*)

(def-fcg-constructions fillmores-grammar
  :cxn-inventory *fillmores-cxns*
  :hashed t
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (sequences set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (dependents sequence)
                  (args sequence)
                  (footprints set)
                  (boundaries sequence))
  :fcg-configurations ((:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:max-number-of-nodes . 1000)
                       ;; goal tests for comprehension
                       (:parse-goal-tests
                        :no-sequence-in-root
                        ;; :no-applicable-cxns ;; succeeds if no more cxns can apply
                        :connected-semantic-network ;; succeeds if the semantic network is fully connected
                        :connected-structure) ;; succeeds if all units are connected
                       ;; goal tests for formulation
                       (:production-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :no-meaning-in-root)) ;; succeeds if no meaning predicates remain in root
  :hierarchy-features (subunits dependents))

;-----------------;
;determination cxn;
;-----------------;
;; determination cxn : a determiner and a non-maximal nominal head (max +)

(def-fcg-cxn the-cxn
             ((?the-unit
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number ?number)))
               (sem-cat (definiteness definite))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?the-unit
               (HASH meaning ((unique ?x)))
               --
               (HASH form ((sequence "the" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn a-cxn
             ((?a-unit
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number singular)))
               (sem-cat (definiteness indefinite))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?a-unit
               (HASH meaning ((instance ?x)))
               --
               (HASH form ((sequence "a" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn an-cxn
             ((?an-unit
               (syn-cat (lex-class (role det)
                                   (cat Art))
                        (agreement (number singular)))
               (sem-cat (definiteness indefinite))
               (morph-form (starts-with vowel))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?an-unit
               (HASH meaning ((instance ?x)))
               --
               (HASH form ((sequence "an" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn my-cxn
             ((?my-unit
               (syn-cat (lex-class (role det)
                                   (cat PN)) ;; possessive nominal
                        (agreement (number ?number)))
               (sem-cat (definiteness ?definiteness))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?my-unit
               (HASH meaning ((poss-nom-1st-pers ?x))) ;; possessive nominal first pers
               --
               (HASH form ((sequence "my" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn this-cxn
             ((?this-unit
               (syn-cat (lex-class (role det)
                                   (cat Dem)) ;;for demonstrative
                        (agreement (number singular)))
               (sem-cat (definiteness definite))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?this-unit
               (HASH meaning ((demonstrative-close-object ?x)))
               --
               (HASH form ((sequence "this" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn these-cxn
             ((?this-unit
               (syn-cat (lex-class (role det)
                                   (cat Dem))
                        (agreement (number plural)))
               (sem-cat (definiteness definite))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?this-unit
               (HASH meaning ((demonstrative-close-objects ?x)))
               --
               (HASH form ((sequence "these" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn joe-cxn
             ((?joe-unit
               (syn-cat (lex-class (cat N)
                                   (max +) ;; because the external syntax of Joe is that of a maximal phrase
                                   (min +)
                                   (role S));; because "joe" is a lexical item
                        (agreement (number singular)))
               (sem-cat (sem-class person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?joe-unit
               (HASH meaning ((joe ?x)))
               --
               (HASH form ((sequence "Joe" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "Joe" :cxn-inventory *fillmores-cxns*)

;; (cat N) (max  ) = mass noun = any noun that represents something impossible or difficult to count
;; maximality in mass nouns are left unspecified

(def-fcg-cxn air-cxn
             ((?air-unit
               (syn-cat (lex-class (cat N)
                                   (max ?undefined) ;; (max  ) becomes (max ?undefined) because if not the feature is not recognized in FCG
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with vowel))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?air-unit
               (HASH meaning ((air ?x)))
               --
               (HASH form ((sequence "air" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn rice-cxn
             ((?rice-unit
               (syn-cat (lex-class (cat N)
                                   (max ?undefined)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?rice-unit
               (HASH meaning ((rice ?x)))
               --
               (HASH form ((sequence "rice" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn intelligence-cxn
             ((?intelligence-unit
               (syn-cat (lex-class (cat N)
                                   (max ?undefined)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with vowel))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?intelligence-unit
               (HASH meaning ((intelligence ?x)))
               --
               (HASH form ((sequence "intelligence" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn intelligences-cxn
             ((?intelligences-unit
               (syn-cat (lex-class (cat N)
                                   (max ?undefined)
                                   (min +))
                        (agreement (number plural)))
               (morph-form (starts-with vowel))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?intelligences-unit
               (HASH meaning ((intelligences ?x)))
               --
               (HASH form ((sequence "intelligences" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)
                                
;; (comprehend-and-formulate "intelligence" :cxn-inventory *fillmores-cxns*)

;; (cat N) (max -) = count noun = any noun that represents something that we can count

(def-fcg-cxn pen-cxn
             ((?pen-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?pen-unit
               (HASH meaning ((pen ?x)))
               --
               (HASH form ((sequence "pen" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn book-cxn
             ((?book-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?book-unit
               (HASH meaning ((book ?x)))
               --
               (HASH form ((sequence "book" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn bottle-cxn
             ((?bottle-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?bottle-unit
               (HASH meaning ((bottle ?x)))
               --
               (HASH form ((sequence "bottle" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn bottles-cxn
             ((?bottles-unit
               (syn-cat (lex-class (cat N)
                                   (max ?undefined) ;; when they are made plural, count nouns are "de-marked" to reuse Fillmore's expression
                                   (min +))
                        (agreement (number plural)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?bottles-unit
               (HASH meaning ((bottles ?x)))
               --
               (HASH form ((sequence "bottles" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn child-cxn
             ((?child-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left-1 ?right-1)))
               <-
               (?child-unit
                (HASH meaning ((child ?x)))
                --
                (HASH form ((sequence "child" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn foolish-cxn
             ((?foolish-unit
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left-1 ?right-1)))
              <-
              (?foolish-unit
               (HASH meaning ((foolish ?x)))
               --
               (HASH form ((sequence "foolish" ?left-1 ?right-1)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn modified-count-noun-cxn
             ((?modified-count-noun-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min -))
                        (agreement (number ?number)))
               (morph-form (starts-with ?starts-with))
               (args (?z))
               (subunits (?adj-unit ?noun-unit))
               (boundaries (?adj-left ?noun-right)))
              <-
              (?adj-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (morph-form (starts-with ?starts-with))
               (boundaries (?adj-left ?adj-right)))
              (?noun-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number ?number)))
               (boundaries (?noun-left ?noun-right)))
              (?modified-count-noun-unit
               (HASH meaning ((arg1 ?z ?y ?x)))
               --
               (HASH form ((sequence " " ?adj-right ?noun-left)))))
              :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn determination-cxn
             ((?determination-unit
               (syn-cat (lex-class (cat N)  
                                   (max +)
                                   (min -))
                        (agreement (number ?number)))
               (args (?z))
               (subunits (?det-unit ?noun-unit))
               (boundaries (?det-left ?noun-right)))
              <-
              (?det-unit
               (syn-cat (lex-class (role det))
                        (agreement (number ?number)))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               --
               (syn-cat (lex-class (role det))
                        (agreement (number ?number)))
               (morph-form (starts-with ?starts-with))
               (boundaries (?det-left ?det-right)))
              (?noun-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat N)
                                   (max -))
                        (agreement (number ?number)))
               (morph-form (starts-with ?starts-with))
               (boundaries (?noun-left ?noun-right)))
              (?determination-unit
               (HASH meaning ((noun-phrase ?z ?x)))
               --
               (HASH form ((sequence " " ?det-right ?noun-left)))))
             :cxn-inventory *fillmores-cxns*)

;; mass nouns exhibit the same use that count nouns in the determination-cxn
;; but mass noun can also be used as proper noun, which as maximal phrase (which is why their maximilality is left undefined)

(defun display-determination (cxn-inventory)
  (add-element '((a :name  "determination")))
  (add-element '((h3) "The determination construction"))
  (add-element '((p) "The determination construction is made out of two elements: a determinor and a non-maximal nominal head (max +). A determinor is either an article (a, an, the), a possessive nominal (my, your, his, etc.), or a demonstrative (this, that, these, those). The non-maximal nominal head can be a single lexical item (min +) or a lexical item and its adjective (min -). A single lexical item can be a mass noun or a count noun."))
  (add-element '((p) "Inside the construction inventory, we load the following determinors: the, a, an, my, this, these. We show the constructions for the items \"the\", \"a\" and \"my\" below."))
  (add-element '((p) "On the right hand side of the unit is the lock. It is made out of a comprehension lock (top) and a production lock (down). On the left-hand side is the post-conditions. As specified by Fillmore, it includes the role and category of the item (ex: det, art for article, and number). We organized the different elements inside a syn-cat feature and added a morph-form feature and a sem-cat feature. When a feature is followed by an undefined value (such as ?number) then it can be filled by any value."))
  (add-element (make-html (find-cxn 'the-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'a-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'my-cxn cxn-inventory)))
  (add-element '((p) "We also examplify the second part of the determination construction with three types of non-maximal noun phrase: count nouns, mass nouns and the association of an adjective and a count noun)."))
  (add-element '((p) "Here are an example of a singular count noun, a singular mass noun, and the modified-count-noun-cxn. We also comprehend and formulate \"foolish child\"."))
  (add-element '((p) "The determination construction cannot call a proper noun because of the incompatible maximality: the determination construction asks for a non-maximal noun phrase but proper nouns are maximal phrases (an example is shown ith the joe-cxn stated by Fillmore)."))
  (add-element (make-html (find-cxn 'pen-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'air-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'joe-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'modified-count-noun-cxn cxn-inventory))))

(display-determination *fillmores-cxns*)

;; mass nouns exhibit the same use that count nouns in the determination-cxn
;; but mass noun can also be used as proper nouns, which are maximal phrases (which is why their maximilality is left undefined)

(comprehend-and-formulate "my pen" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "the air" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "a foolish child" :cxn-inventory *fillmores-cxns*)

;; incorrect:
#| (comprehend-and-formulate "an rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "these rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "an pen" :cxn-inventory *fillmores-cxns*) |#

;-----------------------------------;
; Unique Role Nominal Predicate Cxn ;
;-----------------------------------;

(delete-cxn 'a-cxn *fillmores-cxns*)
(delete-cxn 'an-cxn *fillmores-cxns*)
(delete-cxn 'air-cxn *fillmores-cxns*)
;; we're treating "chairman of the committee" and "president of the club"

(def-fcg-cxn was-cxn
             ((?was-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme be)
                          (tense past))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?was-unit
                (HASH meaning ((be.01 ?x ?a) (tense ?a past)))
                --
                (HASH form ((sequence "was" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn was-cxn
             ((?was-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 1st)
                                   (number singular)))
               (morph-cat (lexeme be)
                          (tense past))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?was-unit
                (HASH meaning ((be.01 ?x ?a) (tense ?a past)))
                --
                (HASH form ((sequence "was" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn I-cxn
             ((?I-unit
               (syn-cat (lex-class (cat P)
                                   (max +)
                                   (min +)
                                   (role S))
                        (agreement (number singular)
                                   (person 1st))) ; means it is a pronoun-headed phrase
               (sem-cat (sem-class person))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?I-unit
               (HASH meaning ((1st-pp ?x)))
               --
               (HASH form ((sequence "I" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn chairman-cxn
             ((?chairman-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?chairman-unit
               (HASH meaning ((chairman ?x)))
               --
               (HASH form ((sequence "chairman" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn president-cxn
             ((?president-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?president-unit
               (HASH meaning ((president ?x)))
               --
               (HASH form ((sequence "president" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn committee-cxn
             ((?committee-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person)) ;; because moral person
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?committee-unit
               (HASH meaning ((committee ?x)))
               --
               (HASH form ((sequence "committee" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn club-cxn
             ((?club-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person)) ;; because moral person
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?club-unit
               (HASH meaning ((club ?x)))
               --
               (HASH form ((sequence "club" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn of-cxn
             ((?of-unit
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (args (?y))
               (boundaries (?left ?right)))
              <-
              (?of-unit
               (HASH meaning ((relation-to-a-whole ?y)))
               --
               (HASH form ((sequence "of" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn part-of-cxn
             ((?part-of-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min -))
                        (agreement (number singular)))
               (sem-cat (sem-class unique-role))
               (args (?a))
               (subunits (?person-unit ?of-unit ?whole-unit))
               (boundaries (?person-left ?complement-right)))
              <-
              (?person-unit
               (args (?x))
               (sem-cat (sem-class person))
               --
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (boundaries (?person-left ?person-right)))
              (?of-unit
               (args (?y)) ;; I include the "of" particule into the meaning of the whole, if I don't the comprehension does not work
               --
               (syn-cat (lex-class (cat Prep)
                                   (min +)))
               (boundaries (?of-left ?of-right)))
              (?whole-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat N)
                                   (max +))
                        (agreement (number singular)))
               (boundaries (?complement-left ?complement-right)))
              (?part-of-unit
               (HASH meaning ((have-org-role ?a ?y ?b) (arg1 ?b ?x) (arg2 ?b ?z))) ;;have-org-role in amr
               --
               (HASH form ((sequence " " ?person-right ?of-left)
                           (sequence " " ?of-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn unique-role-cxn
             ((?unique-role-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)))
               (args (?a))
               (subunits (?subject-unit ?copula-unit ?role-unit))
               (boundaries (?subject-left ?role-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (role S))
                        (agreement (number singular)))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (number singular)))
               (morph-cat (lexeme be))
               (boundaries (?copula-left ?copula-right)))
              (?role-unit
               (sem-cat (sem-class unique-role))
               (args (?z))
               --
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min -))
                        (agreement (number singular)))
               (boundaries (?role-left ?role-right)))
              (?unique-role-unit
               (HASH meaning ((person ?a ?y ?p) (?arg0 ?p ?x) (?arg0-of ?p ?z))) ;;adaptation of "have-org-role" in amr
               --
               (HASH form ((sequence " " ?subject-right ?copula-left)
                           (sequence " " ?copula-right ?role-left)))))
             :cxn-inventory *fillmores-cxns*)

(defun display-unique-role (cxn-inventory)
  (add-element '((a :name  "unique-role")))
  (add-element '((h3) "The unique role construction"))
  (add-element '((p) "This construction is made out of three units: a subject unit, a copula unit, and another grammatical construction that we call part-of construction and allows to produce and comprehend official roles that people may embody.")))

(display-unique-role *fillmores-cxns*)

;;(comprehend "chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "chairman of the committee" :cxn-inventory *fillmores-cxns*)
             
;;(comprehend "president of the club" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "president of the club" :cxn-inventory *fillmores-cxns*)

;;(comprehend "I was chairman of the committee" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "I was chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "Joe was chairman of the committee" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "Joe was president of the club" :cxn-inventory *fillmores-cxns*)

;--------------------;
;Fronting to That cxn;
;--------------------;

(def-fcg-cxn a-cxn
             ((?a-unit
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number singular)))
               (sem-cat (definiteness indefinite))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?a-unit
               (HASH meaning ((instance ?x)))
               --
               (HASH form ((sequence "a" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn an-cxn
             ((?an-unit
               (syn-cat (lex-class (role det)
                                   (cat Art))
                        (agreement (number singular)))
               (sem-cat (definiteness indefinite))
               (morph-form (starts-with vowel))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?an-unit
               (HASH meaning ((instance ?x)))
               --
               (HASH form ((sequence "an" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn that-cxn
             ((?that-unit
               (syn-cat (lex-class (cat Conj)
                                   (max -)
                                   (min +))) ;; for conjunction (clause that-clause))
               (args (?y))
               (boundaries (?left-1 ?right-1)))
              <-
              (?that-unit
               (HASH meaning ((relative ?y)))
               --
               (HASH form ((sequence "that" ?left-1 ?right-1)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "that" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn fronting-to-that-cxn
             ((?fronting-to-that-unit
               (syn-cat (lex-class (cat V) ;; checking which head-type is that cxn
                                   (max +)))
               (args (?z))
               (boundaries (?modified-noun-left ?copula-right))
               (subunits (?modified-noun-unit ?that-unit ?subject-unit ?copula-unit)))
              <-              
              (?modified-noun-unit
               (args (?a))
               --
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min -))
                        (agreement (number ?number)))
               (boundaries (?modified-noun-left ?modified-noun-right)))
              (?that-unit
               (args (?b))
               --
               (syn-cat (lex-class (cat Conj)
                                   (min +)))
               (boundaries (?that-left ?that-right)))
              (?subject-unit
               (args (?c))
               --
               (syn-cat (lex-class (max +)
                                   (role S))
                        (agreement (number ?number)))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?d))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (number ?number)))
               (morph-cat (lexeme be))
               (boundaries (?copula-left ?copula-right)))
              (?fronting-to-that-unit
               (HASH meaning ((arg0-that ?z ?a ?b ?c ?d))) ;;modified arg0 to fit the that structure
               --
               (HASH form ((sequence " " ?modified-noun-right ?that-left)
                           (sequence " " ?that-right ?subject-left)
                           (sequence " " ?subject-right ?copula-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-fronting (cxn-inventory)
  (add-element '((a :name  "fronting")))
  (add-element '((h3) "The fronting to that construction"))
  (add-element '((p) "This construction is made out of four units: a modified count noun, a unit filled by the item \"that\", a subject and a copula. All necessary lexical items are loaded into the grammar."))
  (add-element '((p) "We show below the construction."))
  (add-element (make-html (find-cxn 'fronting-to-that-cxn cxn-inventory)))
  (add-element '((p) "We can comprehend the example given by fillmore \"foolish child that I was\".")))

(display-fronting *fillmores-cxns*)

;; (comprehend "foolish child that I was" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "foolish child that I was" :cxn-inventory *fillmores-cxns*)

;;----------------------;;
;; non maximal V-phrase ;;
;;----------------------;;

(def-fcg-cxn from-cxn
             ((?from-unit
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?from-unit
               (HASH meaning ((origin ?x)))
               --
               (HASH form ((sequence "from" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn prep-headed-cxn
             ((?prep-headed-unit
               (syn-cat (lex-class (cat P)
                                   (max +)
                                   (min -)))
               (args (?a))
               (subunits (?prep-unit ?np-unit))
               (boundaries (?prep-left ?np-right))
               (?from-unit
                (dependents (?np-unit))))
              <-
              (?prep-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (boundaries (?prep-left ?prep-right)))
              (?np-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +)))
               (boundaries (?np-left ?np-right)))
              (?prep-headed-unit
               (HASH meaning ((prep ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?prep-right ?np-left)))))
              :cxn-inventory *fillmores-cxns*)

;; (comprehend "from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "from the book" :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "to Joe" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn remove-cxn
             ((?remove-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (tense preterit)
                          (lexeme remove))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?remove-unit
               (HASH meaning ((remove.01 ?x ?y) (tense ?y preterit)))
               --
               (HASH form ((sequence "removed" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removes-cxn
             ((?removes-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (tense present)
                          (lexeme remove))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?removes-unit
               (HASH meaning ((remove.01 ?x ?y) (tense ?y present)))
               --
               (HASH form ((sequence "removes" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn does-cxn
             ((?does-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme do)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?does-unit
               (HASH meaning ((do.01 ?x)))
               --
               (HASH form ((sequence "does" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "does" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn is-cxn
             ((?is-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme be)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?is-unit
                (HASH meaning ((be.01 ?x ?a) (tense ?a present)))
                --
                (HASH form ((sequence "is" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn non-max-remove-phrase-cxn
             ((?non-max-remove-phrase-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)
                                   (number ?number))) ;; non-maximal verb headed phrase, typically missing a subject
               (args (?a ?arg-0))
               (subunits (?verb-unit ?np-unit ?prep-phrase-unit))
               (boundaries (?verb-left ?pp-right)))
              <-
              (?verb-unit
               (args (?x))
               (morph-cat (lexeme remove))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +))
                        (agreement (person ?person)
                                   (number ?number)))
               (boundaries (?verb-left ?verb-right)))
              (?np-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +)))
               (boundaries (?np-left ?np-right)))
              (?prep-phrase-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat P)
                                   (max +)
                                   (min -)))
               (boundaries (?pp-left ?pp-right)))
              (?non-max-remove-phrase-unit
               (HASH meaning ((action ?a ?x ?arg-0 ?arg-1 ?arg-2) (object ?arg-1 ?y) (domain ?arg-2 ?z)))
               --
               (HASH form ((sequence " " ?verb-right ?np-left)
                           (sequence " " ?np-right ?pp-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-non-max-v (cxn-inventory)
  (add-element '((a :name  "non-max-v")))
  (add-element '((h3) "The non-maximal verb phrase construction"))
  (add-element '((p) "A non-maximal verb phrase is a verb phrase lacking an essential element to be considered complete. We exemplify this construction with the verb \"to remove\"."))
  (add-element (make-html (find-cxn 'non-max-remove-phrase-cxn cxn-inventory))))

(display-non-max-v *fillmores-cxns*)

(comprehend-and-formulate "removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "removes the bottle from the book" :cxn-inventory *fillmores-cxns*)

;;------------------------------------;;
;; the subject predicate construction ;;
;;------------------------------------;;

;; INFL describes a functional head containing a finite head-verb (meaning: a verb that shows tense, person or number)
;; ex: she goes, does your brother know my brother?

;; non-finite verb forms don't show tense, person or number. ex: to go, going, gone.

(def-fcg-cxn she-cxn
             ((?she-unit
               (syn-cat (lex-class (cat P)
                                   (max +) ;; means it is a pronoun-headed phrase
                                   (min +)
                                   (role S))
                        (agreement (number singular)
                                   (person 3rd))) ;; role S is an implementation of page 43 of the article where S is for subject and caracterizes anything capable of filling the role "subject"
               (sem-cat (sem-class person))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?she-unit
               (HASH meaning ((3rd-fem-pp ?x)))
               --
               (HASH form ((sequence "she" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn subject-predicate-cxn
             ((?subject-predicate-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (infl tense))
                        (agreement (number ?number)))
               (boundaries (?subject-left ?verb-right))
               (subunits (?subject-unit ?verb-unit))
               (?verb-unit
                (dependents (?subject-unit))))
              <-
              (?subject-unit
               (args (?subject))
               --
               (syn-cat (lex-class (max +)
                                   (role S))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y ?subject))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -)) ;; non-maximal verb headed phrase, missing a subject
                        (agreement (person ?person)))               
               (morph-cat (tense ?tense))
               (boundaries (?verb-left ?verb-right)))
              (?subject-predicate-unit
               (HASH meaning ((topic ?x ?subject)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she removed the bottle from the book" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn be-smth-cxn
             ((?be-smth-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (tense ?tense))
               (args (?a ?subject))
               (boundaries (?verb-left ?np-right))
               (subunits (?verb-unit ?noun-phrase)))
              <-
              (?verb-unit
               (args (?x))
               (morph-cat (lexeme be))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux ?aux))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?verb-left ?verb-right)))
              (?noun-phrase
               (args (?y))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +))
                        (agreement (number ?number)))
               (boundaries (?np-left ?np-right))) 
              (?be-smth-unit
               (HASH meaning ((arg1 ?a ?y ?x ?subject))) ;;and we add the verb to allow comprehension
               --
               (HASH form ((sequence " " ?verb-right ?np-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "was a foolish child" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "was a foolish child" :cxn-inventory *fillmores-cxns*)

;; (comprehend "I was a foolish child" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "I was a foolish child" :cxn-inventory *fillmores-cxns*)

(defun display-subject-pred (cxn-inventory)
  (add-element '((a :name  "subj-pred")))
  (add-element '((h3) "The subject predicate construction"))
  (add-element '((p) "The subject-predicate-construction integrates the notion of infl which describes a functional head containing a finite head-verb (meaning: a verb that shows tense, person or number). As a comparison, non-finite verb forms don't show tense, person or number (ex: to go, going, gone). Thanks to this construction we can comprehend and formulate any sentence that satisfies these verb criterias. We introduce two examples:"))
  (add-element '((p) "1) \"I removed the bottle from the book\" which uses the non-maximal verb phrase with \"to remove\" previously seen."))
  (add-element '((p) "2) An example that constrasts with another previous section: \"I was a foolish child.\""))
  (add-element (make-html (find-cxn 'subject-predicate-cxn cxn-inventory))))

(display-subject-pred *fillmores-cxns*)
;;(comprehend-and-formulate "I removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "I was a foolish child" :cxn-inventory *fillmores-cxns*)

;------------;
;once removed;
;------------;

;; "second cousin once removed": Your second cousin once removed is the child of your second cousin or the parent of your third cousin.
;; you can encounter "first cousin once removed", "third cousine twice removed": "once removed" means a difference of one generation ->
;; in "second cousin once removed": it means for example the child of you second cousin.

;; according to Fillmore, who bases this analysis on Paul kay's construction grammar treatment of complex English kin-terms,
;; there is no sens in describing "once removed" (= "distant in relationship") based on the verb "to remove"

(def-fcg-cxn cousin-cxn
             ((?cousin-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (sem-cat (sem-class person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left-1 ?right-1)))
               <-
               (?cousin-unit
                (HASH meaning ((grand-child-of-same-grand-parents ?x)))
                --
                (HASH form ((sequence "cousin" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removed-cxn
             ((?removed-unit
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (morph-cat (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?removed-unit
               (HASH meaning ((distant-in-relationship ?x)))
               --
               (HASH form ((sequence "removed" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn second-cxn
             ((?second-unit
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (morph-cat (starts-with consonant))
               (sem-cat (sem-class degrees))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?second-unit
               (HASH meaning ((degree-2 ?x)))
               --
               (HASH form ((sequence "second" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn once-cxn
             ((?once-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)))
               (sem-cat (sem-class times))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?once-unit
               (HASH meaning ((times-1 ?x)))
               --
               (HASH form ((sequence "once" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn x-cousin-y-removed-cxn
             ((?x-cousin-y-removed-unit
               (syn-class (lex-class (cat N)
                                     (max -)))
               (args (?b))
               (subunits (?x-unit ?cousin-unit ?y-unit ?removed-unit))
               (boundaries (?x-left ?removed-right)))
              <-
              (?x-unit
               (syn-cat (lex-class (cat A)
                                   (min +)))
               (sem-cat (sem-class degrees))
               (args (?x))
               --
               (syn-cat (lex-class (cat A)
                                   (min +)))
               (boundaries (?x-left ?x-right)))
              (?cousin-unit
               (sem-cat (sem-class person))
               (args (?y))
               --
               (syn-cat (lex-class (cat N)
                                   (max -))
                        (agreement (number singular)))
               (boundaries (?cousin-left ?cousin-right)))
              (?y-unit
               (sem-cat (sem-class times))
               (args (?z))
               --
               (syn-cat (lex-class (cat Adv)
                                   (min +)))
               (boundaries (?y-left ?y-right)))
              (?removed-unit
               (args (?a))
               --
               (syn-cat (lex-class (cat A)
                                   (min +)))
               (boundaries (?removed-left ?removed-right)))
              (?x-cousin-y-removed-unit
               (HASH meaning ((arg1 ?horizontal-step ?x ?y) (domain ?vertical-step ?z ?a) (domain ?b ?horizontal-step ?vertical-step)))
               --
               (HASH form ((sequence " " ?x-right ?cousin-left)
                           (sequence " " ?cousin-right ?y-left)
                           (sequence " " ?y-right ?removed-left)))))
              :cxn-inventory *fillmores-cxns*)

(defun display-cousin-removed (cxn-inventory)
  (add-element '((a :name  "cousin-removed")))
  (add-element '((h3) "The \"once removed construction\""))
  (add-element '((p) "Although this construction isn't drawn by Fillmore using the boxes-within-boxes representation, but only evoqued, we found interesting to implement it. Your second cousin once removed is the child of your second cousin or the parent of your third cousin. We call that construction x-cousin-y-removed because x can be filled with any ordinal number (first, second, etc.) and y can be filled with any multiplicative number (once, twice, etc.). It is interesting to make a construction out of this example because according to Fillmore, who bases this analysis on Paul kay's construction grammar treatment of complex English kin-terms, there is no sense in describing \"removed\" (distant in relationship) based on the verb \"to remove\""))
  (add-element (make-html (find-cxn 'x-cousin-y-removed-cxn cxn-inventory))))

(display-cousin-removed *fillmores-cxns*)

(comprehend-and-formulate "second cousin once removed" :cxn-inventory *fillmores-cxns*)

;-----------;
;  TO GIVE  ;
;-----------;

;; valence = combinatorial properties of lexical items
;; the valence of give is: Arg0 subject agent Noun-headed phrase (GF), Arg1 object patient Noun-headed phrase (SR), Arg2 complement recipient Pronoun-headed phrases (MS)

(def-fcg-cxn gives-cxn
             ((?gives-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme give)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?gives-unit
                (HASH meaning ((give.01 ?x ?a) (tense ?a present))) ;;a giver, a given entity, a gived entity
                --
                (HASH form ((sequence "gives" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn x-y-smth-to-smn-cxn
             ((?x-y-smth-to-smn-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)))
               (morph-cat (tense ?tense))
               (args (?b))
               (subunits (?subject-unit ?verb-unit ?object-unit ?complement-recipient))
               (boundaries (?subject-left ?complement-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (role S))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?subject-left ?subject-right)))              
              (?verb-unit
               (args (?y))
               --
               #|(syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +))
                        (agreement (number ?number)
                                   (person ?person)))|#
               (morph-cat (tense ?tense))
               (boundaries (?verb-left ?verb-right))) ;;which types of verbs can fill this? explain, do, give,...  
              (?object-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +)))
               (boundaries (?object-left ?object-right)))
              (?complement-recipient
               (args (?a))
               --
               (syn-cat (lex-class (cat P)
                                   (max +)
                                   (min -)))
               (boundaries (?complement-left ?complement-right)))
              (?x-y-smth-to-smn-unit
               (HASH meaning ((action ?b ?y ?arg-0 ?arg-1 ?arg-2) (performer ?arg-0 ?x) (object ?arg-1 ?z) (recipient ?arg-2 ?a)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?object-left)
                           (sequence " " ?object-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;;(comprehend "she gives" :cxn-inventory *fillmores-cxns*)

(defun display-give (cxn-inventory)
  (add-element '((a :name  "give")))
  (add-element '((h3) "The to give construction"))
  (add-element '((p) "This construction is made out of four units: the subject unit, the verb unit, the object unit, the complement recipient unit. We describe the meaning of the sentence using the valence of verbs like \"give\". This valence repesents as Fillmores says the \"combinatorial properties of lexical items\"."))
  (add-element (make-html (find-cxn 'x-y-smth-to-smn-cxn cxn-inventory))))

;;(comprehend "she gives a book to Joe" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "she gives a book to Joe" :cxn-inventory *fillmores-cxns*)

;; it's the for "she passes something to Joe", "I do something to Joe"
             
;;-------------------------------;;
;; Inversion of maximal V-phrase ;;
;;-------------------------------;;

;; ex:
;; 1) antecedent of a counterfactual conditional sentence: "were she here", "had I known"
;; 2) interrogation: "was she here", "is it thursday"
;; 3) scope of negation: "never have I lied"

(def-fcg-cxn here-cxn
             ((?here-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (wh -)))
               (sem-cat (sem-class places))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?here-unit
               (HASH meaning ((current-place ?x)))
               --
               (HASH form ((sequence "here" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn has-cxn
             ((?has-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme have)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?has-unit
                (HASH meaning ((have.01 ?x))) ;;have.01 = auxiliary
                --
                (HASH form ((sequence "has" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn have-cxn
             ((?have-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 1st)
                                   (number singular)))
               (morph-cat (lexeme have)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?have-unit
                (HASH meaning ((have.01 ?x) (tense ?x present))) ;;have.01 = auxiliary
                --
                (HASH form ((sequence "have" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn lied-cxn
             ((?lied-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -)))
               (morph-cat (lexeme lie)
                          (tense preterit))
               (args (?y))
               (boundaries (?left ?right)))
               <-
               (?lied-unit
                (HASH meaning ((lie.08 ?y) (tense ?y preterit))) ;;lie.08 = tell a falsehood
                --
                (HASH form ((sequence "lied" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn never-cxn
             ((?never-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH -)))
               (sem-cat (sem-class negation))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?never-unit
               (HASH meaning ((not-been-a-time ?x)))
               --
               (HASH form ((sequence "never" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn inversion-max-V-cxn
             ((?inversion-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (args (?a))
               (boundaries (?aux-left ?complement-right))
               (subunits (?aux-unit ?subject-unit ?complement-unit))
               (?aux-unit
                (dependents (?subject-unit ?complement-unit))))
              <-
              (?aux-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +)))
               (boundaries (?aux-left ?aux-right)))
              (?subject-unit
               (args (?y))
               --
               (syn-cat (lex-class (max +)
                                   (role S)))
               (boundaries (?subject-left ?subject-right)))
              (?complement-unit
               (args (?z))
               --
               (syn-cat ;;(cat ?cat) according to Fillmore, the category satys undefined and takes whatever category is given by the complement, so we don't add anything
                        (lex-class (max -)))
               (boundaries (?complement-left ?complement-right)))
              (?inversion-unit
               (HASH meaning ((action ?a ?x ?arg-0 ?arg-1) (performer ?arg-0 ?y) (performed ?arg-1 ?z)))
               --
               (HASH form ((sequence " " ?aux-right ?subject-left)
                           (sequence " " ?subject-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "were she here" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "have I lied" :cxn-inventory *fillmores-cxns*) ; as in "never have I lied"
;; (comprehend-and-formulate "was she here" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn never-x-cxn
             ((?never-x-unit
               (syn-class (lex-class (cat V)
                                     (max +)
                                     (min -)))
               (args (?x))
               (subunits (?never-unit ?inversion-unit))
               (boundaries (?never-left ?inversion-right)))
              <-
              (?never-unit
               (sem-cat (sem-class negation))
               (args (?x))
               --
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH -)))
               (boundaries (?never-left ?never-right)))
              (?inversion-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (boundaries (?inversion-left ?inversion-right)))
              (?never-x-unit
               (HASH meaning ((never-smth ?x ?y)))
               --
               (HASH form ((sequence " " ?never-right ?inversion-left)))))
             :cxn-inventory *fillmores-cxns*)

(defun display-inversion (cxn-inventory)
  (add-element '((a :name  "inversion")))
  (add-element '((h3) "The inversion of maximal verb phrase construction"))
  (add-element '((p) "An inversion can appear in three cases:"))
  (add-element '((p) "1) As antecedent of a counterfactual conditional sentence: \"were she here\", \"had I known\"."))
  (add-element '((p) "2) Inan interrogation: \"was she here\", \"is it thursday\"."))
  (add-element '((p) "3) scope of negation: \"never have I lied\"")))

(comprehend-and-formulate "were she here" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "have I lied" :cxn-inventory *fillmores-cxns*) ; as in "never have I lied"
(comprehend-and-formulate "was she here" :cxn-inventory *fillmores-cxns*)

(defun display-never ()
  (add-element '((p) "We also examplify how an inversion can be used inside another construction by display the never-x-cxn.")))

(display-never)

(comprehend-and-formulate "never have I lied" :cxn-inventory *fillmores-cxns*)

;;------------;;
;; wh phrases ;;
;;------------;;

(def-fcg-cxn where-cxn
             ((?where-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?where-unit
               (HASH meaning ((what-place ?x)))
               --
               (HASH form ((sequence "where" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn why-cxn
             ((?why-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?why-unit
               (HASH meaning ((reason ?x)))
               --
               (HASH form ((sequence "why" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn wh-phrase-cxn
             ((?wh-phrase-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (WH +)))
               (args (?a))
               (subunits (?wh-unit ?vp-unit))
               (boundaries (?wh-left ?vp-right))
               (?vp-unit
                (dependents (?wh-unit))))
              <-
              (?wh-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH +)))    
               (boundaries (?wh-left ?wh-right)))
              (?vp-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (min -))) ;; max and inv are not specified because they are not necessary
               (boundaries (?vp-left ?vp-right)))
              (?wh-phrase-unit
               (HASH meaning ((wh-smth ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?wh-right ?vp-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-wh-phrase (cxn-inventory)
  (add-element '((a :name  "wh-phrase")))
  (add-element '((h3) "Wh-phrases constructions"))
  (add-element '((p) "We have seen how we can implement an inverted maximal verb phrase. This type of construction can appear in wh-phrases. Wh-phrases start with \"wh\" words (what, which, how, etc.). We instanciate a wh-phrase with the sentences \"Where have I lied\" and \"Why was she here\".")))

(display-wh-phrase *fillmores-cxns*)

;; (comprehend "where have I lied" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "where have I lied" :cxn-inventory *fillmores-cxns*)

;; (comprehend "why was she here" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "why was she here" :cxn-inventory *fillmores-cxns*)

;;---------------;;
;; TO CONTRIBUTE ;;
;;---------------;;

;; the verb "to contribute" can appear in multiple types of cxns:
;; the construction: subject + contribute
;; the construction: subject + contribute + object (thing being contributed) (without the "to" complement because of conversational givenness)
;; the construction: subject + contribute + complement (recipient with "to") (without the object: indefinite interpretation)
;; the construction: subject + contribute + object + complement (with "to")

(def-fcg-cxn to-cxn
             ((?to-unit
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (sem-cat (sem-class destination))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?to-unit
               (HASH meaning ((destination ?x)))
               --
               (HASH form ((sequence "to" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn contributed-cxn
             ((?contributed-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (tense preterit)
                          (lexeme contribute))
               (sem-cat (sem-class contribute))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?contributed-unit
               (HASH meaning ((contribute.01 ?x)))
               --
               (HASH form ((sequence "contributed" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; we exemplify the second one:
(def-fcg-cxn contribute-smth-cxn
             ((?contribute-smth-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)))
               (morph-cat (tense ?tense))
               (args (?a ?arg-0))
               (subunits (?contribute-unit ?object-unit))
               (boundaries (?contribute-left ?object-right)))
              <-
              (?contribute-unit
               (args (?x))
               (sem-cat (sem-class contribute))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (lexeme contribute)
                          (tense ?tense))
               (boundaries (?contribute-left ?contribute-right)))
              (?object-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +)))
               (boundaries (?object-left ?object-right)))
              (?contribute-smth-unit
               (HASH meaning ((action ?a ?x ?arg-0 ?arg-1) (object ?arg-1 ?y)))
               --
               (HASH form ((sequence " " ?contribute-right ?object-left)))))
             :cxn-inventory *fillmores-cxns*)

;; we exemplify the third one:
(def-fcg-cxn contribute-to-cxn
             ((?contribute-to-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)))
               (morph-cat (tense ?tense))
               (args (?a ?arg-0))
               (subunits (?contribute-unit ?to-unit ?complement-unit))
               (boundaries (?contribute-left ?complement-right)))
              <-
              (?contribute-unit
               (args (?x))
               (sem-cat (sem-class contribute))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (lexeme contribute)
                          (tense ?tense))
               (boundaries (?contribute-left ?contribute-right)))
              (?to-unit
               (args (?y))
               (sem-cat (sem-class destination))
               --
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (boundaries (?to-left ?to-right)))
              (?complement-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat N)  
                                   (max +)))
               (boundaries (?complement-left ?complement-right)))
              (?contribute-to-unit
               (HASH meaning ((action ?a ?x ?arg-0 ?arg-1 ?arg-2) (object ?arg-1 ?y) (to ?y ?arg-2 ?z)))
               --
               (HASH form ((sequence " " ?contribute-right ?to-left)
                           (sequence " " ?to-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

(defun display-contribute (cxn-inventory)
  (add-element '((a :name  "contribute")))
  (add-element '((h3) "The to contribute construction"))
  (add-element '((p) "The verb \"to contribute\" can appear in multiple types of cxns:"))
  (add-element '((p) "1) The construction: subject + contribute."))
  (add-element '((p) "2) The construction: subject + contribute + object (thing being contributed) (without the "to" complement because of conversational givenness)"))
  (add-element '((p) "3) The construction: subject + contribute + complement (recipient with \"to\") (without the object: indefinite interpretation)"))
  (add-element '((p) "4) The construction: subject + contribute + object + complement (with \"to\")<i>We exemplify the second and third one."))
  (add-element (make-html (find-cxn 'contribute-to-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'contribute-smth-cxn cxn-inventory)))
  (add-element '((p) "Finally, we comprehend and formulate an instanciation of each construction and we call the subject-predicate-cxn to comprehend and formulate the sentence \"She contributed to this book\".")))

(display-contribute *fillmores-cxns*)

;; (comprehend "contributed a book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "contributed a book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she contributed a book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she contributed a book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "contributed to this book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "contributed to this book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she contributed to this book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "she contributed to this book" :cxn-inventory *fillmores-cxns*)

;;---------------;;
;; worth knowing ;;
;;---------------;;

(def-fcg-cxn knowing-cxn
             ((?knowing-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (lexeme remove)
                          (tense gerund)) ;; for gerundive
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?knowing-unit
               (HASH meaning ((knowing.01 ?x)))
               --
               (HASH form ((sequence "knowing" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "knowing" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn worth-cxn
             ((?worth-unit
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (args (?x))
               (morph-cat (lexeme worth))
               (boundaries (?left ?right)))
              <-
              (?worth-unit
               (HASH meaning ((worth.01 ?x)))
               --
               (HASH form ((sequence "worth" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "worth" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn worth-ing-cxn
             ((?worth-ing-unit
               (syn-cat (lex-class (cat A)
                                   (max +)
                                   (min -)))
               (sem-cat (sem-class worth))
               (args (?a))
               (subunits (?worth-unit ?ing-unit))
               (boundaries (?worth-left ?ing-right)))
              <-
              (?worth-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)))
               (morph-cat (lexeme worth))
               (boundaries (?worth-left ?worth-right)))
              (?ing-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)))
               (morph-cat (tense gerund))
               (boundaries (?ing-left ?ing-right)))
              (?worth-ing-unit
               (HASH meaning ((worth-x ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?worth-right ?ing-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "worth knowing" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "worth knowing" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn be-worth-ing-cxn
             ((?be-worth-ing-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)))
               (morph-cat (tense ?tense))
               (args (?a ?arg-1))
               (subunits (?verb-unit ?worth-unit))
               (boundaries (?verb-left ?worth-right)))
              <-
              (?verb-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person ?person)))
               (morph-cat (lexeme be)
                          (tense ?tense))
               (boundaries (?verb-left ?verb-right)))
              (?worth-unit
               (args (?y))
               (sem-cat (sem-class worth))
               --
               (syn-cat (lex-class (cat A)
                                   (max +)
                                   (min -)))
               (boundaries (?worth-left ?worth-right)))
              (?be-worth-ing-unit
               (HASH meaning ((copula ?a ?x ?arg-1 ?arg-2) (comment ?arg-2 ?y)))
               --
               (HASH form ((sequences " " ?verb-right ?worth-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-worth (cxn-inventory)
  (add-element '((a :name  "worth")))
  (add-element '((h3) "The to be worth of construction"))
  (add-element '((p) "be-worth-x is a construction that calls for the construction worth-x and a copula. We show it below and instantiate it."))
  (add-element (make-html (find-cxn 'be-worth-ing-cxn cxn-inventory)))
  (add-element '((p) "Let's consider that it is a non-maximal phrase. It needs a subject. We instantiate a maximal verb phrase made out of this construction.")))

(display-worth *fillmores-cxns*)

;; (comprehend "is worth knowing" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "is worth knowing" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she is worth knowing" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "she is worth knowing" :cxn-inventory *fillmores-cxns*)

;;---------------;;
;; DO ONE'S BEST ;;
;;---------------;;

(def-fcg-cxn her-cxn
             ((?her-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +)
                                   (role det)
                                   (subcat pronoun))
                        (agreement (number ?number)
                                   (person 3rd)))
               (sem-cat (sem-class person))
               (morph-cat (morph poss))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?her-unit
                (HASH meaning ((3rd-poss-fem ?x)))
                --
                (HASH form ((sequence "her" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn best-cxn
             ((?best-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +)))
               (morph-cat (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?best-unit
               (HASH meaning ((best ?x)))
               --
               (HASH form ((sequence "best" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn ones-best-cxn
             ((?ones-best-unit
              (syn-cat (lex-class (cat N)
                                  (max +))
                       (agreement (person ?person)))
              (args (?a))
              (subunits (?possessive-unit ?best-unit))
              (boundaries (?possessive-left ?best-right)))
             <-
             (?possessive-unit
              (args (?x))
              --
              (syn-cat (lex-class (cat N)
                                  (max -)
                                  (min +)
                                  (role det)
                                  (subcat pronoun))
                       (agreement (person ?person)))
              (morph-cat (morph poss))
              (boundaries (?possessive-left ?possessive-right)))
             (?best-unit
              (args (?y))
              --
              (syn-cat (lex-class (cat N)
                                  (max -)
                                  (min +)))
              (boundaries (?best-left ?best-right)))
             (?ones-best-unit
              (HASH meaning ((domain ?a ?y ?x)))
              --
              (HASH form ((sequence " " ?possessive-right ?best-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "her best" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn does-cxn
             ((?does-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme do)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?does-unit
               (HASH meaning ((do.01 ?x)))
               --
               (HASH form ((sequence "does" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn do-ones-best-cxn
             ((?do-ones-best-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (number ?number)
                                   (person ?person)))
               (morph-cat (tense ?tense))
               (args (?x ?arg-0))
               (subunits (?do-unit ?ones-best-unit))
               (boundaries (?do-left ?ones-best-right)))
              <-
              (?do-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (min +))
                        (agreement (number ?number)
                                   (person ?person)))
               (morph-cat (lexeme do)
                          (tense ?tense))
               (boundaries (?do-left ?do-right)))
              (?ones-best-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat N)
                                   (max +))
                        (agreement (person ?person)))
               (boundaries (?ones-best-left ?ones-best-right)))
              (?do-ones-best-unit
               (HASH meaning ((action ?x ?arg-0 ?arg-1) (object ?arg-1 ?y)))
               --
               (HASH form ((sequence " " ?do-right ?ones-best-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-do-ones-best (cxn-inventory)
  (add-element '((a :name  "do-ones-best")))
  (add-element '((h3) "The do one's best construction"))
  (add-element '((p) "do-ones-best is a construction that calls for the construction ones-best and a form of the verb \"to do\". We show it below and instantiate it."))
  (add-element (make-html (find-cxn 'do-ones-best-cxn cxn-inventory)))
  (add-element '((p) "Let's consider that it is a non-maximal phrase. It needs a subject. We instantiate a maximal verb phrase made out of this construction.")))

(display-do-ones-best *fillmores-cxns*)

;; (comprehend "does her best" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "does her best" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she does her best" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "she does her best" :cxn-inventory *fillmores-cxns*)

;; "I did my best"
;; "she did her best"

;;---------------------------------;;
;; The correlative conditional cxn ;;
;;---------------------------------;;

;; this cxns aims to map onto any sentence that combines the + comparative + complement + the + comparative + complement
;; ex: "The sooner you learn how to pronounce her name, the more likely is she to go out with you"

;; we'll exemplify the first example given by Fillmore.

(def-fcg-constructions fillmores-grammar
  :cxn-inventory *fillmores-cxns*
  :hashed t
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (sequences set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (dependents sequence)
                  (args sequence)
                  (footprints set)
                  (boundaries sequence))
  :fcg-configurations ((:node-tests :restrict-nr-of-nodes)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:max-number-of-nodes . 10000)
                       ;; goal tests for comprehension
                       (:parse-goal-tests
                        :no-sequence-in-root
                        ;; :no-applicable-cxns ;; succeeds if no more cxns can apply
                        :connected-semantic-network ;; succeeds if the semantic network is fully connected
                        :connected-structure) ;; succeeds if all units are connected
                       ;; goal tests for formulation
                       (:production-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :no-meaning-in-root)) ;; succeeds if no meaning predicates remain in root
  :hierarchy-features (subunits dependents))

(def-fcg-cxn pronounce-cxn
             ((?pronounce-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -)))
               (morph-cat (tense inf)
                          (lexeme pronounce))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?pronounce-unit
               (HASH meaning ((pronounce.01 ?x ?y) (tense ?y inf)))
               --
               (HASH form ((sequence "to pronounce" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn learn-cxn
             ((?learn-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person 2nd)
                                   (number singular)))
               (morph-cat (tense present)
                          (lexeme learn))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?learn-unit
               (HASH meaning ((learn.01 ?x ?y) (tense ?y present)))
               --
               (HASH form ((sequence "learn" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn how-cxn
             ((?how-unit
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?how-unit
               (HASH meaning ((manner ?x)))
               --
               (HASH form ((sequence "how" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn you-cxn
             ((?you-unit
               (syn-cat (lex-class (cat P)
                                   (max -) ;; means it is a pronoun-headed phrase
                                   (min +)
                                   (role S))
                        (agreement (number ?number)
                                   (person 2nd))) ;; role S is an implementation of page 43 of the article where S is for subject and caracterizes anything capable of filling the role "subject"
               (sem-cat (sem-class person))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?you-unit
               (HASH meaning ((2nd-pp ?x)))
               --
               (HASH form ((sequence "you" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn name-cxn
             ((?name-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +))
                        (agreement (number singular)))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left-1 ?right-1)))
               <-
               (?name-unit
                (HASH meaning ((name ?x)))
                --
                (HASH form ((sequence "name" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)


(def-fcg-cxn more-cxn
             ((?more-unit
               (syn-cat (lex-class (max +)
                                   (min +)
                                   (MS comp)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?more-unit
               (HASH meaning ((comp-sup ?x)))
               --
               (HASH form ((sequence "more" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn longer-cxn
             ((?longer-unit
               (syn-cat (lex-class (max +)
                                   (min +)
                                   (MS comp)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?longer-unit
               (HASH meaning ((longer ?x)))
               --
               (HASH form ((sequence "longer" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn sooner-cxn
             ((?sooner-unit
               (syn-cat (lex-class (max +)
                                   (min +)
                                   (MS comp)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?sooner-unit
               (HASH meaning ((sooner ?x)))
               --
               (HASH form ((sequence "sooner" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "sooner" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn go-out-cxn
             ((?go-out-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -)))
               (morph-cat (tense inf)
                          (lexeme go))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?go-out-unit
               (HASH meaning ((go_out.34 ?x ?y) (tense ?y inf)))
               --
               (HASH form ((sequence "to go out" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "to go out" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn likely-cxn
             ((?likely-unit
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)
                                   (WH -)))
               (morph-cat (starts-with consonant))
               (sem-cat (sem-class probability))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?likely-unit
               (HASH meaning ((likely ?x)))
               --
               (HASH form ((sequence "likely" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn with-cxn
             ((?with-unit
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (args (?y))
               (boundaries (?left ?right)))
              <-
              (?with-unit
               (HASH meaning ((with ?y)))
               --
               (HASH form ((sequence "with" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn her-cxn
             ((?her-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +)
                                   (role det)
                                   (subcat pronoun))
                        (agreement (number ?number)
                                   (person 3rd)))
               (sem-cat (sem-class person))
               (morph-cat (morph poss))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?her-unit
                (HASH meaning ((3rd-poss-fem ?x)))
                --
                (HASH form ((sequence "her" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn wh-phrase-cxn
             ((?wh-phrase-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (WH +)))
               (args (?a))
               (subunits (?wh-unit ?vp-unit))
               (boundaries (?wh-left ?vp-right))
               (?vp-unit
                (dependents (?wh-unit))))
              <-
              (?wh-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat Adv)
                                   (max -)
                                   (min +)
                                   (WH +)))    
               (boundaries (?wh-left ?wh-right)))
              (?vp-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (min -))) ;; max and inv are not specified because they are not necessary
               (boundaries (?vp-left ?vp-right)))
              (?wh-phrase-unit
               (HASH meaning ((wh-smth ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?wh-right ?vp-left)))))
  :cxn-inventory *fillmores-cxns*)

;;-----------;;
;; Posession ;;
;;-----------;;

(def-fcg-cxn posession-cxn
             ((?posession-unit
              (syn-cat (lex-class (cat N)
                                  (max +)
                                  (min -)))
              (args (?a))
              (subunits (?possessive-unit ?np-unit))
              (boundaries (?possessive-left ?np-right)))
             <-
             (?possessive-unit
              (args (?x))
              --
              (syn-cat (lex-class (cat N)
                                  (max -)
                                  (min +)
                                  (role det)
                                  (subcat pronoun)))
              (morph-cat (morph poss))
              (boundaries (?possessive-left ?possessive-right)))
             (?np-unit
              (args (?y))
              --
              (syn-cat (lex-class (cat N)
                                  (max -)
                                  (min +)))
              (boundaries (?np-left ?np-right)))
             (?posession-unit
              (HASH meaning ((domain ?a ?y ?x)))
              --
              (HASH form ((sequence " " ?possessive-right ?np-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "her name" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn infinitive-one-arg-phrase-cxn
             ((?infinitive-one-arg-phrase-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -)))
               (args (?a))
               (subunits (?verb-unit ?np-unit))
               (boundaries (?verb-left ?np-right))
               (?verb-unit
                (dependents (?np-unit))))
              <-
              (?verb-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)))
               (morph-cat (tense inf))
               (boundaries (?verb-left ?verb-right)))
              (?np-unit
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (min -)))
               (boundaries (?np-left ?np-right)))
              (?infinitive-one-arg-phrase-unit
               (HASH meaning ((action ?a ?x) (arg-1 ?x)))
               --
               (HASH form ((sequence " " ?verb-right ?np-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "to pronounce her name" :cxn-inventory *fillmores-cxns*)

;; (comprehend "how to pronounce her name" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "how to pronounce her name" :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "to go out with you" :cxn-inventory *fillmores-cxns*)

;; this cxns aims to map onto any sentence that combines the + comparative + complement + the + comparative + complement
;; ex: "The sooner you learn how to pronounce her name, the more likely is she to go out with you"

;; we'll exemplify the first example given by Fillmore.

(def-fcg-cxn that-comp-cxn
             ((?that-comp-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)))
               (morph-form (marker that)
                          (negpol -))
               (args (?a))
               (subunits (?that-unit ?subject-unit ?verb-unit ?complement-unit))
               (boundaries (?that-left ?complement-right)))
              <-
              (?that-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat Conj)
                                   (max -)
                                   (min +)))
               (boundaries (?that-left ?that-right)))
              (?subject-unit
               (args (?y))
               --
               (syn-cat (lex-class (role S))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat V)
                                   (max -))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (tense ?tense))
               (boundaries (?verb-left ?verb-right)))
              (?complement-unit
               (args (?b))
               --
               (syn-cat (lex-class (max -)))
               (boundaries (?complement-left ?complement-right)))
              (?that-comp-unit
               (HASH meaning ((that-x ?a ?x ?y ?z ?b)))
               --
               (HASH form ((sequence " " ?that-right ?subject-left)
                           (sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "that you stay here" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn no-that-comp-cxn
             ((?no-that-comp-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)))
               (morph-form (marker none)
                           (negpol -))
               (args (?a))
               (subunits (?subject-unit ?verb-unit ?complement-unit))
               (boundaries (?subject-left ?complement-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (lex-class (role S))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +))
                        (agreement (person ?person)
                                   (number ?number)))
               (boundaries (?verb-left ?verb-right)))
              (?complement-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (WH +)))
               (boundaries (?complement-left ?complement-right)))
              (?no-that-comp-unit
               (HASH meaning ((to-be-filled ?a ?x ?y ?z)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn comp-antecedent-cxn
             ((?comp-antecedent-unit
               (syn-cat (lex-class (max -)
                                   (min -)))
               (morph-form (marker ?marker)
                           (negpol ?negpol))
               (args (?a))
               (subunits (?comparative-unit ?complement-unit))
               (boundaries (?comparative-left ?complement-right))
               (?complement-unit
                (dependents (?comparative-unit))))
              <-
              (?comparative-unit
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (min +)
                                   (MS comp)))
               (boundaries (?comparative-left ?comparative-right)))
              (?complement-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)))
               (morph-form (marker ?marker)
                           (negpol ?negpol))
               (boundaries (?complement-left ?complement-right)))
              (?comp-antecedent-unit
               (HASH meaning ((arg3-comp ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?comparative-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "more that you stay here" :cxn-inventory *fillmores-cxns*)

;; (comprehend "sooner you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "sooner you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn the-cxn
             ((?the-unit
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number ?number)))
               (sem-cat (definiteness definite))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?the-unit
               (HASH meaning ((unique ?x)))
               --
               (HASH form ((sequence "the" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)
  
(def-fcg-cxn antecedent-cxn
             ((?antecedent-unit
               (syn-cat (lex-class (role antecedent)))
               (args (?a))
               (subunits (?the-unit ?comparation-unit))
               (boundaries (?the-left ?comparation-right)))
              <-
              (?the-unit
               (args (?x))
               --
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number ?number)))
               (boundaries (?the-left ?the-right)))
              (?comparation-unit
               (args (?y))
               --
               (syn-cat (lex-class (max -)
                                   (min -)))
               (morph-form (marker ?marker)
                           (negpol ?negpol))
               (boundaries (?comparation-left ?comparation-right)))
              (?antecedent-unit
               (HASH meaning ((antecedent-arg1 ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?the-right ?comparation-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "the longer that you stay here" :cxn-inventory *fillmores-cxns*)

;; (comprehend "the sooner you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "the sooner you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn is-cxn
             ((?is-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person 3rd)
                                   (number singular)))
               (morph-cat (lexeme be)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?is-unit
                (HASH meaning ((be.01 ?x ?a) (tense ?a present)))
                --
                (HASH form ((sequence "is" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn she-cxn
             ((?she-unit
               (syn-cat (lex-class (cat P)
                                   (max +) ;; means it is a pronoun-headed phrase
                                   (min +)
                                   (role S))
                        (agreement (number singular)
                                   (person 3rd))) ;; role S is an implementation of page 43 of the article where S is for subject and caracterizes anything capable of filling the role "subject"
               (sem-cat (sem-class person))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?she-unit
               (HASH meaning ((3rd-fem-pp ?x)))
               --
               (HASH form ((sequence "she" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

#|(def-fcg-cxn you-cxn
             ((?you-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +)
                                   (subcat pronoun))
                        (agreement (number ?number)
                                   (person 2nd)))
               (sem-cat (sem-class person))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?you-unit
                (HASH meaning ((2nd-pp-o ?x))) ;;2nd personal pronoun object
                --
                (HASH form ((sequence "you" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)|#

(def-fcg-cxn to-go-out-with-cxn
    ((?to-go-out-with-unit
      (syn-cat (lex-class (cat V)
                          (max -)
                          (min -)
                          (inv -)
                          (infl none)))
      (args (?a))
      (subunits (?go-out-unit ?with-unit ?second-dater-unit))
      (boundaries (?go-out-left ?second-dater-right)))
     <-
     (?go-out-unit
      (args (?x))
      --
      (morph-cat (tense inf)
                 (lexeme go))
      (boundaries (?go-out-left ?go-out-right)))
     (?with-unit
      (args (?y))
      --
      (syn-cat (lex-class (cat Prep)
                          (max -)
                          (min +)))
      (boundaries (?with-left ?with-right)))
     (?second-dater-unit
      (args (?z))
      --
      (syn-cat (lex-class (max -)
                          (min +)))
      (sem-cat (sem-class person))
      (boundaries (?second-dater-left ?second-dater-right)))
     (?to-go-out-with-unit
      (HASH meaning ((go-out-with ?a ?x ?y)(arg1-dater ?y ?z)))
      --
      (HASH form ((sequence " " ?go-out-right ?with-left)
                  (sequence " " ?with-right ?second-dater-left)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "to go out with you" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn inversion-max-V-cxn
             ((?inversion-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (args (?a))
               (boundaries (?aux-left ?complement-right))
               (subunits (?aux-unit ?subject-unit ?complement-unit))
               (?aux-unit
                (dependents (?subject-unit ?complement-unit))))
              <-
              (?aux-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +)))
               (boundaries (?aux-left ?aux-right)))
              (?subject-unit
               (args (?y))
               --
               (syn-cat (lex-class (max +)
                                   (role S)))
               (boundaries (?subject-left ?subject-right)))
              (?complement-unit
               (args (?z))
               --
               (syn-cat (lex-class (cat V)
                          (max -)
                          (min -)
                          (inv -)
                          (infl none)))
               (boundaries (?complement-left ?complement-right)))
              (?inversion-unit
               (HASH meaning ((action ?a ?x ?arg-0 ?arg-1) (performer ?arg-0 ?y) (performed ?arg-1 ?z)))
               --
               (HASH form ((sequence " " ?aux-right ?subject-left)
                           (sequence " " ?subject-right ?complement-left)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "is she to go out with you" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn likely-to-cxn
             ((?likely-to-unit
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (args (?a))
               (subunits (?likely-unit ?inv-unit))
               (boundaries (?likely-left ?inv-right)))
              <-
              (?likely-unit
               (args (?x))
               (sem-cat (sem-class probability))
               --
               (syn-cat (lex-class (cat A)
                                   (max -)
                                   (min +)
                                   (WH -)))
               (morph-cat (starts-with consonant))
               (boundaries (?likely-left ?likely-right)))
              (?inv-unit
               (args (?y))
               --
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (boundaries (?inv-left ?inv-right)))
              (?likely-to-unit
               (HASH meaning ((arg2 ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?likely-right ?inv-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "likely is she to go out with you" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn comp-consequent-cxn
             ((?comp-unit
               (syn-cat (lex-class (max -)
                                   (min -)
                                   (inv +)))
               (args (?a))
               (subunits (?comparative-unit ?complement-unit))
               (boundaries (?comparative-left ?complement-right))
               (?complement-unit
                (dependents (?comparative-unit))))
              <-
              (?comparative-unit
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (min +)
                                   (MS comp)))
               (boundaries (?comparative-left ?comparative-right)))
              (?complement-unit
               (args (?Y))
               --
               (syn-cat (lex-class (cat V)
                                   (max +)
                                   (min -)
                                   (inv +)
                                   (infl tense)))
               (boundaries (?complement-left ?complement-right)))
              (?comp-unit
               (HASH meaning ((have-degree-91 ?a ?x ?y) (arg3 ?x) (arg2 ?y)))
               --
               (HASH form ((sequence " " ?comparative-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn consequence-cxn
             ((?consequence-unit
               (syn-cat (lex-class (role consequent)))
               (args (?a))
               (subunits (?the-unit ?comparation-unit))
               (boundaries (?the-left ?comparation-right)))
              <-
              (?the-unit
               (args (?x))
               --
               (syn-cat (lex-class (role det)
                                   (cat Art))
                        (agreement (number ?number)))
               (boundaries (?the-left ?the-right)))
              (?comparation-unit
               (args (?y))
               --
               (syn-cat (lex-class (max -)
                                   (min -)
                                   (inv +)))
               (boundaries (?comparation-left ?comparation-right)))
              (?consequence-unit
               (HASH meaning ((consequent-arg2 ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?the-right ?comparation-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "the more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "the more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn correlative-conditional-cxn
             ((?correlative-conditional-cxn-unit
              (syn-cat (syn-class (max +)
                                  (min -)))
              (args (?a))
              (subunits (?antecedent-unit ?consequent-unit))
              (boundaries (?antecedent-left ?consequent-right)))
             <-
             (?antecedent-unit
              (args (?x))
              --
              (syn-cat (lex-class (role antecedent)))
              (boundaries (?antecedent-left ?antecedent-right)))
             (?consequent-unit
              (args (?y))
              --
              (syn-cat (lex-class (role consequent)))
              (boundaries (?consequent-left ?consequent-right)))
             (?correlative-conditional-cxn-unit
              (HASH meaning ((correlate-91 ?a ?x ?y)))
              --
              (HASH form ((sequence " " ?antecedent-right ?consequent-left)))))
  :cxn-inventory *fillmores-cxns*)

(defun display-correlative (cxn-inventory)
  (add-element '((a :name  "correlative")))
  (add-element '((h3) "The correlative conditional construction"))
  (add-element '((p) "We are implementing the correlative condition construction as described by Fillmore. To do this we need multiple constructions, some that will fit into the others. As Fillmore didn't necessarily give them names, we came up with some original names. For the user to be able to dive into the different constructions of this section without ambiguity, we clear up the construction inventory from the previously stored constructions. Here are the necessary constructions of this section:"
                 ((i) "- The the-cxn.")
                 ((i) "-The antecedance-cxn (ex: \"The sooner you learn how to pronounce her name\")")
                 ((i) "- The consequence-cxn (ex: \"the more likely is he to go out with you\")")
                 ((i) "- The possession-cxn (ex: \"her name\")")
                 ((i) "- The infinitive-one-arg-phrase-cxn (ex: \"to pronounce her name\")")
                 ((i) "- The that-comp-cxn in case we want to instantiate a antecedent with a \"that\" item (ex: \"\")")
                 ((i) "- The no-that-comp-cxn (ex: \"you learn how to pronounce her name\")")
                 ((i) "- The comp-cxn (ex: \"sooner you learn how to pronounce her name\")")
                 ((i) "- The likely-to-cxn (ex: \"likely is she to go out\")")
                 ((i) "- The comp-consequence-cxn (ex: \"more likely is he to go out with you\")")
                 ((i) "We also fill our grammar with constructions for necessary lexical items such as sooner, more or name."))))

(display-correlative *fillmores-cxns*)

(comprehend-and-formulate "the sooner you learn how to pronounce her name" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "the more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)

;;(comprehend "the sooner you learn how to pronounce her name the more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)

(comprehend-and-formulate "the sooner you learn how to pronounce her name the more likely is she to go out with you" :cxn-inventory *fillmores-cxns*)
