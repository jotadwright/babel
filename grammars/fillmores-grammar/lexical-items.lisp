(in-package :fcg)

(defvar *fillmores-cxns*)

;-----------------;
; maximal phrases ;
;-----------------;

;; proper nouns can be considered as maximal phrases, in consequence, they have a maximality +
;; they also can be considered as words, thus they have minimality +

;; maximal phrases: (max +) and (min -)
;; minimal phrases: (max -) and (min -) 
;; lexical items that cannot be used alone (non maximal phrases): (max -) (min +)
;; lexical items that are also maximal phrases: (max +) (min +)

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

(def-fcg-cxn I-cxn
             ((?I-unit
               (syn-cat (lex-class (cat P)
                                   (max +)
                                   (min +)
                                   (role S))
                        (agreement (number singular))) ; means it is a pronoun-headed phrase
               (morph-cat (person 1st))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?I-unit
               (HASH meaning ((1st-pp ?x)))
               --
               (HASH form ((sequence "I" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn she-cxn
             ((?she-unit
               (syn-cat (lex-class (cat P)
                                   (max +) ;; means it is a pronoun-headed phrase
                                   (min +)
                                   (role S))
                        (agreement (number singular)
                                   (person 3rd))) ;; role S is an implementation of page 43 of the article where S is for subject and caracterizes anything capable of filling the role "subject"
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?she-unit
               (HASH meaning ((3rd-fem-pp ?x)))
               --
               (HASH form ((sequence "she" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn her-cxn
             ((?her-unit
               (syn-cat (lex-class (cat N)
                                   (max -)
                                   (min +)
                                   (role det)
                                   (subcat pronoun))
                        (agreement (number ?number)
                                   (person 3rd)))
               (morph-cat (morph poss))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?her-unit
                (HASH meaning ((3rd-poss-p ?x)))
                --
                (HASH form ((sequence "her" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

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

;; (comprehend-and-formulate "bottle" :cxn-inventory *fillmores-cxns*)

;; article : a, an, the
;; possessive nominal : my, your, his, her, etc.
;; demonstrative : this, that, these, those
;; article, possessive nominal, and demonstrative have the role of "determiners"

(def-fcg-cxn the-cxn
             ((?the-unit
               (syn-cat (lex-class (role det)
                                   (cat Art)) ;;for article
                        (agreement (number ?number)))
               (sem-cat (definiteness definite))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               (sequences ((sequence "the" ?left ?right))))
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
               (sequences ((sequence "a" ?left ?right))))
              <-
              (?a-unit
               (HASH meaning ((instance-of ?x)))
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
               (sequences ((sequence "an" ?left ?right))))
              <-
              (?an-unit
               (HASH meaning ((instance-of ?x)))
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
               (sequences ((sequence "my" ?left ?right))))
              <-
              (?my-unit
               (HASH meaning ((possessive-nominal-first-pers ?x)))
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
               (sequences ((sequence "this" ?left ?right))))
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
               (sequences ((sequence "these" ?left ?right))))
              <-
              (?this-unit
               (HASH meaning ((demonstrative-close-objects ?x)))
               --
               (HASH form ((sequence "these" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;;(comprehend-and-formulate "a" :cxn-inventory *fillmores-cxns*)

;; adding a mechanism that transforms "this" into "these" in plural and "that" into "those"
;; adding a mechanism that transforms "a" and "an" when the count noun is used in plural (no need for "the")

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

;; (comprehend-and-formulate "child" :cxn-inventory *fillmores-cxns*)

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

;; (comprehend-and-formulate "foolish" :cxn-inventory *fillmores-cxns*)

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

(def-fcg-cxn goes-cxn
             ((?goes-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person 3rd)                       
                                   (number singular)))
               (morph-cat (lexeme go)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?goes-unit
                (HASH meaning ((go.01 ?x)))
                --
                (HASH form ((sequence "goes" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

#|(def-fcg-cxn remove-cxn
             ((?remove-unit
               (syn-cat (cat V)
                        (min +))
               (morph-cat (lexeme remove)
                          (tense present)) ;; maybe a matrix here
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?remove-unit
               (HASH meaning ((remove.01 ?x)))
               --
               (HASH form ((sequence "remove" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)|#

(def-fcg-cxn gives-cxn
             ((?give-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person 3rd)
                                   (number singular))
               (morph-cat (lexeme give)
                          (tense present))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?give-unit
               (HASH meaning ((give.01 ?x))) ;;a giver, a given entity, a gived entity
               --
               (HASH form ((sequence "gives" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn were-cxn
             ((?were-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux +))
                        (agreement (person ?person)
                                   (number ?number)))
               (morph-cat (lexeme be)
                          (tense subj)) ;;for subjunctive
               (args (?x))
               (boundaries (?left ?right)))
               <-
               (?were-unit
                (HASH meaning ((be.02 ?x))) ;;be.02 has existential meaning as "there is no parking space"
                --
                (HASH form ((sequence "were" ?left ?right)))))
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
                (HASH meaning ((have.01 ?x))) ;;have.01 = auxiliary
                --
                (HASH form ((sequence "have" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn lied-cxn
             ((?lied-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +)
                                   (aux -))
                        (agreement (person ?person)
                                   (number ?number))
               (morph-cat (lexeme lie)
                          (tense preterit))
               (args (?y))
               (boundaries (?left ?right)))
               <-
               (?lied-unit
                (HASH meaning ((lie.08 ?y))) ;;lie.08 = tell a falsehood
                --
                (HASH form ((sequence "lied" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

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
               (HASH meaning ((remove.01 ?x)))
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
               (HASH meaning ((remove.01 ?x)))
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
                (HASH meaning ((be.01 ?x)))
                --
                (HASH form ((sequence "is" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

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
                (HASH meaning ((be.01 ?x)))
                --
                (HASH form ((sequence "was" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "was" :cxn-inventory *fillmores-cxns*)
;; (formulate '((be.01 ?x)) :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "was" :cxn-inventory *fillmores-cxns*)

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

(def-fcg-cxn from-cxn
             ((?from-unit
               (syn-cat (lex-class (cat Prep)
                                   (max -)
                                   (min +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?from-unit
               (HASH meaning ((from ?x)))
               --
               (HASH form ((sequence "from" ?left ?right)))))
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