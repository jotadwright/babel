(in-package :fcg)

(defvar *fillmores-cxns*)

;-----------------;
; maximal phrases ;
;-----------------;

;; proper nouns can be considered as maximal phrases, in consequence, they have a maximality +
;; they also can be considered as words, thus they have minimality +

;; maximal phrases: (max +) and (min -)
;; minimal phrases: (max -) and (min -) 
;; lexical items: (max -) (min +)
;; lexical items that are also maximal phrases: (max +) (min +)

(def-fcg-cxn joe-cxn
             ((?joe-unit
               (syn-cat (cat N)
                        (max +) ;; because the external syntax of Joe is that of a maximal phrase
                        (min +)
                        ;;(role S);; because "joe" is a lexical item
                        (number singular))
               (sem-cat (cat person))
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
               (syn-cat (cat N)
                        (max ?undefined) ;; (max  ) becomes (max ?undefined) because if not the feature is not recognized in FCG
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max ?undefined)
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max ?undefined)
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max ?undefined)
                        (number plural))
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
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
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
               (syn-cat (cat N)
                        (max ?undefined) ;; when they are made plural, count nouns are "de-marked" to reuse Fillmore's expression
                        (min +)
                        (number plural))
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
               (syn-cat (role det)
                        (cat Art) ;;for article
                        (number ?number)
                        (definiteness definite))
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
               (syn-cat (role det)
                        (cat Art) ;;for article
                        (number singular)
                        (definiteness indefinite))
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
               (syn-cat (role det)
                        (cat Art)
                        (number singular)
                        (definiteness indefinite))
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
               (syn-cat (role det)
                        (cat PN) ;; possessive nominal
                        (number ?number)
                        (definiteness ?definiteness))
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
               (syn-cat (role det)
                        (cat Dem) ;;for demonstrative
                        (number singular)
                        (definiteness definite))
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
               (syn-cat (role det)
                        (cat Dem)
                        (number plural)
                        (definiteness definite))
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

(def-fcg-cxn I-cxn
             ((?I-unit
               (syn-cat (cat P)
                        (max +)
                        (min +)
                        (role S)
                        (number singular)) ; means it is a pronoun-headed phrase
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
               (syn-cat (cat P)
                        (max +) ;; means it is a pronoun-headed phrase
                        (min +)
                        (role S)
                        (number singular)) ;; role S is an implementation of page 43 of the article where S is for subject and
                                  ;; caracterizes anything capable of filling the role "subject"
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?she-unit
               (HASH meaning ((3rd-fem-pp ?x)))
               --
               (HASH form ((sequence "she" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn was-cxn
             ((?was-unit
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number singular))
               (args (?x ?y))
               (lexeme be)
               (sequences ((sequence "was" ?left ?right)))
               (boundaries (?left ?right)))
               <-
               (?was-unit
                (HASH meaning ((be.01 ?x ?y)))
                --
                (HASH form ((sequence "was" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "was" :cxn-inventory *fillmores-cxns*)
;; (formulate '((be.01 ?x)) :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn chairman-cxn
             ((?chairman-unit
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
               (sem-cat (cat person))
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
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
               (sem-cat (cat person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?president-unit
               (HASH meaning ((president ?x)))
               --
               (HASH form ((sequence "president" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn of-cxn
             ((?of-unit
               (syn-cat (cat Prep)
                        (min +))
               (args (?y))
               (boundaries (?left ?right)))
              <-
              (?of-unit
               (HASH meaning ((relation-to-a-whole ?y)))
               --
               (HASH form ((sequence "of" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn committee-cxn
             ((?committee-unit
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
               (sem-cat (cat person)) ;; because moral person
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
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
               (sem-cat (cat person)) ;; because moral person
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left ?right)))
               <-
              (?club-unit
               (HASH meaning ((club ?x)))
               --
               (HASH form ((sequence "club" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn I-cxn
             ((?I-unit
               (syn-cat (cat P)
                        (max +)
                        (role S)
                        (number singular)) ; means it is a pronoun-headed phrase
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
               (syn-cat (cat P)
                        (max +) ;; means it is a pronoun-headed phrase
                        (min +)
                        (role S)
                        (number singular)) ;; role S is an implementation of page 43 of the article where S is for subject and
                                  ;; caracterizes anything capable of filling the role "subject"
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?she-unit
               (HASH meaning ((3rd-fem-pp ?x)))
               --
               (HASH form ((sequence "she" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn was-cxn
             ((?was-unit
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number singular))
               (args (?x ?y))
               (lexeme be)
               (sequences ((sequence "was" ?left ?right)))
               (boundaries (?left ?right)))
               <-
               (?was-unit
                (HASH meaning ((be.01 ?x ?y)))
                --
                (HASH form ((sequence "was" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "was" :cxn-inventory *fillmores-cxns*)
;; (formulate '((be.01 ?x)) :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn foolish-cxn
             ((?foolish-unit
               (syn-cat (cat A)
                        (min +)
                        (number ?number))
               (morph-form (starts-with consonant))
               (args (?x))
               (sequences ((sequence "foolish" ?left-1 ?right-1))))
              <-
              (?foolish-unit
               (HASH meaning ((foolish ?x)))
               --
               (HASH form ((sequence "foolish" ?left-1 ?right-1)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "foolish" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn child-cxn
             ((?child-unit
               (syn-cat (cat N)
                        (max -)
                        (number singular))
               (sem-cat (cat person))
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

(def-fcg-cxn that-cxn
             ((?that-unit
               (syn-cat (clause that-clause))
               (args (?y))
               (sequences ((sequence "that" ?left-1 ?right-1))))
              <-
              (?that-unit
               (HASH meaning ((relative ?y)))
               --
               (HASH form ((sequence "that" ?left-1 ?right-1)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "that" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn goes-cxn
             ((?goes-unit
               (syn-cat (cat V)
                        (min +)
                        (aux -)
                        (number singular))
               (args (?x ?y))
               (sequences ((sequence "goes" ?left ?right))))
               <-
               (?goes-unit
                (HASH meaning ((go.01 ?x ?y)))
                --
                (HASH form ((sequence "goes" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn remove-cxn
             ((?remove-unit
               (syn-cat (cat V)
                        (min +))
               (lexeme remove)
               (sequences (sequence "remove" ?left ?right)))
              <-
              (?remove-unit
               (HASH meaning ((remove.01 ?x)))
               --
               (HASH form ((sequence "remove" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removed-cxn
             ((?removed-unit
               (syn-cat (cat Adj)
                        (min +))
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
               (syn-cat (cat A)
                        (min +)
                        (number singular)
                        (starts-with consonant))
               (sem-cat (sem-class degrees))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?second-unit
               (HASH meaning ((degree-2 ?x)))
               --
               (HASH form ((sequence "second" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn cousin-cxn
             ((?cousin-unit
               (syn-cat (cat N)
                        (max -)
                        (number singular))
               (sem-cat (cat person))
               (morph-form (starts-with consonant))
               (args (?x))
               (boundaries (?left-1 ?right-1)))
               <-
               (?cousin-unit
                (HASH meaning ((grand-child-of-same-grand-parents ?x)))
                --
                (HASH form ((sequence "cousin" ?left-1 ?right-1)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn once-cxn
             ((?once-unit
               (syn-cat (cat Adv)
                        (min +))
               (sem-cat (sem-class times))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?once-unit
               (HASH meaning ((times-1 ?x)))
               --
               (HASH form ((sequence "once" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn give-cxn
             ((?give-unit
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (number singular))
               (morph-cat (lexeme GIVE)
                        (tense Present)
                        (person 3rd))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?give-unit
               (HASH meaning ((give.01 ?x))) ;;a giver, a given entity, a gived entity
               --
               (HASH form ((sequence "gives" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn to-cxn
             ((?to-unit
               (syn-cat (cat Prep)
                        (max -)
                        (min +))
               (sem-cat (cat destination))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?to-unit
               (HASH meaning ((destination ?x)))
               --
               (HASH form ((sequence "to" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn were-cxn
             ((?were-unit
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +))
               (args (?x))
               (lexeme be)
               (boundaries (?left ?right)))
               <-
               (?were-unit
                (HASH meaning ((be.02 ?x))) ;;be.02 has existential meaning as "there is no parking space"
                --
                (HASH form ((sequence "were" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn here-cxn
             ((?here-unit
               (syn-cat (cat Adv)
                        (max -)
                        (min +))
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
               (syn-cat (cat V)
                        (min +)
                        (aux +))
               (args (?x ?y))
               (lexeme have)
               (sequences ((sequence "has" ?left ?right))))
               <-
               (?has-unit
                (HASH meaning ((has.01 ?x ?y))) ;;have.01 = auxiliary
                --
                (HASH form ((sequence "has" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn have-cxn
             ((?have-unit
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +))
               (args (?x))
               (lexeme have)
               (boundaries (?left ?right)))
               <-
               (?have-unit
                (HASH meaning ((has.01 ?x))) ;;have.01 = auxiliary
                --
                (HASH form ((sequence "have" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn lied-cxn
             ((?lied-unit
               (syn-cat (cat V)
                        (min +))
               (args (?y))
               (lexeme lie)
               (boundaries (?left ?right)))
               <-
               (?lied-unit
                (HASH meaning ((lie.08 ?y))) ;;lie.08 = tell a falsehood
                --
                (HASH form ((sequence "lied" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn remove-cxn
             ((?remove-unit
               (syn-cat (cat V)
                        (max -)
                        (min +))
               (lexeme remove)
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?remove-unit
               (HASH meaning ((remove.01 ?x)))
               --
               (HASH form ((sequence "removed" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn from-cxn
             ((?from-unit
               (syn-cat (cat Prep)
                        (max -)
                        (min +))
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
               (syn-cat (cat Adv)
                        (min +))
               (sem-cat (sem-class negation))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?never-unit
               (HASH meaning ((not-been-a-time ?x)))
               --
               (HASH form ((sequence "never" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)
