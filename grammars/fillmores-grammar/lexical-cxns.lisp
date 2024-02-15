(in-package :fillmores-grammar)

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
      (cat N)
      (max +) ;; the external syntax of Joe is that of a maximal phrase
      (min +)
      (role S);; because "joe" is a lexical item
      (agreement (number singular))
      (sem-cat (sem-class person))
      (morph-form (starts-with consonant))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?joe-unit
      (HASH meaning ((person ?p) (name ?n) (:name ?p ?n) (:op1 ?n "Joe")))
      --
      (HASH form ((sequence "Joe" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "Joe" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn I-cxn
    ((?I-unit
      (cat P)
      (max +)
      (min +)
      (role S)
      (agreement (number singular)
                 (person 1st)) ; means it is a pronoun-headed phrase
      (sem-cat (sem-class person))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?I-unit
      (HASH meaning ((I ?i)))
      --
      (HASH form ((sequence "I" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn she-cxn
    ((?she-unit
      (cat P)
      (max +) ;; means it is a pronoun-headed phrase
      (min +)
      (role S)
      (agreement (number singular)
                 (person 3rd)) ;; role S is an implementation of page 43 of the article where S is for subject and caracterizes anything capable of filling the role "subject"
      (sem-cat (sem-class person))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?she-unit
      (HASH meaning ((she ?s)))
      --
      (HASH form ((sequence "she" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "she" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn her-cxn
    ((?her-unit
      (cat N)
      (max -)
      (min +)
      (role det)
      (subcat pronoun)
      (agreement (number ?number)
                 (person 3rd))
      (sem-cat (sem-class person))
      (morph-cat (morph poss))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?her-unit
      (HASH meaning ((she ?s)))
      --
      (HASH form ((sequence "her" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (cat N) (max  ) = mass noun = any noun that represents something impossible or difficult to count
;; maximality in mass nouns are left unspecified

(def-fcg-cxn air-cxn
    ((?air-unit
      (cat N)
      (max ?undefined) ;; (max  ) becomes (max ?undefined) because if not the feature is not recognized in FCG
      (min +)
      (agreement (number singular))
      (morph-form (starts-with vowel))
      (args (?a))
      (boundaries (?left ?right)))
     <-
     (?air-unit
      (HASH meaning ((air ?a)))
      --
      (HASH form ((sequence "air" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "intelligence" :cxn-inventory *fillmores-cxns*)

;; (cat N) (max -) = count noun = any noun that represents something that we can count

(def-fcg-cxn book-cxn
    ((?book-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (morph-form (starts-with consonant))
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?book-unit
      (HASH meaning ((book ?b)))
      --
      (HASH form ((sequence "book" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn bottle-cxn
    ((?bottle-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (morph-form (starts-with consonant))
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?bottle-unit
      (HASH meaning ((bottle ?b)))
      --
      (HASH form ((sequence "bottle" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "bottle" :cxn-inventory *fillmores-cxns*)

;; article : a, an, the
;; possessive nominal : my, your, his, her, etc.
;; demonstrative : this, that, these, those
;; article, possessive nominal, and demonstrative have the role of "determiners"

(def-fcg-cxn the-cxn
    ((?the-unit
      (role det)
      (cat Art) ;;for article (added)
      (agreement (number ?number))
      (sem-cat (definiteness definite))
      (morph-form (starts-with ?starts-with))
      (args (?t))
      (boundaries (?left ?right)))
     <-
     (?the-unit
      (HASH meaning ((unique ?t)))
      --
      (HASH form ((sequence "the" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn a-cxn
    ((?a-unit
      (role det)
      (cat Art) ;;for article
      (agreement (number singular))
      (sem-cat (definiteness indefinite))
      (morph-form (starts-with consonant))
      (args (?a))
      (boundaries (?left ?right)))
     <-
     (?a-unit
      (HASH meaning ((instance ?a)))
      --
      (HASH form ((sequence "a" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn an-cxn
    ((?an-unit
      (role det)
      (cat Art)
      (agreement (number singular))
      (sem-cat (definiteness indefinite))
      (morph-form (starts-with vowel))
      (args (?a))
      (boundaries (?left ?right)))
     <-
     (?an-unit
      (HASH meaning ((instance ?a)))
      --
      (HASH form ((sequence "an" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn my-cxn
    ((?my-unit
      (role det)
      (cat PN) ;; possessive nominal
      (agreement (number ?number))
      (sem-cat (definiteness ?definiteness))
      (morph-form (starts-with ?starts-with))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?my-unit
      (HASH meaning ((I ?i) (:poss ?x ?i)))
      --
      (HASH form ((sequence "my" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn this-cxn
    ((?this-unit
      (role det)
      (cat Dem) ;;for demonstrative
      (agreement (number singular))
      (sem-cat (definiteness definite))
      (morph-form (starts-with ?starts-with))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?this-unit
      (HASH meaning ((this ?t) (:mod ?x ?t)))
      --
      (HASH form ((sequence "this" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn these-cxn
    ((?these-unit
      (role det)
      (cat Dem)
      (agreement (number plural))
      (sem-cat (definiteness definite))
      (morph-form (starts-with ?starts-with))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?these-unit
      (HASH meaning ((these ?t) (:mod ?x ?t)))
      --
      (HASH form ((sequence "these" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;;(comprehend-and-formulate "a" :cxn-inventory *fillmores-cxns*)

;; adding a mechanism that transforms "this" into "these" in plural and "that" into "those"
;; adding a mechanism that transforms "a" and "an" when the count noun is used in plural (no need for "the")

(def-fcg-cxn chairman-cxn
    ((?chairman-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person))
      (morph-form (starts-with consonant))
      (args (?c))
      (boundaries (?left ?right)))
     <-
     (?chairman-unit
      (HASH meaning ((chairman ?c)))
      --
      (HASH form ((sequence "chairman" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn president-cxn
    ((?president-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person))
      (morph-form (starts-with consonant))
      (args (?p))
      (boundaries (?left ?right)))
     <-
     (?president-unit
      (HASH meaning ((president ?p)))
      --
      (HASH form ((sequence "president" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn committee-cxn
    ((?committee-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person)) ;; because moral person
      (morph-form (starts-with consonant))
      (args (?c))
      (boundaries (?left ?right)))
     <-
     (?committee-unit
      (HASH meaning ((committee ?c)))
      --
      (HASH form ((sequence "committee" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn club-cxn
    ((?club-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person)) ;; because moral person
      (morph-form (starts-with consonant))
      (args (?c))
      (boundaries (?left ?right)))
     <-
     (?club-unit
      (HASH meaning ((club ?c)))
      --
      (HASH form ((sequence "club" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn child-cxn
    ((?child-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person))
      (morph-form (starts-with consonant))
      (args (?c))
      (boundaries (?left-1 ?right-1)))
     <-
     (?child-unit
      (HASH meaning ((child ?c)))
      --
      (HASH form ((sequence "child" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "child" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn cousin-cxn
    ((?cousin-unit
      (cat N)
      (max -)
      (min +)
      (agreement (number singular))
      (sem-cat (sem-class person))
      (morph-form (starts-with consonant))
      (args (?c))
      (boundaries (?left-1 ?right-1)))
     <-
     (?cousin-unit
      (HASH meaning ((cousin ?c)))
      --
      (HASH form ((sequence "cousin" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn best-cxn
    ((?best-unit
      (cat N)
      (max -)
      (min +)
      (morph-cat (starts-with consonant))
      (args (?h))
      (boundaries (?left ?right)))
     <-
     (?best-unit
      (HASH meaning ((have-degree-91 ?h) (good.03 ?g) (most ?m) (:arg2 ?h ?g) (:arg3 ?h ?m)))
      --
      (HASH form ((sequence "best" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn foolish-cxn
    ((?foolish-unit
      (cat A)
      (max -)
      (min +)
      (morph-form (starts-with consonant))
      (args (?f))
      (boundaries (?left-1 ?right-1)))
     <-
     (?foolish-unit
      (HASH meaning ((foolish.01 ?f)))
      --
      (HASH form ((sequence "foolish" ?left-1 ?right-1)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "foolish" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removed-cxn
    ((?removed-unit
      (cat A)
      (max -)
      (min +)
      (morph-cat (starts-with consonant))
      (args (?r))
      (boundaries (?left ?right)))
     <-
     (?removed-unit
      (HASH meaning ((removed ?r)))
      --
      (HASH form ((sequence "removed" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

#|(def-fcg-cxn that-cxn
((?that-unit
(cat Conj)
(max -)
(min +)) ;; for conjunction (clause that-clause))
(args (?t))
               (boundaries (?left-1 ?right-1)))
              <-
              (?that-unit
               (HASH meaning ((that ?t)))
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
               (HASH meaning ((to ?t)))
               --
               (HASH form ((sequence "to" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*)|#

(def-fcg-cxn goes-cxn
    ((?goes-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person 3rd)                       
                 (number singular))
      (morph-cat (lexeme go)
                 (tense present))
      (args (?g))
      (boundaries (?left ?right)))
     <-
     (?goes-unit
      (HASH meaning ((go.01 ?g)))
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

(def-fcg-cxn were-cxn
    ((?were-unit
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person ?person)
                 (number ?number))
      (morph-cat (lexeme be)
                 (tense subj)) ;;for subjunctive
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?were-unit
      (HASH meaning ((be.02 ?b))) ;;be.02 has existential meaning as "there is no parking space"
      --
      (HASH form ((sequence "were" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn have-cxn
    ((?have-unit
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person 1st)
                 (number singular))
      (morph-cat (lexeme have)
                 (tense present))
      (args (?h))
      (boundaries (?left ?right)))
     <-
     (?have-unit
      (HASH meaning ((have.01 ?h))) ;;have.01 = auxiliary
      --
      (HASH form ((sequence "have" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn lied-cxn
    ((?lied-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (morph-cat (lexeme lie)
                 (tense preterit))
      (args (?l))
      (boundaries (?left ?right)))
     <-
     (?lied-unit
      (HASH meaning ((lie.08 ?l))) ;;lie.08 = tell a falsehood
      --
      (HASH form ((sequence "lied" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removed2-cxn
    ((?removed-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person ?person)
                 (number ?number))
      (morph-cat (tense preterit)
                 (lexeme remove))
      (args (?r))
      (boundaries (?left ?right)))
     <-
     (?removed-unit
      (HASH meaning ((remove.01 ?r))) ;;(HASH meaning ((remove.01 ?x ?y) (tense ?y preterit)))
      --
      (HASH form ((sequence "removed" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn removes-cxn
    ((?removes-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person 3rd)
                 (number singular))
      (morph-cat (tense present)
                 (lexeme remove))
      (args (?r))
      (boundaries (?left ?right)))
     <-
     (?removes-unit
      (HASH meaning ((remove.01 ?r))) ;;(HASH meaning ((remove.01 ?x ?y) (tense ?y present)))
      --
      (HASH form ((sequence "removes" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn does-cxn
    ((?does-unit
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person 3rd)
                 (number singular))
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
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person 3rd)
                 (number singular))
      (morph-cat (lexeme be)
                 (tense present))
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?is-unit
      (HASH meaning ((be.01 ?b))) ;; (HASH meaning ((be.01 ?x ?a) (tense ?a present)))
      --
      (HASH form ((sequence "is" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn was-cxn
    ((?was-unit
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person 3rd)
                 (number singular))
      (morph-cat (lexeme be)
                 (tense past))
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?was-unit
      (HASH meaning ((be.01 ?b))) ;; (HASH meaning ((be.01 ?x ?a) (tense ?a past)))
      --
      (HASH form ((sequence "was" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn gives-cxn
    ((?gives-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person 3rd)
                 (number singular))
      (morph-cat (lexeme give)
                 (tense present))
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?gives-unit
      (HASH meaning ((give.01 ?x))) ;; (HASH meaning ((give.01 ?x ?a) (tense ?a present)))
      --
      (HASH form ((sequence "gives" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn was-cxn
    ((?was-unit
      (cat V)
      (max -)
      (min +)
      (aux +)
      (agreement (person 1st)
                 (number singular))
      (morph-cat (lexeme be)
                 (tense past))
      (args (?b))
      (boundaries (?left ?right)))
     <-
     (?was-unit
      (HASH meaning ((be.01 ?b))) ;;(HASH meaning ((be.01 ?x ?a) (tense ?a past)))
      --
      (HASH form ((sequence "was" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend "was" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn contributed-cxn
    ((?contributed-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person ?person)
                 (number ?number))
      (morph-cat (tense preterit)
                 (lexeme contribute))
      (sem-cat (sem-class contribute))
      (args (?c))
      (boundaries (?left ?right)))
     <-
     (?contributed-unit
      (HASH meaning ((contribute.01 ?c)))
      --
      (HASH form ((sequence "contributed" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn knowing-cxn
    ((?knowing-unit
      (cat V)
      (max -)
      (min +)
      (aux -)
      (agreement (person ?person)
                 (number ?number))
      (morph-cat (lexeme remove)
                 (tense gerund)) ;; for gerundive
      (args (?k))
      (boundaries (?left ?right)))
     <-
     (?knowing-unit
      (HASH meaning ((know.01 ?k)))
      --
      (HASH form ((sequence "knowing" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "knowing" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn worth-cxn
    ((?worth-unit
      (cat A)
      (max -)
      (min +)
      (args (?w))
      (morph-cat (lexeme worth))
      (boundaries (?left ?right)))
     <-
     (?worth-unit
      (HASH meaning ((worth.02 ?w)))
      --
      (HASH form ((sequence "worth" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "worth" :cxn-inventory *fillmores-cxns*)

#|(def-fcg-cxn of-cxn
((?of-unit
(cat Prep)
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
               (cat Prep)
                                   (max -)
                                   (min +)))
               (args (?x))
               (boundaries (?left ?right)))
              <-
              (?from-unit
               (HASH meaning ((origin ?x)))
               --
               (HASH form ((sequence "from" ?left ?right)))))
             :cxn-inventory *fillmores-cxns*) |#

(def-fcg-cxn never-cxn
    ((?never-unit
      (cat Adv)
      (max -)
      (min +)
      (WH -)
      (sem-cat (sem-class negation))
      (args (?l))
      (boundaries (?left ?right)))
     <-
     (?never-unit
      (HASH meaning ((ever ?e) (:polarity ?l –) (:time ?l ?e)))
      --
      (HASH form ((sequence "never" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn here-cxn
    ((?here-unit
      (cat Adv)
      (max -)
      (min +)
      (wh -)
      (sem-cat (sem-class places))
      (args (?h))
      (boundaries (?left ?right)))
     <-
     (?here-unit
      (HASH meaning ((here ?h)))
      --
      (HASH form ((sequence "here" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn where-cxn
    ((?where-unit
      (cat Adv)
      (max -)
      (min +)
      (WH +)
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?where-unit
      (HASH meaning ((amr-unknown ?a) (:location ?x ?a)))
      --
      (HASH form ((sequence "where" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn why-cxn
    ((?why-unit
      (cat Adv)
      (max -)
      (min +)
      (WH +)
      (args (?x))
      (boundaries (?left ?right)))
     <-
     (?why-unit
      (HASH meaning ((amr-unknown ?a) (:cause ?x ?a)))
      --
      (HASH form ((sequence "why" ?left ?right)))))
  :cxn-inventory *fillmores-cxns*)
