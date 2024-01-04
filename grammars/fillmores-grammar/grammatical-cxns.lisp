(in-package :fcg)

;-----------------;
;determination cxn;
;-----------------;
;; determination cxn : a determiner and a non-maximal nominal head (max +)

;; the determination cxn is made of a determinor and a mass noun / a singular count noun

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
               (args (?y))
               --
               (syn-cat (lex-class (cat N)
                                   (max -))
                        (agreement (number ?number)))
               (morph-form (starts-with ?starts-with))
               (boundaries (?noun-left ?noun-right)))
              (?determination-unit
               (HASH meaning ((determining ?z ?x ?y)))
               --
               (HASH form ((sequence " " ?det-right ?noun-left)))))
             :cxn-inventory *fillmores-cxns*)

;; mass nouns exhibit the same use that count nouns in the determination-cxn
;; but mass noun can also be used as proper noun, which as maximal phrase (which is why their maximilality is left undefined)

#|;; correct:
(comprehend-and-formulate "my pen" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "my air" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "this air" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "my bottle" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "my rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "this intelligence" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "this rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "the book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "this pen" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "a book" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "an intelligence" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "an air" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "the club" :cxn-inventory *fillmores-cxns*)

(comprehend-and-formulate "a foolish child" :cxn-inventory *fillmores-cxns*)

;; incorrect:
(comprehend-and-formulate "an rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "these rice" :cxn-inventory *fillmores-cxns*)
(comprehend-and-formulate "an pen" :cxn-inventory *fillmores-cxns*) |#

;-----------------------------------;
; Unique Role Nominal Predicate Cxn ;
;-----------------------------------;

;; we're treating only "chairman of the committee" and "president of the club"

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
               (args (?y))
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
               (HASH meaning ((part-of ?a ?x ?y ?z)))
               --
               (HASH form ((sequence " " ?person-right ?of-left)
                           (sequence " " ?of-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;;(comprehend "chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "chairman of the committee" :cxn-inventory *fillmores-cxns*)
             
;;(comprehend "president of the club" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "president of the club" :cxn-inventory *fillmores-cxns*)

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
               (HASH meaning ((copula ?a ?y ?arg-1 ?arg-2) (topic ?arg-1 ?x) (role ?arg-2 ?z))) ;;adaptation of "have-rel-role" in amr
               --
               (HASH form ((sequence " " ?subject-right ?copula-left)
                           (sequence " " ?copula-right ?role-left)))))
             :cxn-inventory *fillmores-cxns*)

;;(comprehend "I was chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "I was chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "Joe was chairman of the committee" :cxn-inventory *fillmores-cxns*)
;;(comprehend-and-formulate "I was president of the club" :cxn-inventory *fillmores-cxns*)

;--------------------;
;Fronting to That cxn;
;--------------------;

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
               (HASH meaning ((domain ?z ?y ?x))) ;; = the domain (from amr) of the adjective (?y) is the noun (?x)
               --
               (HASH form ((sequence " " ?adj-right ?noun-left)))))
              :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "foolish child" :cxn-inventory *fillmores-cxns*)

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
               (HASH meaning ((copula ?z ?d ?arg-1 ?arg-2) (topic ?arg-1 ?c) (comment ?complement ?a) (fronting ?arg-2 ?b ?complement))) ;;but focus on non-maximal headed-phrase
               --
               (HASH form ((sequence " " ?modified-noun-right ?that-left)
                           (sequence " " ?that-right ?subject-left)
                           (sequence " " ?subject-right ?copula-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "foolish child that I was" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "foolish child that I was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn be-smth-cxn
             ((?be-smth-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)))
               (args (?a ?arg-1))
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
               (HASH meaning ((copula ?a ?x ?arg-1 ?arg-2) (comment ?arg-2 ?y)))
               --
               (HASH form ((sequence " " ?verb-right ?np-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "was a foolish child" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "was a foolish child" :cxn-inventory *fillmores-cxns*)

;; (comprehend "I was a foolish child" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "I was a foolish child" :cxn-inventory *fillmores-cxns*)

;;------------------------------------;;
;; the subject predicate construction ;;
;;------------------------------------;;

;; INFL describes a functional head containing a finite head-verb (meaning: a verb that shows tense, person or number)
;; ex: she goes, does your brother know my brother?

;; non-finite verb forms don't show tense, person or number. ex: to go, going, gone.

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
               (args (?x))
               --
               (syn-cat (lex-class (max +)
                                   (role S))
                        (agreement (number ?number)
                                   (person ?person)))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y ?arg-1))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -)) ;; non-maximal verb headed phrase, missing a subject
                        (agreement (person ?person)))               
               (morph-cat (tense ?tense))
               (boundaries (?verb-left ?verb-right)))
              (?subject-predicate-unit
               (HASH meaning ((topic ?x ?arg-1)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "I removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "I removed the bottle from the book" :cxn-inventory *fillmores-cxns*)

;------------;
;once removed;
;------------;

;; "second cousin once removed": Your second cousin once removed is the child of your second cousin or the parent of your third cousin.
;; you can encounter "first cousin once removed", "third cousine twice removed": "once removed" means a difference of one generation ->
;; in "second cousin once removed": it means for example the child of you second cousin.

;; according to Fillmore, who bases this analysis on Paul kay's construction grammar treatment of complex English kin-terms,
;; there is no sens in describing "once removed" (= "distant in relationship") based on the verb "to remove"

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
               (HASH meaning ((domain ?horizontal-step ?x ?y) (domain ?vertical-step ?z ?a) (domain ?b ?horizontal-step ?vertical-step)))
               --
               (HASH form ((sequence " " ?x-right ?cousin-left)
                           (sequence " " ?cousin-right ?y-left)
                           (sequence " " ?y-right ?removed-left)))))
              :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "second cousin once removed" :cxn-inventory *fillmores-cxns*)

;-----------;
;  TO GIVE  ;
;-----------;

;; valence = combinatorial properties of lexical items
;; the valence of give is: Arg0 subject agent Noun-headed phrase (GF), Arg1 object patient Noun-headed phrase (SR), Arg2 complement recipient Pronoun-headed phrases (MS)

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

;; (comprehend-and-formulate "never have I lied" :cxn-inventory *fillmores-cxns*)


;;----------------------;;
;; non maximal V-phrase ;;
;;----------------------;;

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

(def-fcg-cxn non-max-remove-phrase-cxn
             ((?non-max-remove-phrase-unit
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min -))
                        (agreement (person ?person)
                                   (number ?number))) ;; non-maximal verb headed phrase, typically missing a subject
               (args (?a ?arg-0))
               (subunits (?verb-unit ?np-unit ?prep-phrase-unit))
               (boundaries (?verb-left ?pp-right))
               (?verb-unit
                (dependents (?np-unit ?prep-phrase-unit))))
              <-
              (?verb-unit
               (args (?x))
               --
               (syn-cat (lex-class (cat V)
                                   (max -)
                                   (min +))
                        (agreement (person ?person)))
               (morph-cat (lexeme remove))
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

;; (comprehend-and-formulate "removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "removes the bottle from the book" :cxn-inventory *fillmores-cxns*)


;;---------------;;
;; TO CONTRIBUTE ;;
;;---------------;;

;; the verb "to contribute" can appear in multiple types of cxns:
;; the construction: subject + contribute
;; the construction: subject + contribute + object (thing being contributed) (without the "to" complement because of conversational givenness)
;; the construction: subject + contribute + complement (recipient with "to") (without the object: indefinite interpretation)
;; the construction: subject + contribute + object + complement (with "to")

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

;; (comprehend "contributed a book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "contributed a book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she contributed a book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she contributed a book" :cxn-inventory *fillmores-cxns*)

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

;; (comprehend "contributed to this book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "contributed to this book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she contributed to this book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she contributed to this book" :cxn-inventory *fillmores-cxns*)

;;------------;;
;; wh phrases ;;
;;------------;;

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

;; (comprehend "where have I lied" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "where have I lied" :cxn-inventory *fillmores-cxns*)

;; (comprehend "why was she here" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "why was she here" :cxn-inventory *fillmores-cxns*)

;;---------------;;
;; worth knowing ;;
;;---------------;;

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

;; (comprehend "is worth knowing" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "is worth knowing" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she is worth knowing" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she is worth knowing" :cxn-inventory *fillmores-cxns*)

;;---------------;;
;; DO ONE'S BEST ;;
;;---------------;;

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

;; (comprehend "does her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "does her best" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she does her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she does her best" :cxn-inventory *fillmores-cxns*)

;; "I did my best"
;; "she did her best"

;;---------------------------------;;
;; The correlative conditional cxn ;;
;;---------------------------------;;

;; this cxns aims to map onto any sentence that combines the + comparative + complement + the + comparative + complement
;; ex: "The sooner you learn how to pronounce her name, the more likely is she to go out with you"

;; we'll exemplify the first example given by Fillmore.

;; more-cxn
;; sooner-cxn
;; you-cxn
;; learn-cxn
;; how-cxn

#|(def-fcg-cxn antecedent-cxn
             ((?antecedent-unit)
              <-
              (?the-unit)
              
              (?antecedent-unit))
(def-fcg-cxn consequence-cxn)

(def-fcg-cxn correlative-conditional-cxn
             (?correlative-conditional-cxn-unit
              (syn-cat (syn-class (max +)
                                  (min -)))
              (args (?x))
              (subunits (?antecedent-unit ?consequent-unit))
              (boundaries (?antecedent-left ?consequent-right)))
             <-
             (?antecedent-unit
              --
              (boundaries (?antecedent-left ?antecedent-right)))
             (?consequent-unit
              --
              (boundaries (?consequent-left ?consequent-right)))
             (?correlative-conditional-cxn-unit
              (HASH meaning (()))
              --
              (HASH form ((sequence " " ?antecedent-right ?consequent-left))))
|#
              