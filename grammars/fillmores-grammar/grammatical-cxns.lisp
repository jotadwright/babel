(in-package :fcg)
(activate-monitor trace-fcg)

;-----------------;
;determination cxn;
;-----------------;
;; determination cxn : a determiner and a non-maximal nominal head (max +)

;; the determination cxn is made of a determinor and a mass noun / a singular count noun

(def-fcg-cxn determination-cxn
             ((?determination-unit
               (syn-cat (cat N)  
                        (max +)
                        (min -)
                        (number ?number))
               (args (?z))
               (subunits (?det-unit ?noun-unit))
               (boundaries (?det-left ?noun-right)))
              <-
              (?det-unit
               (syn-cat (role det)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (args (?x))
               --
               (syn-cat (role det)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (sequences ((sequence ?det-string ?det-left ?det-right))))
              (?noun-unit
               (args (?y))
               --
               (syn-cat (cat N)
                        (max -)
                        (min ?min)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (boundaries (?noun-left ?noun-right)))
              (?determination-unit
               (HASH meaning ((determining-x ?z ?x ?y)))
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
(comprehend-and-formulate "the air" :cxn-inventory *fillmores-cxns*)
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

(def-fcg-cxn person-of-whole-cxn
             ((?person-of-whole-unit
               (syn-cat (cat N)
                        (max -)
                        (min -)
                        (number singular))
               (sem-cat (cat unique-role))
               (args (?a))
               (subunits (?person-unit ?of-unit ?whole-unit))
               (boundaries (?person-left ?complement-right)))
              <-
              (?person-unit
               (args (?x))
               (sem-cat (cat person))
               --
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number singular))
               (boundaries (?person-left ?person-right)))
              (?of-unit
               (args (?y))
               --
               (syn-cat (cat Prep)
                        (min +))
               (boundaries (?of-left ?of-right)))
              (?whole-unit
               (args (?z))
               --
               (syn-cat (cat N)
                        (max +)
                        (min ?min)
                        (number singular))
               (boundaries (?complement-left ?complement-right)))
              (?person-of-whole-unit
               (HASH meaning ((person-of-whole ?a ?x ?y ?z)))
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
               (syn-cat (cat V)
                        (max +)
                        (min -))
               ;;(args (?a))
               (subunits (?subject-unit ?copula-unit ?role-unit))
               (boundaries (?subject-left ?role-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (cat ?cat)
                        (max +)
                        (role S)
                        (number singular))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?x ?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number singular))
               (boundaries (?copula-left ?copula-right)))
              (?role-unit
               (sem-cat (cat unique-role))
               (args (?y))
               --
               (syn-cat (cat N)
                        (max -)
                        (min -)
                        (number singular))
               (boundaries (?role-left ?role-right)))
              (?unique-role-unit
               ;;(HASH meaning ((x-unique-role ?a ?x ?y ?z)))
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
               (syn-cat (cat N)
                        (max -)
                        (min -)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (args (?z))
               (subunits (?adj-unit ?noun-unit))
               (boundaries (?adj-left ?noun-right)))
              <-
              (?adj-unit
               (args (?y))
               --
               (syn-cat (cat A)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (sequences ((sequence ?adj-string ?adj-left ?adj-right))))
              (?noun-unit
               (args (?x))
               --
               (syn-cat (cat N)
                        (number ?number))
               (sequences ((sequence ?noun-string ?noun-left ?noun-right))))
              (?modified-count-noun-unit
               (HASH meaning ((domain ?z ?y ?x))) ;; = the domain of the adjective (?y) is the noun (?x)
               --
               (HASH form ((sequence " " ?adj-right ?noun-left)))))
              :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "foolish child" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn fronting-to-that-cxn
             ((?fronting-to-that-unit
               (syn-cat (cat V) ;; checking which head-type is that cxn
                        (max +))
               ;;(args (?z ?a ?b))
               (boundaries (?modified-noun-left ?copula-right))
               (subunits (?modified-noun-unit ?that-unit ?subject-unit ?copula-unit)))
              <-              
              (?modified-noun-unit
               (args (?a))
               --
               (syn-cat (cat N)
                        (max -)
                        (min -)
                        (number ?number))
               (boundaries (?modified-noun-left ?modified-noun-right)))
              (?that-unit
               (args (?b))
               --
               (syn-cat (clause that-clause))
               (sequences ((sequence ?that-string ?that-left ?that-right))))
              (?subject-unit
               (args (?c))
               --
               (syn-cat (cat ?cat)
                        (max +)
                        (role S)
                        (number ?number))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?c ?d))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number ?number))
               (boundaries (?copula-left ?copula-right)))
              (?fronting-to-that-unit
               (HASH meaning ((focus-on-non-maximal-nominal-phrase ?a ?b ?d)))
               --
               (HASH form ((sequence " " ?modified-noun-right ?that-left)
                           (sequence " " ?that-right ?subject-left)
                           (sequence " " ?subject-right ?copula-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "foolish child that I was" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "foolish child that I was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn svo-cxn
             ((?svo-unit
               (syn-cat (cat V)
                        (max +)
                        (min -))
               (args (?x ?y ?z))
               (boundaries (?subject-left ?np-right))
               (subunits (?subject-unit ?verb-unit ?noun-phrase)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (cat ?cat)
                        (max +)
                        (role S)
                        (number singular))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?x ?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux ?aux)
                        (number ?number))
               (boundaries (?verb-left ?verb-right)))
              (?noun-phrase
               (args (?y))
               --
               (syn-cat (cat N)  
                        (max +)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (boundaries (?np-left ?np-right))) 
              (?svo-unit
               ;;(HASH meaning (()))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?np-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "I was a foolish child" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "I was a foolish child" :cxn-inventory *fillmores-cxns*)

;;------------------------------------;;
;; the subject predicate construction ;;
;;------------------------------------;;

;; INFL describes a functional head containing a finite head-verb (meaning: a verb that shows tense, person or number)
;; ex: she goes, does your brother know my brother?

;; non-finite verb forms don't show tense, person or number. ex: to go, going, gone.

#|(def-fcg-cxn subject-predicate-cxn
             ((?subject-predicate-unit
               (syn-cat (cat V)
                        (max +)
                        (infl tense)
                        (number ?number))
               (args (?x ?y))
               (boundaries (?subject-left ?verb-right))
               (subunits (?subject-unit ?verb-unit)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (max +)
                        (role S)
                        (number ?number))
               (sequences ((sequence ?subject-string ?subject-left ?subject-right))))
              (?verb-unit
               (args (?x ?y))
               --
               (syn-cat (cat V)
                        (min +)
                        (aux -)
                        (number ?number))
               (sequences ((sequence ?verb-string ?verb-left ?verb-right))))
              (?subject-predicate-unit
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)))))
             :cxn-inventory *fillmores-cxns*)|#

;; (comprehend "she goes" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she goes" :cxn-inventory *fillmores-cxns*)

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
               (syn-class (cat N)
                          (max -))
               (args (?b))
               (boundaries (?x-unit ?removed-unit))
               (subunits (?x-unit ?cousin-unit ?y-unit ?removed-unit)))
              <-
              (?x-unit
               (syn-cat (cat A)
                        (min +))
               (sem-cat (sem-class degrees))
               (args (?x))
               --
               (syn-cat (cat A)
                        (min +))
               (boundaries (?x-left ?x-right)))
              (?cousin-unit
               (sem-cat (cat person))
               (args (?y))
               --
               (syn-cat (cat N)
                        (max -)
                        (number singular))
               (boundaries (?cousin-left ?cousin-right)))
              (?y-unit
               (sem-cat (sem-class times))
               (args (?z))
               --
               (syn-cat (cat Adv)
                        (min +))
               (boundaries (?y-left ?y-right)))
              (?removed-unit
               (args (?a))
               --
               (syn-cat (cat Adj)
                        (min +))
               (boundaries (?removed-left ?removed-right)))
              (?x-cousin-y-removed-unit
               (HASH meaning ((y-generational-step ?x ?y ?z ?a)))
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

#|(def-fcg-cxn x-y-smth-to-smn-cxn
             ((?x-y-smth-to-smn-unit
               (syn-cat (cat V)
                        (max +)
                        (min -)
                        (number ?number))
               ;; (args (?x ?y ?a))
               (subunits (?subject-unit ?verb-unit ?object-unit ?to-unit ?complement-recipient))
               (boundaries (?subject-left ?complement-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (cat ?cat)
                        (max +)
                        (role S)
                        (number ?number))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (number ?number))
               (boundaries (?verb-left ?verb-right))) ;;which types of verbs can fill this? explain, do, give,...
              (?object-unit
               (args (?z))
               --
               (syn-cat (cat N)  
                        (max +)
                        (min ?min)
                        (number ?number))
               (boundaries (?object-left ?object-right)))  
              (?to-unit
               (args (?a))
               (sem-cat (cat destination))
               --
               (syn-cat (cat Prep)
                        (max -)
                        (min +))
               (boundaries ?to-left ?to-right))
              (?complement-recipient
               (args (?b))
               (sem-cat (cat person))
               --
               (syn-cat (cat N)
                        (max +)
                        (min ?min)
                        (number ?number))
               (boundaries (?complement-left ?complement-right)))
              (?x-y-smth-to-smn-unit
               (HASH meaning ((giving-smth-to-smn ?x ?y ?z ?a ?b)))
               --
               (sequences ((sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?object-left)
                           (sequence " " ?object-right ?to-left)
                           (sequence " " ?to-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)|#

;;(comprehend "she gives a book to Joe" :cxn-inventory *fillmores-cxns*)

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
               (syn-cat (cat V)
                        (max +)
                        (min -)
                        (inv +)
                        (infl tense))
               (args (?a))
               (boundaries (?verb-left ?complement-right))
               (subunits (?verb-unit ?subject-unit ?complement-unit)))
              <-
              (?verb-unit
               (args (?x))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +))
               (boundaries (?verb-left ?verb-right)))
              (?subject-unit
               (args (?y))
               --
               (syn-cat (max +)
                        (role S))
               (boundaries (?subject-left ?subject-right)))
              (?complement-unit
               (args (?z))
               --
               (syn-cat ;;(cat ?cat) according to Fillmore, the category satys undefined and takes whatever category is given by the complement, so we don't add anything
                        (max -))
               (boundaries (?complement-left ?complement-right)))
              (?inversion-unit
               (HASH meaning ((hypothesis-x ?a ?x ?y ?z)))
               --
               (HASH form ((sequence " " ?verb-right ?subject-left)
                           (sequence " " ?subject-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "were she here" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "have I lied" :cxn-inventory *fillmores-cxns*) ; as in "never have I lied"
;; (comprehend-and-formulate "was she here" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn never-x-cxn
             ((?never-x-unit
               (args (?x))
               (subunits (?never-unit ?inversion-unit))
               (boundaries (?never-left ?inversion-right)))
              <-
              (?never-unit
               (sem-cat (sem-class negation))
               (args (?x))
               --
               (syn-cat (cat Adv)
                        (min +)))
              (?inversion-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max +)
                        (min -)
                        (inv +)
                        (infl tense)))
              (?never-x-unit
               (HASH meaning ((never-smth ?x ?y)))
               --
               (HASH form ((sequence " " ?never-right ?inversion-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "never have I lied" :cxn-inventory *fillmores-cxns*)


;;----------------------;;
;; non maximal V-phrase ;;
;;----------------------;;

(def-fcg-cxn pronoun-headed-cxn
             ((?pronoun-headed-unit
               (syn-cat (cat P)
                        (max +)
                        (min -))
               (args (?a))
               (boundaries (?from-left ?np-right))
               (subunits (?from-unit ?np-unit)))
              <-
              (?from-unit
               (args (?x))
               --
               (syn-cat (cat Prep)
                        (max -)
                        (min +))
               (boundaries (?from-left ?from-right)))
              (?np-unit
               (args (?y))
               --
               (syn-cat (cat N)  
                        (max +))
               (boundaries (?np-left ?np-right)))
              (?pronoun-headed-unit
               (HASH meaning ((from-smth ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?from-right ?np-left)))))
              :cxn-inventory *fillmores-cxns*)

;; (comprehend "from the book" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn non-max-remove-phrase-cxn
             ((?non-max-remove-phrase-unit
               (syn-cat (cat V)
                        (max -)
                        (min -)) ;; non-maximal verb headed phrase, typically missing a subject
               (args (?a))
               (boundaries (?verb-left ?pp-right))
               (subunits (?verb-unit ?np-unit ?pronoun-phrase-unit)))
              <-
              (?verb-unit
               (args (?x))
               (lexeme remove)
               --
               (syn-cat (cat V)
                        (max -)
                        (min +))
               (boundaries (?verb-left ?verb-right)))
              (?np-unit
               (args (?y))
               --
               (syn-cat (cat N)  
                        (max +))
               (boundaries (?np-left ?np-right)))
              (?pronoun-phrase-unit
               (args (?z))
               --
               (syn-cat (cat P)
                        (max +)
                        (min -))
               (boundaries (?pp-left ?pp-right)))
              (?non-max-remove-phrase-unit
               (HASH meaning ((removed-smth-from-smth ?a ?x ?y ?z)))
               --
               (HASH form ((sequence " " ?verb-right ?np-left)
                           (sequence " " ?np-right ?pp-left)))))
             :cxn-inventory *fillmores-cxns*)


;; (comprehend-and-formulate "removed the bottle from the book" :cxn-inventory *fillmores-cxns*)


#|;;---------------;;
;; TO CONTRIBUTE ;;
;;---------------;;

(def-fcg-cxn contribute-cxn
             ((?contribute-unit
               (syn-cat (cat V)
                        (min +))
               (lexeme contribute))
              <-
              (?contribute-unit
               (HASH meaning ((contribute.01 ?S-A-N ?O-P-N ?C-R-P))) ;; valence : Subject agent noun-headed phrase; optional: Object patient noun-headed phrase, complement recipient "to" + pronoun-headed phrase
               --
               )))|#

;;------------;;
;; wh phrases ;;
;;------------;;

(def-fcg-cxn wh-phrase-cxn
             ((?wh-phrase-unit
               (syn-cat (cat V)
                        (max +)
                        (WH +))
               (args (?x ?y))
               (boundaries (?wh-unit ?verb-unit))
               (subunits (?wh-unit ?verb-unit)))
              <-
              (?wh-unit
               (syn-cat (max +)
                        (WH +)) ;;and cat undefined
               (args (?x))
               --
               (sequences ((sequence ?wh-string ?wh-left ?wh-right))))
              (?verb-unit
               (syn-cat (cat V)) ;; and max undefined
               --
               (sequences ((sequence ?verb-string ?verb-left ?verb-right))))
              (?wh-phrase-unit
               --
               (sequences ((sequence " " ?wh-right ?verb-left))))))

;;---------------;;
;; worth knowing ;;
;;---------------;;

;; "she is worth knowing"
;; "she seems worth knowing"

;;---------------;;
;; DO ONE'S BEST ;;
;;---------------;;

;; "I did my best"
;; "she did her best"

;;---------;;
;; TO HAVE ;;
;;---------;;

              
              