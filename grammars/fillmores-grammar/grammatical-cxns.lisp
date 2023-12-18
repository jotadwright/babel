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
               (sem-cat (sem-class unique-role))
               (args (?a))
               (subunits (?person-unit ?of-unit ?whole-unit))
               (boundaries (?person-left ?complement-right)))
              <-
              (?person-unit
               (args (?x))
               (sem-cat (sem-class person))
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
               (args (?a))
               (subunits (?subject-unit ?copula-unit ?role-unit))
               (boundaries (?subject-left ?role-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (max +)
                        (role S)
                        (number singular))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number singular))
               (morph-cat (lexeme be))
               (boundaries (?copula-left ?copula-right)))
              (?role-unit
               (sem-cat (sem-class unique-role))
               (args (?z))
               --
               (syn-cat (cat N)
                        (max -)
                        (min -)
                        (number singular))
               (boundaries (?role-left ?role-right)))
              (?unique-role-unit
               (HASH meaning ((x-unique-role ?a ?x ?y ?z)))
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
                        (max -)
                        (min +)
                        (number ?number))
               (morph-form (starts-with ?starts-with))
               (boundaries (?adj-left ?adj-right)))
              (?noun-unit
               (args (?x))
               --
               (syn-cat (cat N)
                        (max -)
                        (min +)
                        (number ?number))
               (boundaries (?noun-left ?noun-right)))
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
               (args (?z ?a ?b))
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
               (syn-cat (cat Conj))
               (boundaries (?that-left ?that-right)))
              (?subject-unit
               (args (?c))
               --
               (syn-cat (max +)
                        (role S)
                        (number ?number))
               (boundaries (?subject-left ?subject-right)))
              (?copula-unit
               (args (?d))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +)
                        (number ?number))
               (boundaries (?copula-left ?copula-right)))
              (?fronting-to-that-unit
               (HASH meaning ((focus-on-non-maximal-nominal-phrase ?z ?a ?b ?c ?d)))
               --
               (HASH form ((sequence " " ?modified-noun-right ?that-left)
                           (sequence " " ?that-right ?subject-left)
                           (sequence " " ?subject-right ?copula-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "foolish child that I was" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "foolish child that I was" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn be-smth-cxn
             ((?be-smth-unit
               (syn-cat (cat V)
                        (max -)
                        (min -))
               (morph-cat (person ?person))
               (args (?a))
               (boundaries (?verb-left ?np-right))
               (subunits (?verb-unit ?noun-phrase)))
              <-
              (?verb-unit
               (args (?x))
               (morph-cat (lexeme be))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux ?aux)
                        (number ?number))
               (morph-cat (person ?person))
               (boundaries (?verb-left ?verb-right)))
              (?noun-phrase
               (args (?y))
               --
               (syn-cat (cat N)  
                        (max +)
                        (number ?number))
               (boundaries (?np-left ?np-right))) 
              (?be-smth-unit
               (HASH meaning ((fill-meaning ?a ?x ?y)))
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
               (syn-cat (cat V)
                        (max +)
                        (infl tense)
                        (number ?number))
               (args (?a))
               (boundaries (?subject-left ?verb-right))
               (subunits (?subject-unit ?verb-unit)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (max +)
                        (role S)
                        (number ?number))
               (morph-cat (person ?person))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min -)) ;; non-maximal verb headed phrase, typically missing a subject
               (morph-cat (person ?person))
               (boundaries (?verb-left ?verb-right)))
              (?subject-predicate-unit
               (HASH meaning ((svo ?a ?x ?y)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she removes the bottle from the book" :cxn-inventory *fillmores-cxns*)

;; (comprehend "I removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "I removed the bottle from the book" :cxn-inventory *fillmores-cxns*)

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
               (subunits (?x-unit ?cousin-unit ?y-unit ?removed-unit))
               (boundaries (?x-left ?removed-right)))
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
               (sem-cat (sem-class person))
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
               (syn-cat (cat A)
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

(def-fcg-cxn x-y-smth-to-smn-cxn
             ((?x-y-smth-to-smn-unit
               (syn-cat (cat V)
                        (max +)
                        (min -))
               (args (?c))
               (subunits (?subject-unit ?verb-unit ?object-unit ?to-unit ?complement-recipient))
               (boundaries (?subject-left ?complement-right)))
              <-
              (?subject-unit
               (args (?x))
               --
               (syn-cat (max +)
                        (role S)
                        (number ?number))
               (morph-cat (person ?person))
               (boundaries (?subject-left ?subject-right)))
              (?verb-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (number ?number))
               (morph-cat (person ?person))
               (boundaries (?verb-left ?verb-right))) ;;which types of verbs can fill this? explain, do, give,...
              (?object-unit
               (args (?z))
               --
               (syn-cat (cat N)  
                        (max +))
               (boundaries (?object-left ?object-right)))             
              (?to-unit
               (args (?a))
               (sem-cat (sem-class destination))
               --
               (syn-cat (cat Prep)
                        (max -)
                        (min +))
               (boundaries (?to-left ?to-right)))
              (?complement-recipient
               (args (?b))
               (sem-cat (sem-class person)) ;; can be a moral person, e.g. "the school"
               --
               (syn-cat (cat N)
                        (max +))
               (boundaries (?complement-left ?complement-right)))
              (?x-y-smth-to-smn-unit
               (HASH meaning ((giving-smth-to-smn ?c ?x ?y ?z ?a ?b)))
               --
               (HASH form ((sequence " " ?subject-right ?verb-left)
                           (sequence " " ?verb-right ?object-left)
                           (sequence " " ?object-right ?to-left)
                           (sequence " " ?to-right ?complement-left)))))
             :cxn-inventory *fillmores-cxns*)

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
               (syn-class (cat V)
                          (max +)
                          (min -))
               (args (?x))
               (subunits (?never-unit ?inversion-unit))
               (boundaries (?never-left ?inversion-right)))
              <-
              (?never-unit
               (sem-cat (sem-class negation))
               (args (?x))
               --
               (syn-cat (cat Adv)
                        (max -)
                        (min +)
                        (WH -))
               (boundaries (?never-left ?never-right)))
              (?inversion-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max +)
                        (min -)
                        (inv +)
                        (infl tense))
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
;; (comprehend-and-formulate "from the book" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn non-max-remove-phrase-cxn
             ((?non-max-remove-phrase-unit
               (syn-cat (cat V)
                        (max -)
                        (min -)) ;; non-maximal verb headed phrase, typically missing a subject
               (morph-cat (person ?person))
               (args (?a))
               (boundaries (?verb-left ?pp-right))
               (subunits (?verb-unit ?np-unit ?pronoun-phrase-unit)))
              <-
              (?verb-unit
               (args (?x))
               (morph-cat (lexeme remove)
                          (person ?person))
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
               (HASH meaning ((remove-smth-from-smth ?a ?x ?y ?z)))
               --
               (HASH form ((sequence " " ?verb-right ?np-left)
                           (sequence " " ?np-right ?pp-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend-and-formulate "removed the bottle from the book" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "removes the bottle from the book" :cxn-inventory *fillmores-cxns*)


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
               (args (?a))
               (subunits (?wh-unit ?vp-unit))
               (boundaries (?wh-left ?vp-right)))
              <-
              (?wh-unit
               (args (?x))
               --
               (syn-cat (cat Adv)
                        (max -)
                        (min +)
                        (WH +))               
               (boundaries (?wh-left ?wh-right)))
              (?vp-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (min -)) ;; max and inv are not spcified because they are not necessary
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
               (syn-cat (cat A)
                        (max +)
                        (min -))
               (sem-cat (sem-class worth))
               (args (?a))
               (subunits (?worth-unit ?ing-unit))
               (boundaries (?worth-left ?ing-right)))
              <-
              (?worth-unit
               (args (?x))
               --
               (syn-cat (cat A)
                        (max -)
                        (min +))
               (morph-cat (lexeme worth))
               (boundaries (?worth-left ?worth-right)))
              (?ing-unit
               (args (?y))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +))
               (morph-cat (tense ing))
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
               (syn-cat (cat V)
                        (max -)
                        (min -))
               (morph-cat (person ?person)
                          (tense ?tense))
               (args (?a))
               (subunits (?verb-unit ?worth-unit))
               (boundaries (?verb-left ?worth-right)))
              <-
              (?verb-unit
               (args (?x))
               --
               (syn-cat (cat V)
                        (max -)
                        (min +)
                        (aux +))
               (morph-cat (lexeme be)
                          (tense ?tense)
                          (person ?person))
               (boundaries (?verb-left ?verb-right)))
              (?worth-unit
               (args (?y))
               (sem-cat (sem-class worth))
               --
               (syn-cat (cat A)
                        (max +)
                        (min -))
               (boundaries (?worth-left ?worth-right)))
              (?be-worth-ing-unit
               (HASH meaning ((be-worthy-of ?a ?x ?y)))
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
              (syn-cat (cat N)
                       (max +))
              (morph-cat (person ?person))
              (args (?a))
              (subunits (?possessive-unit ?best-unit))
              (boundaries (?possessive-left ?best-right)))
             <-
             (?possessive-unit
              (args (?x))
              --
              (syn-cat (cat N)
                       (max -)
                       (min +)
                       (role det))
              (morph-cat (morph poss)
                         (subcat pronoun)
                         (person ?person))
              (boundaries (?possessive-left ?possessive-right)))
             (?best-unit
              (args (?y))
              --
              (syn-cat (cat N)
                       (max -)
                       (min +))
              (boundaries (?best-left ?best-right)))
             (?ones-best-unit
              (HASH meaning ((ones-best ?a ?x ?y)))
              --
              (HASH form ((sequence " " ?possessive-right ?best-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "her best" :cxn-inventory *fillmores-cxns*)

(def-fcg-cxn do-ones-best-cxn
             ((?do-ones-best-unit
               (syn-cat (cat V)
                        (max -)
                        (min -))
               (morph-cat (person ?person)
                          (tense ?tense))
               (args (?x))
               (subunits (?do-unit ?ones-best-unit))
               (boundaries (?do-left ?ones-best-right)))
              <-
              (?do-unit
               (args (?x))
               (morph-cat (lexeme do)
                          (person ?person)
                          (tense ?tense))
               --
               (syn-cat (cat V)
                        (min +))
               (morph-cat (lexeme do)
                          (person ?person)
                          (tense ?tense))
               (boundaries (?do-left ?do-right)))
              (?ones-best-unit
               (args (?y))
               --
               (syn-cat (cat N)
                        (max +))
               (morph-cat (person ?person))
               (boundaries (?ones-best-left ?ones-best-right)))
              (?do-ones-best-unit
               (HASH meaning ((give-ones-best ?x ?y)))
               --
               (HASH form ((sequence " " ?do-right ?ones-best-left)))))
             :cxn-inventory *fillmores-cxns*)

;; (comprehend "does her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "does her best" :cxn-inventory *fillmores-cxns*)

;; (comprehend "she does her best" :cxn-inventory *fillmores-cxns*)
;; (comprehend-and-formulate "she does her best" :cxn-inventory *fillmores-cxns*)

;; "I did my best"
;; "she did her best"
              
              