
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This File contains a tutorial for using type-hierarchies in FCG's match and merge ;;
;; File by Paul - 01/2017                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :type-hierarchies)
(in-package :type-hierarchies)
(activate-monitor trace-fcg)

(def-fcg-constructions-with-type-hierarchy type-hierarchy-example-grammar
  :feature-types ((args sequence)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set))
  
  (def-fcg-cxn cat-cxn
               ((?cat-unit
                 (syn-cat (lex-class count-noun))
                 (sem-cat (sem-class animal))
                 (args (?x)))
                <-
                (?cat-unit
                 (HASH meaning ((cat ?x)))
                 --
                 (HASH form ((string ?cat-unit "cat")))))
               :description "Lexical unit for the word cat.")

  (def-fcg-cxn grass-cxn
               ((?grass-unit
                 (syn-cat (lex-class mass-noun))
                 (sem-cat (sem-class plant))
                 (args (?x)))
                <-
                (?grass-unit
                 (HASH meaning ((grass ?x)))
                 --
                 (HASH form ((string ?grass-unit "grass")))))
               :description "Lexical unit for the word grass.")

  (def-fcg-cxn the-cxn
               ((?the-unit
                 (syn-cat (lex-class determiner))
                 (sem-cat (sem-function identifier))
                 (args (?x)))
                <-
                (?the-unit
                 (HASH meaning ((unique ?x)))
                 --
                 (HASH form ((string ?the-unit "the")))))
               :description "Lexical unit for the word the.")  

  ;; NP -> Det N
  (def-fcg-cxn np-cxn
               ((?np-unit
                 (args (?args))
                 (syn-cat (lex-class np))
                 (sem-cat (sem-function referring-expression))
                 (subunits (?det ?noun)))
                <-
                (?det
                 (sem-cat (sem-function identifier))
                 (args (?args))
                 --
                 (syn-cat (lex-class determiner)))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun)))))
               :description "Grammatical construction combining a determiner and a noun into a noun phrase."))

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Using type hierarchies in Fluid Construction Grammar"))
  (add-element '((h3) "Paul Van Eecke"))
  
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "WD-1. Introduction.")))
  (add-element '((p)  ((a :href "#WD-2") "WD-2. An Example Grammar.")))
  (add-element '((p)  ((a :href "#WD-3") "WD-3. Specifying the type-hierarchy.")))
  (add-element '((p)  ((a :href "#WD-4") "WD-4. Using the type-hierarchy.")))
  (add-element '((hr))))

(defun introduction ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "WD-1. Introduction."))
  (add-element '((p) "The type-hierarchy package is an extension to FCG which makes it possible to represent hierarchies of values (categories), to build these hierarchies up, and to use them in FCG processing (match and merge). This extension is particularly useful for capturing generalisations about the relations between different categories. These can be of any type, e.g. semantic (ontology-like) in for example plant -> physical-object, or syntactic as in count-noun -> noun."))
  (add-element '((hr))))

(defun grammar ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "WD-2. An example grammar."))
  (add-element '((p) "We use the macro" ((code) " def-fcg-constructions-with-type-hierarchy") " for constructing a cxn-inventory that can be used by the type-hierarchy package. It is very similar to the default" ((code) " def-fcg-constructions") " macro except for the fact that it adds an initially empty type-hierarchy to the construction-inventory."))
  (add-element '((p) "The construction-inventory holds four constructions:"))
  (add-element '((ul)
                 ((li)"A lexical construction for the determiner 'the'")
                 ((li)"A grammatical construction combining a determiner and a noun into a noun phrase, the noun should have (lex-class noun) and (sem-class physical-object)")
                 ((li)"A lexical construction for 'cat', being of (lex-class count-noun) and (sem-class animal)")
                 ((li)"A lexical construction for 'grass', being of (lex-class mass-noun) and (sem-class plant)")))
  (add-element '((p) "When comprehending of formulating with this grammar in standard FCG, the NP-cxn can never apply. This is because the values of the lex-class and sem-class features of the nouns do not correspond with those requested in the NP-cxn"))
  (add-element '((p) "The construction-inventory is shown below:"))
  (add-element (make-html *fcg-constructions* :expand-initially t))
  (add-element '((hr))))

(defun type-hierarchy ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "WD-3. Specifying the type-hierarchy."))
  (add-element '((p) "We will now add a few categories and relations to the type-hierarchy of the construction-inventory, such that the NP-cxn will be able to apply. We will specify that 'mass-noun' and 'count-noun' are subtypes of 'common-noun', which is a subtype of 'noun'. We will also specify that both 'plant' and 'animal' are subtypes of 'physical-object'."))
  (add-element '((p) "We will now add a few categories and relations to the type-hierarchy of the construction-inventory, such that the NP-cxn will be able to apply. We will specify that 'mass-noun' and 'count-noun' are subtypes of 'common-noun', which is a subtype of 'noun'. We will also specify that both 'plant' and 'animal' are subtypes of 'physical-object'. The resulting type-hierarchy of the construction-inventory is visualised below: "))
  (let ((th (get-type-hierarchy *fcg-constructions*)))
  ;; more syntactic (used by lex-class)
  (add-categories '(noun mass-noun count-noun common-noun proper-noun) th)
  (add-link 'proper-noun 'noun th)
  (add-link 'common-noun 'noun th)
  (add-link 'mass-noun 'common-noun th)
  (add-link 'count-noun 'common-noun th)
  ;; more semantic (used by sem-class)
  (add-categories '(physical-object plant animal) th)
  (add-link 'animal 'physical-object th)
  (add-link 'plant 'physical-object th)
  th)
  (add-element (make-html (get-type-hierarchy *fcg-constructions*)))
  (add-element '((hr))))

(defun example ()
  (add-element '((a :name "WD-4")))
  (add-element '((h2) "WD-4. Using the type-hierarchy: example."))
  (add-element '((p) "When a construction-inventory contains a non-empty type-hierarchy, it is automatically used by the FCG processing engine (match and merge). Matching will succeed if a directed path is found between the value in the transient structure and the value in the construction. Merging will retain the value of the existing transient structure in the new transient structure."))
  (add-element '((p) "In the following example, we comprehend 'the cat'. The NP-cxn can apply as count-noun (transient structure) is found to be a subtype of noun (construction), and animal (transient structure) a subtype of physical-object (construction)." ))
  (comprehend "the cat")
  (add-element '((hr)))
  (add-element '((p) "In the following example, we formulate the meaning ((unique o-2) (grass o-2))'. The NP-cxn can apply as mass-noun (transient structure) is found to be a subtype of noun (construction), and plant (transient structure) a subtype of physical-object (construction)." ))
  (formulate '((unique o-2) (grass o-2)))
  (add-element '((hr))))

(defun make-demo ()
  (create-static-html-page "Type hierarchies in FCG"
    (header)
    (introduction)
    (grammar)
    (type-hierarchy)
    (example))
  )

;; (make-demo)
