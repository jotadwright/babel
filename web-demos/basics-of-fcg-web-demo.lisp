;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE BASICS OF FLUID CONSTRUCTION GRAMMAR    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Luc Steels, may 2016
;;; with help from Katrien Beuls and Paul Van Eecke

;;;; 1. Load and set up FCG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)

; don't forget to open a web browser at http://localhost:8000

;; Larger font for text in <p> tags
(define-css 'main
            "p {font-size: 10pt}")

(defun my-head-menu ()
  ; the header file 
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (set-configuration *fcg-constructions* :form-predicates '(meets))
  (add-element
   '((h1) "The Basics of Fluid Construction Grammar"))
  (add-element '((p) "This is a web demo that supplements the paper:"))
  (add-element '((p) ((i)"Steels, L. (2017). " ((a :href "https://www.fcg-net.org/wp-content/uploads/papers/basics-of-fcg.pdf") "Basics of Fluid Construction Grammar.") " Constructions and Frames. Submitted.")))
; how to make a link here to: "https:www.fcg-net.org/projects/web-demonstration-guide/"
  (add-element '((p)"Please check our " ((a :href "https:www.fcg-net.org/projects/web-demonstration-guide/") "web demonstration guide ") "to find out more on what you can see in the demo and what you can do with it."))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#simple-np") "I. Simple noun phrase construction")))
  (add-element '((h3)  ((a :href "#intransitive") "II. Intransitive clause construction")))
  (add-element '((h3)  ((a :href "#transitive") "III. Transitive clause construction")))
  (add-element '((h3)  ((a :href "#ditransitive") "IV. Ditransitive clause construction")))
  (add-element '((p :style "color:darkred") "DISCLAIMER: It is recommended to use Firefox or Safari to optimally explore the contents of this page.")))

;(my-head-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; I. A simple noun phrase 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(set-configuration *fcg-visualization-configuration* :selected-hierarchy 'subunits)

(def-fcg-constructions basic-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (boundaries set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (sem-class set))
  :fcg-configurations ((:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:node-tests :update-references :check-duplicate )
                       (:production-goal-tests :no-applicable-cxns)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network))
  :hierarchy-features (subunits)
;  :cxn-inventory *cxn-inventory*

  (def-fcg-cxn simple-np-cxn
               ((?np-unit
                 (referent ?referent)
                 (sem-cat (sem-fun referring)
                          (sem-class ?sem-class))
                 (syn-cat (phrasal-cat NP)
                          (number ?number))
                 (subunits (?art-unit ?noun-unit)))
                (?art-unit
                 (syn-cat (syn-fun (determiner ?np-unit))))
                (?noun-unit
                 (syn-cat (syn-fun (head ?np-unit))))
                <-
                (?np-unit
                 --
                 (HASH form ((meets ?art-unit ?noun-unit ?np-unit))))
                (?art-unit
                 (args (?referent))
                 --
                 (syn-cat (lex-cat article)
                          (number ?number)))
                (?noun-unit
                 (args (?referent))
                 (sem-cat (sem-class ?sem-class))
                 --
                 (syn-cat (lex-cat noun)
                          (number ?number)))))

  (def-fcg-cxn girl-cxn
                           ; stores also association between "girl" and the construction in *vocabulary*
               ((?girl-word
                 (args (?referent))
                 (sem-cat (sem-class (physobj animate feminine)))
                 (syn-cat (lex-cat noun)
                          (number singular)))
                <-
                (?girl-word
                 (HASH meaning ((person girl ?referent)))                     
                 --
                 (HASH form ((string ?girl-word  "girl"))))))

  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?referent))
                 (syn-cat (lex-cat article)
                          (number ?number)))
                <-
                (?the-word
                 (HASH meaning ((specificity definite ?referent)))                     
                 --
                 (HASH form ((string ?the-word  "the")))))))

(defun display-constructions-simple-np (cxn-inventory)
  (add-element '((a :name  "simple-np")))
  (add-element '((h2) "I. A simple noun phrase construction."))
  (add-element '((h3) "(I.a.) The following construction schemas are loaded in memory:"))
  (add-element '((p) "(click on encircled + to zoom in and on encircled dot to zoom out):"))
  (add-element '((p) "1. <i> A construction schema for the article `the'.</i> There is only one unit involved, which is for
the word itself. On the right hand side of the unit is the lock.
The production lock (top) requires that the meaning includes `specificity(definite,?referent). 
The comprehension lock (bottom) requires that there is a string `the' in the input. On the left hand is the contributor. It specifies the arguments of the meaning and syntactic-categories which include
a lex-cat (filled with article) and a number feature, the latter filled with ?number. ?number is a variable because `the' can be 
singular or plural."))
  (add-element (make-html (find-cxn 'the-cxn cxn-inventory)))
  (add-element '((p) "2. <i> A construction schema for the noun `girl'.</i> 
The production lock requires that the meaning includes `person(girl,?referent). The comprehension lock requires that there is a string `girl' in the input. The contributor introduces meaning and syntactic categories. Note that number is now singular. There are also some semantic categories."))
  (add-element (make-html (find-cxn 'girl-cxn cxn-inventory)))
  (add-element '((p) "3. <i> A construction schema for a simple noun-phrase. </i> 
The production lock requires that the args value is the same of two units: the article and noun-unit. The comprehension lock requires
that there are two units, to be bound to ?art-unit and ?noun-unit. The ?art-unit must have the value article for its lex-cat and
the ?noun-unit must have the value noun. The ?art-unit must come before the ?noun-unit, specified as a meets-constraint within the NP. 
There is agreement for number between the ?art-unit and ?noun-unit because they share the same variable ?number for the number features and
number percolates up to the ?NP-unit if found. The contributor specifies the ?np-unit
that will be merged: On the semantic side, the referent of the ?np-unit is ?x, which is equal to the arguments of the two 
subunits, the semantic function of the ?np-unit is referring, the phrasal-cat is NP, and the subunits are the ?noun-unit and ?article-unit.
The sem-class and agreement feature values are inherited from the ?noun-unit. The syntactic functions of the ?art-unit 
and ?noun-unit in the ?NP-unit are head and determiner respectively."))
  (add-element (make-html (find-cxn 'simple-np-cxn cxn-inventory))))

(defun parse-example-the-girl (cxn-inventory)
  (add-element '((h3) "(I.b) Comprehending an utterance: 'the girl'."))
  (add-element '((p) "Comprehension takes place by the the successive application of construction schemas. It starts from
an initial transient structure that contains a base unit (called root) that represents information about which words occur and
their ordering in terms of meets-constraints. The web-interface shows the different steps, which construction schemas 
have been applied and the final resulting transient structure. The color of each box shows whether the step succeeded
(green) or not (light green). Construction schemas are applied in no particular order. A construction applies as soon as 
their lock is able to match with the transient structure. At each node goal-tests are executed to see whether a successful
final transient structure is reached. For parsing: there are two of them here: :no-applicable-cxns 
and :connected-semantic-network. The latter will signal a failure when all the predicates supplied by the final meaning are
not connected."))
  (add-element '((p) "Click once on a box in the application process 
to see the transient structure at that point and once again to see more detail: the transient structure before, the construction
schema that is applied, the transient structure after application (called the resulting structure), the bindings that were
used in the construction application process and the meaning so far (in case of comprehending) or 
the utterance so far (in case of formulating). Details are in a lower-level notation close to the
internal datastructures used by FCG. This notation is documented and used in Steels, L. (2011) Design patterns in Fluid Construction Grammar.
John Benjamins Pub, Amsterdam."))
  (comprehend '("the" "girl") :cxn-inventory cxn-inventory))

(defun parse-example-girl-the (inventory)
  (add-element '((h3) "(I.c) Example of parsing an ungrammatical utterance: 'girl the'."))
  (add-element '((p) "If 'girl the' is given as input, the lexical constructions for 'girl' and 'the' still apply. However no noun phrase gets
built because the simple-np-cxn cannot apply due to a violation of the ordering constraint.  We see that the final box in the application 
process has a light green color and if you click on that you see that the goal-tests have failed, in particular the
meaning fragments supplied by the individual words are not connected."))
  (comprehend '("girl" "the") :cxn-inventory inventory))

(defun produce-example-the-girl (inventory)
  (add-element '((h3) "(I.d) Producing an utterance: 'the girl'."))
  (add-element '((p) "Producing happens using the exact same construction inventory and the same processing engine, but now the matcher uses
the production-locks rather than the comprehension-locks."))
  (formulate '((specificity definite obj-1)(person girl obj-1)) :cxn-inventory inventory))

(defun simple-np-example () 
  (let ((inventory (make-basic-grammar-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-simple-np inventory)
   (parse-example-the-girl inventory) (parse-example-girl-the inventory) (produce-example-the-girl inventory)))

;; test this example (progn (clear-page) (simple-np-example))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; An intransitive clause 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions basic-grammar-2
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (boundaries set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (sem-class set))
  :fcg-configurations ((:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :update-references :check-duplicate )
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network))
  :hierarchy-features (subunits)

  (def-fcg-cxn intransitive-cxn
               ((?intransitive-clause
                 (subunits
                  (?subject-unit ?verb-unit)))
                <-
                (?intransitive-clause
                 --
                 (HASH form ((meets ?subject-unit ?verb-unit ?intransitive-clause))))
                (?verb-unit 
                 (referent ?event)
                 (sem-cat (sem-fun predicating)
                          (frame (actor ?agent)))
                 --
                 (syn-cat (lex-cat verb)
                          (syn-valence (subject ?subject-unit))
                          (number ?number)))
                (?subject-unit
                 (referent ?agent)
                 (sem-cat (sem-fun referring))
                 --
                 (sem-cat (sem-class (animate)))
                 (syn-cat (phrasal-cat NP)
                          (case nominative)
                          (number ?number)))))

  (def-fcg-cxn bakes-cxn
               ((?bakes-unit
                 (referent ?event)
                 (args (?event ?baker ?baked))
                 (sem-cat (sem-class (event))
                          (sem-fun predicating)
                          (frame (actor ?baker)
                                 (undergoer ?baked)))
                 (syn-cat (lex-cat verb)
                          (syn-valence (subject ?subj)
                                       (direct-object ?dir-obj))
                          (number singular)))
                <-
                (?bakes-unit
                 (HASH meaning ((action bake ?event) (baker ?event ?baker) (baked ?event ?baked)))
                 --
                 (HASH form ((string ?bakes-unit "bakes"))))))

  (def-fcg-cxn he-cxn
               ((?he-word
                 (referent ?person)
                 (args (?person))
                 (sem-cat (sem-class (physobj animate masculine))
                          (sem-fun referring))
                 (syn-cat (lex-cat pronoun)
                          (phrasal-cat NP)
                          (number singular)
                          (case nominative)))
                <-
                (?he-word
                 (HASH meaning ((person male ?person)))
                 --
                 (HASH form ((string ?he-word  "he")))))))

(defun display-constructions-intransitive (cxn-inventory)
  (add-element '((h2) ((a :name  "intransitive") "II. An intransitive clause construction.")))
  (add-element '((p) "This illustrates a very simple clausal construction with the utterance `he bakes'"))
  (add-element '((p) "The following <i> lexical construction schemas </i> are loaded in memory:"))
  (add-element (make-html (find-cxn 'he-cxn cxn-inventory))) ; he 
  (add-element (make-html (find-cxn 'bakes-cxn cxn-inventory)))
  (add-element '((p) "There is a single grammatical construction schema for an intransitive clause. This clause uses three constraints on the syntactic side: lexical categories (there must be an NP and a verb), ordering constraints (the NP must come before the verb), and agreement for number between the subject and the verb."))
  (add-element (make-html (find-cxn 'intransitive-cxn cxn-inventory))))
  
(defun parse-example-he-bakes (inventory)
  (add-element '((h3) "(II.a) Parsing an intransive clause: 'He bakes'."))
  (add-element '((p) "Of particular interest here is the way that argument bindings between the verb and its objects is achieved in FCG. We see that the bakes-cxn introduces a bake-event in the meaning feature with roles for a baker and a baked. It has also a case frame 
with slots for an actor and an undergoer, but the variables of baker/baked and actor/undergoer are not yet made equal because that will be
done by the intransitive construction. (See van Trijp, Remi (2011). A design pattern for argument structure constructions. In: Steels, Luc (ed.), Design Patterns in Fluid Construction Grammar (pp. 115-145). Amsterdam: John Benjamins.)"))
  (add-element '((p) "After the application of the intransitive-cxn, we see that the referent-variable associated with `he' has been linked properly with the ?agent-variable for `bake'. You can see this clearly by clicking on the occurrence of a variable and then the interface colors all occurrences with the same hue."))
  (comprehend '("he" "bakes" ) :cxn-inventory inventory))

(defun produce-example-he-bakes (inventory)
  (add-element '((h3) "Producing an intransive clause: 'He bakes'."))
  (add-element '((p) "The same construction schemas work again for formulation, but now the production-locks are being used for matching. Note that the role of baked in the bake-event (ev-1) is filled with a variable, implying that it is unknown."))
  (formulate '((person male o-2) (action bake ev-1) (baker ev-1 o-2) (baked ev-1 ?unknown-object))
           :cxn-inventory
            inventory))

(defun intransitive-example () 
  (let ((inventory (make-basic-grammar-2-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-intransitive inventory)
   (parse-example-he-bakes inventory) (produce-example-he-bakes inventory)))

;test (progn (clear-page) (intransitive-example))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; A transitive clause 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions basic-grammar-3
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (boundaries set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (sem-class set)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:node-tests :update-references :check-duplicate )
                       (:production-goal-tests :no-applicable-cxns)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network))
  :hierarchy-features (subunits)

(def-fcg-cxn bakes-cxn
             ((?bakes-unit
               (referent ?event)
               (args (?event ?baker ?baked))
               (sem-cat (sem-class (event))
                        (sem-fun predicating)
                        (frame (actor ?baker)
                               (undergoer ?baked)))
               (syn-cat (lex-cat verb)
                        (syn-valence (subject ?subj)
                                     (direct-object ?dir-obj))
                        (number singular)))
              <-
              (?bakes-unit
               (HASH meaning ((action bake ?event) (baker ?event ?baker) (baked ?event ?baked)))
               --
               (HASH form ((string ?bakes-unit "bakes"))))))

(def-fcg-cxn simple-np-cxn
             ((?np-unit
               (referent ?referent)
               (sem-cat (sem-fun referring)
                        (sem-class ?sem-class))
               (syn-cat (phrasal-cat NP)
                        (number ?number)
                        (case ?case))
               (subunits (?art-unit ?noun-unit)))
              (?art-unit
               (syn-cat (syn-fun (determiner ?np-unit))))
              (?noun-unit
               (syn-cat (syn-fun (head ?np-unit))))
              <-
              (?np-unit
               --
               (HASH form ((meets ?art-unit ?noun-unit ?np-unit))))
              (?art-unit
               (args (?referent))
               --
               (syn-cat (lex-cat article)
                        (number ?number)))
              (?noun-unit
               (args (?referent))
               (sem-cat (sem-class ?sem-class))
               --
               (syn-cat (lex-cat noun)
                        (number ?number)
                        (case ?case)))))

(def-fcg-cxn a-cxn
             ((?a-word
               (args (?referent))
               (syn-cat (lex-cat article)
                        (number singular)))
              <-
              (?a-word
               (HASH meaning ((specificity indefinite ?referent)))                     
               --
               (HASH form ((string ?a-word  "a"))))))

(def-fcg-cxn cake-cxn
             ((?cake-word
               (args (?referent))
               (sem-cat (sem-class (physobj inanimate)))
               (syn-cat (lex-cat noun)
                        (number singular)
                        (case ?case)))
              <-
              (?cake-word
               (HASH meaning ((physobj cake ?referent)))                     
               --
               (HASH form ((string ?cake-word  "cake"))))))

(def-fcg-cxn he-cxn
             ((?he-word
               (referent ?referent)
               (args (?referent))
               (sem-cat (sem-class (physobj animate masculine))
                        (sem-fun referring))
               (syn-cat (lex-cat pronoun)
                        (phrasal-cat NP)
                        (number ?number)
                        (case nominative)))
              <-
              (?he-word
               (HASH meaning ((person male ?referent)))                     
               --
               (HASH form ((string ?he-word  "he"))))))

(def-fcg-cxn transitive-cxn
             ((?transitive-clause
               (subunits
                (?subject-unit ?verb-unit ?direct-object-unit)))
              (?verb-unit
               (syn-cat (syn-fun (head ?transitive-clause))))
              (?subject-unit
               (syn-cat (syn-fun (subject ?transitive-clause))))
              (?direct-object-unit
               (syn-cat (syn-fun (direct-object ?transitive-clause))))
              <-
              (?transitive-clause
               --
               (HASH form ((meets ?subject-unit ?verb-unit ?transitive-clause)
                           (meets ?verb-unit ?direct-object-unit ?transitive-clause))))
              (?verb-unit 
               (referent ?event)
               (sem-cat (sem-fun predicating)
                        (frame (actor ?causer)
                               (undergoer ?product)))
               --
               (syn-cat (lex-cat verb)
                        (syn-valence (subject ?subject-unit))
                        (number ?number)))
              (?direct-object-unit
               (referent ?product)
               (sem-cat (sem-fun referring))
               --
               (sem-cat (sem-class (inanimate)))
               (syn-cat (phrasal-cat NP)
                        (case not-nominative)))
              (?subject-unit
               (referent ?causer)
               (sem-cat (sem-fun referring))
               --
               (sem-cat (sem-class (animate)))
               (syn-cat (phrasal-cat NP)
                        (number ?number)
                        (case nominative))))))

(defun display-constructions-transitive (cxn-inventory)
  (add-element '((h2) ((a :name  "transitive") "III. A transitive clause construction.")))
  (add-element '((p) "This illustrates a very simple clausal construction illustrated with the utterance `he bakes a cake'."))
  (add-element '((p) "The following <i> lexical construction schemas </i> are loaded in memory:"))
  (add-element (make-html (find-cxn 'he-cxn cxn-inventory))) ; he
  (add-element (make-html (find-cxn 'a-cxn cxn-inventory))) ; a 
  (add-element (make-html (find-cxn 'cake-cxn cxn-inventory))) ; cake
  (add-element (make-html (find-cxn 'bakes-cxn cxn-inventory))) ; bakes
  (add-element '((p) "There is the same simple-np-cxn as before and an additional grammatical construction schema for a transitive clause. The schema has an additional unit for a direct object with not-nominative case and regulates how the roles of the action introduced by 
the verb get bound to their fillers."))
  (add-element (make-html (find-cxn 'transitive-cxn cxn-inventory))))

(defun parse-example-he-bakes-a-cake (inventory)
  (add-element '((h3) "IIIa. Parsing a transitive clause: 'He bakes a cake'."))
  (comprehend '("he" "bakes" "a" "cake") :cxn-inventory inventory))

(defun produce-example-he-bakes-a-cake (inventory)
  (add-element '((h3) "IIIb. Producing a transive clause: 'He bakes a cake'."))
  (formulate '((physobj cake o-1) (specificity indefinite o-1)(person male o-2) (action bake ev-1) (baker ev-1 o-2) (baked ev-1 o-1))
             :cxn-inventory inventory))

(defun transitive-example () 
  (let ((inventory (make-basic-grammar-3-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-transitive inventory)
   (parse-example-he-bakes-a-cake inventory) (produce-example-he-bakes-a-cake inventory)))

;test (progn (clear-page) (transitive-example))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; A ditransitive clause 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions basic-grammar-4
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (boundaries set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (dependents set-of-predicates)
                  (sem-class set)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :update-references :check-duplicate )
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network))
  :hierarchy-features (subunits)

(def-fcg-cxn a-cxn
             ((?a-word
               (args (?x))
               (syn-cat (lex-cat article)
                        (number singular)))
              <-
              (?a-word
               (HASH meaning ((specificity indefinite ?x)))                     
               --
               (HASH form ((string ?a-word  "a"))))))

(def-fcg-cxn cake-cxn
             ((?cake-word
               (head ?cake-word)
               (args (?x))
               (sem-cat (sem-class (physobj inanimate)))
               (syn-cat (lex-cat noun)
                        (number singular)
                        (case ?case)))
              <-
              (?cake-word
               (HASH meaning ((cake ?x)))                     
               --
               (HASH form ((string ?cake-word  "cake"))))))

(def-fcg-cxn her-cxn
             ((?her-word
               (head ?her-word)
               (referent ?x)
               (args (?x))
               (sem-cat (sem-class (physobj animate feminine))
                        (sem-fun referring))
               (syn-cat (lex-cat pronoun)
                        (phrasal-cat NP)
                        (case not-nominative)))
              <-
              (?her-word
               (HASH meaning ((person female ?x)))                     
               --
               (HASH form ((string ?her-word  "her"))))))

(def-fcg-cxn he-cxn
             ((?he-word
               (referent ?x)
               (head ?he-word)
               (args (?x))
               (sem-cat (sem-class (physobj animate masculine))
                        (sem-fun referring))
               (syn-cat (lex-cat pronoun)
                        (phrasal-cat NP)
                        (case nominative)
                        (number singular)))
              <-
              (?he-word
               (HASH meaning ((person male ?x)))                     
               --
               (HASH form ((string ?he-word  "he"))))))

(def-fcg-cxn bakes-cxn
             ((?bakes-unit
               (referent ?x)
               (args (?x ?baker ?baked))
               (head ?bakes-unit)
               (sem-cat (sem-class (event))
                        (sem-fun predicating)
                        (frame (actor ?baker)
                               (undergoer ?baked)))
               (syn-cat (lex-cat verb)
                        (syn-valence (subject ?subj)
                                     (direct-object ?dir-obj))
                        (number singular)))
              <-
              (?bakes-unit
               (HASH meaning ((action bake ?x) (baker ?x ?baker) (baked ?x ?baked)))
               --
               (HASH form ((string ?bakes-unit "bakes"))))))

(def-fcg-cxn ditransitive-cxn
             ((?ditransitive-clause
               (subunits
                (?subject-unit ?verb-unit ?indirect-object-unit ?direct-object-unit)))
              (?subject-unit
               (head ?head-subject-unit) 
               (syn-cat
                (syn-fun (subject ?transitive-clause))))
              (?direct-object-unit
               (head ?head-direct-object-unit)
               (syn-cat 
                (syn-fun (direct-object ?transitive-clause))))
              (?indirect-object-unit
               (head ?head-indirect-object-unit)
               (syn-cat
                (syn-fun (indirect-object ?transitive-clause))))
              (?verb-unit
               (sem-cat (frame (receiver ?receiver)))
               (dependents ((subject ?head-subject-unit) (indirect-object ?head-indirect-object-unit)
                            (direct-object ?head-direct-object-unit)))
               (syn-cat (syn-valence (indirect-object ?indirect-object-unit))))

              <-
              (?ditransitive-clause
               --
               (HASH form ((meets ?subject-unit ?verb-unit ?ditransitive-clause)
                           (meets ?verb-unit ?indirect-object-unit ?ditransitive-clause)
                           (meets ?indirect-object-unit ?direct-object-unit ?ditransitive-clause))))
              (?verb-unit 
               (HASH meaning
                     ((action cause-receive ?event) (causer ?event ?causer)
                      (transferred ?event ?transferred) (receiver ?event ?receiver)))
               (referent ?event)
               (sem-cat (sem-fun predicating)
                        (frame (actor ?causer)
                               (undergoer ?transferred)))
               --
               (syn-cat (lex-cat verb)
                        (syn-valence (subject ?subject-unit)
                                     (direct-object ?direct-object-unit))
                        (number ?number)))
              (?subject-unit
               (referent ?causer)
               (sem-cat (sem-fun referring))
               --
               (sem-cat (sem-class (animate)))
               (syn-cat (phrasal-cat NP)
                        (case nominative)
                        (number ?number)))
               
              (?direct-object-unit
               (referent ?transferred)
               (sem-cat (sem-fun referring)
                        (sem-class (physobj)))
               --
               (sem-cat (sem-class (inanimate)))
               (syn-cat (phrasal-cat NP)
                        (case not-nominative)))

              (?indirect-object-unit
               (referent ?receiver)
               (sem-cat (sem-fun referring))
               --
               (sem-cat (sem-class (animate)))
               (syn-cat (phrasal-cat NP)
                        (case not-nominative)))))

(def-fcg-cxn simple-np-cxn
             ((?np-unit
               (head ?head-of-noun)
               (referent ?x)
               (sem-cat (sem-fun referring)
                        (sem-class ?sem-class)) ;;percolating sem-class
               (syn-cat (phrasal-cat NP)
                        (number ?number)
                        (case ?case)) ;;adding case
               (subunits (?art-unit ?noun-unit)))
              (?art-unit
               (syn-cat (syn-fun (determiner ?noun-unit))))
              (?noun-unit
               (head ?head-of-noun)
               (syn-cat (syn-fun (head ?np-unit)))
               (dependents 
                ((determiner ?art-unit))))
              <-
              (?np-unit
               --
               (HASH form ((meets ?art-unit ?noun-unit ?np-unit))))
              (?art-unit
               (args (?x))
               --
               (syn-cat (lex-cat article)
                        (number ?number)))
              (?noun-unit
               (args (?x))
               (sem-cat (sem-class ?sem-class))
               --
               (syn-cat (lex-cat noun)
                        (number ?number)
                        (case ?case))))))

(defun display-constructions-ditransitive (cxn-inventory)
  (add-element '((h2) ((a :name  "ditransitive") "IV. A ditransitive clause construction.")))
  (add-element '((p) "This illustrates a ditransitive clause using the utterance `he bakes her a cake'. The example is noteworthy because the construction schema for the ditransitive construction adds more meaning to the meaning supplied by the verb. The example is also interesting because it shows how we can add a dependency grammar perspective alongside the constituent structure and functional viewpoint."))
  (add-element '((p) "In addition to the previous lexical constructions, we need one lexical construction for `her':"))
  (add-element (make-html (find-cxn 'her-cxn cxn-inventory))) ; her
  (add-element '((p) "And then the ditransitive construction."))
  (add-element (make-html (find-cxn 'ditransitive-cxn cxn-inventory))))

(defun parse-example-he-bakes-her-a-cake (inventory)
  (add-element '((h3) "IVa.Parsing a ditransitive clause: 'He bakes her a cake'."))
  (comprehend '("he" "bakes" "her" "a" "cake") 
         :cxn-inventory inventory)
  (add-element '((p) "You can observe that new meaning has been added to the verb by inspecting the meaning feature of the bakes unit."))
  ;(add-element '((p) "When clicking on the bar above the resulting transient structure, it is possible to choose two viewpoints: h1 which is the constituent structure viewpoint and h2 which is the dependency network viewpoint."))
  )

(defun produce-example-he-bakes-her-a-cake (inventory)
  (add-element '((h3) "IVb.Producing a ditransitive clause: 'He bakes her a cake'."))
  (formulate
   '((specificity indefinite cake-1) (cake cake-1) (action cause-receive event-1) (causer event-1 obj-1) (transferred event-1 cake-1) 
     (receiver event-1 obj-2) (action bake event-1) (baker event-1 obj-1) (baked event-1 cake-1) (person female obj-2) (person male obj-1))
   :cxn-inventory inventory))

(defun ditransitive-example () 
  (let ((inventory (make-basic-grammar-4-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-ditransitive inventory)
   (parse-example-he-bakes-her-a-cake inventory)
   (produce-example-he-bakes-her-a-cake inventory)))

;test (progn (clear-page) (ditransitive-example))

(defun total-demo ()
  (my-head-menu)
  ; simple NP: 'the girl'
  (simple-np-example)
  ; intransitive: 'he bakes'
  (intransitive-example)
  ; transitive: 'he bakes a cake'
  (transitive-example)
  ; ditransitive: 'he bakes her a cake'
  (ditransitive-example))

; (total-demo)

(defun create-web-page () 
  (create-static-html-page
      "Basics of Fluid Construction Grammar"
    (clear-page)
    (total-demo)))

;;(create-web-page)

