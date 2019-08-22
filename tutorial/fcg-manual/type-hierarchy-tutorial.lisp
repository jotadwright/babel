;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This File contains a tutorial for using type-hierarchies in FCG's match and merge ;;
;; File by Paul - 01/2017                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :type-hierarchies)
(in-package :type-hierarchies)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;
;; 1. Introduction ;;
;;;;;;;;;;;;;;;;;;;;;

;; They type-hierarchy package is an extension to FCG which makes it possible to represent
;; hierarchies of values (categories), to build these hierarchies up, and to use them in
;; FCG processing (match and merge).
;;
;; Say, we have two units for the words 'man' and 'cat' with the features (sem-class human)
;; and (sem-class animal) respectively. However, our NP-cxn is not that specific, it is happy
;; to combine a determiner with any noun with (sem-class physical-object). The type-hierarchy
;; extension allows to declare that animal and human are subtypes of physical-object. The FCG
;; processing engine will then look up these relations in the type-hierarchy and use them in
;; matching and merging.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. An example grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We use the macro def-fcg-constructions-with-type-hierarchy for constructing a cxn-inventory
;; that can be used by the type-hierarchy extension. Besides this, the macro is entirely the
;; same as the standard def-fcg-constructions macro.

;; The cxn-inventory holds four constructions:
;; - A lexical construction for the determiner the
;; - A grammatical construction corresponding to NP <- Determiner Noun, the noun should have
;;   (lex-class noun) and (sem-class physical-object)
;; - A lexical construction for cat, being of (lex-class count-noun) and (sem-class animal)
;; - A lexical construction for grass, being of (lex-class mass-noun) and (sem-class plant)

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

;; When comprehending of formulating with standard FCG and this grammar, the NP cxn can never apply. This is because
;; the lex-class and sem-class of the nouns do not correspond with the one requested in the NP-cxn

(comprehend '("the" "cat"))
(comprehend '("the" "grass"))
(formulate '((unique o-1) (cat o-1)))
(formulate '((unique o-2) (grass o-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Specifying the type hierarchy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The type-hierarchy is stored in the data field (blackboard) of the cxn-inventory
;; under the key :type-hierarchy. It can easily be accessed with (get-type-hierarchy cxn-inventory)
;; categories and links can be added with the add-link, add-categories and add-category functions.

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

;; The type-hierarchies can be visualised in the web-interface
(add-element (make-html (get-type-hierarchy *fcg-constructions*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Using the type hierarchy      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When specified, the type hierarchy is used automatically. We can see that the values of
;; lex-class and sem-class in the transient structure now match with those in the NP-cxn,
;; In merging, the values from the transient structure are retained.

(comprehend '("the" "cat"))
(comprehend '("the" "grass"))
(formulate '((unique o-1) (cat o-1)))
(formulate '((unique o-2) (grass o-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using weighted edges ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The grammar

(def-fcg-constructions-with-type-hierarchy type-hierarchy-weights-example-grammar
  :feature-types ((args sequence)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set))
  
  (def-fcg-cxn ham-cheese-sandwich-cxn
               ((?ham-cheese-sandwich-unit
                 (syn-cat (meal-type sandwich))
                 (args (?x)))
                <-
                (?ham-cheese-sandwich-unit
                 (HASH meaning ((object ?x ham-cheese-sandwich)))
                 --
                 (HASH form ((string ?ham-cheese-sandwich-unit "ham-cheese-sandwich")))))
               :description "Lexical unit for the dish ham-chees-sandwich.")

  (def-fcg-cxn steak-bearnaise-cxn
               ((?steak-bearnaise-unit
                 (syn-cat (meal-type cooked-meal))
                 (args (?x)))
                <-
                (?steak-bearnaise-unit
                 (HASH meaning ((object ?x steak-bearnaise)))
                 --
                 (HASH form ((string ?steak-bearnaise-unit "steak-bearnaise")))))
               :description "Lexical unit for the dish steak-bearnaise.")

  (def-fcg-cxn day-menu-cxn
               ((?day-menu-unit
                 (args (noon evening))
                 (syn-cat (menu-type day-menu))
                 (subunits (?noon-meal-unit ?evening-meal-unit)))
                <-
                (?noon-meal-unit
                 (args (noon))
                 (syn-cat (meal-type noon-meal))
                 --
                 (syn-cat (meal-type noon-meal)))
                (?evening-meal-unit
                 (args (evening))
                 (syn-cat (meal-type evening-meal))
                 --
                 (syn-cat (meal-type evening-meal)))
                (?day-menu-unit
                 --
                 (HASH form ((meets ?noon-meal-unit ?evening-meal-unit)))))
               :description "Construction for a day-menu.")
  )

 

;; Setting the type hierarchy

(let ((th (get-type-hierarchy *fcg-constructions*)))
  (add-categories '(sandwich cooked-meal noon-meal evening-meal) th)
  (add-link 'sandwich 'noon-meal th :weight 0.3)
  (add-link 'sandwich 'evening-meal th :weight 0.7)
  (add-link 'cooked-meal 'noon-meal th :weight 0.8)
  (add-link 'cooked-meal 'evening-meal th :weight 0.2)
  th)

;; Visualizing the type hierarchy with weights

(add-element (make-html (get-type-hierarchy *fcg-constructions*) :weights? t))

;; Changing the weights on the edges

(link-weight  'sandwich 'cooked-meal (get-type-hierarchy *fcg-constructions*))
(set-link-weight 'sandwich 'cooked-meal (get-type-hierarchy *fcg-constructions*)  0.7)
(incf-link-weight 'sandwich 'cooked-meal (get-type-hierarchy *fcg-constructions*) 0.1)
(decf-link-weight 'sandwich 'cooked-meal (get-type-hierarchy *fcg-constructions*) 0.1)

;; Paths and distances
(directed-path-p 'cooked-meal 'noon-meal (get-type-hierarchy *fcg-constructions*))
(directed-distance  'cooked-meal 'noon-meal (get-type-hierarchy *fcg-constructions*))

;; Comprehending and formulating

(comprehend-all "steak-bearnaise ham-cheese-sandwich")
(formulate '((object ?x ham-cheese-sandwich) (object ?y steak-bearnaise)))
