
;;Special issue Thomas Hoffmann, Creativity
;;Contribution by Paul Van Eecke and Katrien Beuls
;;###################################################

;; (ql:quickload :type-hierarchies)
(in-package :type-hierarchies)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Exploring the creative potential of computional construction grammar"))
  (add-element '((h3) "Paul Van Eecke &amp; Katrien Beuls"))
  (add-element '((p) "This web demo accompanies the paper:" ((br)) 
                 "Van Eecke, Paul &amp; Beuls, Katrien. (submitted). Exploring the creative potential of computional construction grammar."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "1.  Introduction")))
  (add-element '((p)  ((a :href "#WD-2") "2.  Creativity through the free combination of constructions")))
  (add-element '((p)  ((a :href "#WD-3") "3.  Creativity through the appropriate violation of usual constraints")))
  (add-element '((p)  ((a :href "#WD-4") "4.  References")))
  (add-element '((hr))))

(defun introduction ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "1.  Introduction"))
  (add-element '((p) "This interactive web demonstration shows in full detail the examples used in the paper " ((i)"Exploring the creative potential of computational construction grammar") ". In this paper, two types of creativity are adressed. The first type concerns creativity that results from the free combination of the constructions in the grammar, and the second type concerns creative language use that cannot be accounted for by the existing constructions of the grammar, but for which novel constructions need to be created. The first type of creativity was illustrated by the resultative utterance " ((i) "Firefighters cut the man free") ", as illustrated in paragraph 2 of this web demo. The second type was illustrated by the generalisation of the idiomatic " ((i) "not-the-sharpest-tool-in-the-box") "-construction into the more general" ((i) " not-the-x-est-y-in-the-z") "-construction, as shown in paragraph 3 below." ))
  (add-element '((hr))))

(defun free-combination ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "2.  The free combination of constructions"))
  (add-element '((p) "Goldberg (2006: 22) writes that \"the infinitely creative potential of language\" is accounted for in construction grammar by \"allowing constructions to combine freely as long as there are no conflicts\". This is exactly how constructional processing is implemented in Fluid Construction Grammar. We will now show the concrete representations and mechanisms that are involved, using the example utterance '" ((i) "Firefighters cut the man free.") "'. This utterance is used as an example of creative language use through the free combination of constructions by Hoffmann (this volume). It exhibits two interesting phenomena. First, the word " ((i) "firefighters ") "fills the slot of both the argument role 'agent' in the resultative construction and of the 'cutter' in the verb-specific cut-construction. Second, the cut.object argument role of the verb-specific cut-construction is left unexpressed (definite null instantiation)."))
  (add-element '((p) "In order to see the constructions that are involved in the processing of this utterance, click on FCG CONSTRUCTION SET (7) below. This will show the constructions as blue boxes. Click one of the boxes to expand the construction, and then click the encircled + to reveal all features in the construction. The construction application process, shown in green can also be expanded into the individual construction applications, which can be expanded again to show the transient structure before construction application, the applied construction, and the transient structure after construction application. The meaning that the analysis attributes to the utterance is shown at the bottom."))
(add-element '((p) "The example clearly shows how the different constructions fit together to process the utterance. The resultative-cxn is conditioned on the activation of the firefighter-cxn as well as the plual-n-cxn for its subject unit. Its oblique unit is filled by the unit introduced by the definite-np-cxn, which in turn depends on the man-sg-cxn. FCG implements constructional language processing in a bidirectional way, in the sense that the same constructions and processing mechanisms are used for both comprehending and producing utterances. Below, the processing of the example utterance is shown in comprehension first, and in production underneath."))
  
  (make-hoffmann-grammar-cxns)
  (with-activated-monitor trace-fcg (comprehend "firefighter -s cut the man free"))
  (with-activated-monitor trace-fcg (formulate '((MAN REF) (DEFINITE REF) (FREE REF)
                                                 (CUT.OBJECT CUT-EVENT CUT-OBJECT)
                                                 (CUTTER CUT-EVENT CUTTER)
                                                 (EVENT-FRAME CUT CUT-EVENT)
                                                 (RESULTATIVE-PATIENT CUT-EVENT REF)
                                                 (RESULTATIVE-AGENT CUT-EVENT CUTTER)
                                                 (RESULTATIVE-STATE CUT-EVENT REF)
                                                 (EVENT-FRAME RESULTATIVE CUT-EVENT)
                                                 (FIREFIGHTER CUTTER))))
  (add-element '((hr))))

(defun constraint-violation ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "3.  The appriopriate violation of usual constraints"))
  (add-element '((p) "The free combination of the existing constructions of the grammar can only account for a very small part of the creativity observed in human language use. Additional mechanisms are needed to invent novel constructions, for example by changing one or more constraints in an existing construction. This kind of creativity is crucial for a language to emerge and evolve. The following example shows how the idiomatic 'not-the-sharpest-tool-in-the-box'-cxn can be generalised to the 'not-the-x-est-y-in-the-z'-cxn by relaxing its constraints on the semantic field of the metaphor and on the specific words that are used. All the other constraints in the construction remain in place."))
  (add-element '((p) "The following example shows the comprehension process of the utterance 'he's not the sharpest tool in the box', which can be processed by the existing constructions of the grammar. The resulting meaning indicates that a male person is not smart, as conveyed by a metaphorical expression in the semantic field of hardware."))
  (with-activated-monitor trace-fcg (comprehend "he 's not the sharpest tool in the box" :cxn-inventory *the-sharpest-tool-in-the-box*))
  (add-element '((hr)))
  (add-element '((p) "Now, we want to comprehend the utterance 'he's not the quickest bunny in the forest'. This utterance cannot be processed by the existing constructions of the grammar, and requires the generalization of the 'not-the-sharpest-tool-in-the-box'-cxn into the 'not-the-x-est-y-in-the-z'-cxn. The original construction is shown on top, and the novel, generalised construction below. Click the encircled plus to expand all features in the construction. Note that the constraints on the specific lex-ids of the words sharp, tool and box have disappeared. Moreover, the semantic field of these words and of the predicate itself is not required to be hardware anymore, but it is still required that the semantic field is the shared among these words. The generalization of a construction is performed automatically using FCG's build-in anti-unification algorithm, but the constructions below have been adapted to use symbols that are easier to read by humans, than the ones generated by the algorithm. The comprehension process of the utterance 'he's not the quickest bunny in the forest', using the generalised 'not-the-x-est-y-in-the-z'-cxn, is shown below. The resulting meaning is the same as in the original construction, but indicates now that the semantic field of the metaphor is 'animals'."))
  (add-element '((hr)))
  (add-element (make-html (find-cxn 'not-the-sharpest-tool-in-the-box-cxn *the-sharpest-tool-in-the-box* :expand-initially t)))
  (add-element (make-html (find-cxn 'not-the-x-est-y-in-the-z-cxn *the-x-est-y-in-the-z* :expand-initially t)))
  (with-activated-monitor trace-fcg (comprehend "he 's not the quickest bunny in the forest" :cxn-inventory *the-x-est-y-in-the-z*))
  (add-element '((hr)))
  (add-element '((p) "In formulation, the generalized construction can  be triggered by changing the semantic field of the metaphor in the meaning representation. FCG will then freely combine the constructions of the grammar and find a solution that satisfies both the semantic field of the metaphor and all the other constraints in the construction, e.g. that x, y and z need to be of the same semantic field, that x needs to be a positive property expressed as a superlative, that y needs to be some kind of object and that z needs to be some kind of container. By setting the semantic field to clothing and food respectively, we get the following two interesting, creative utterances."))
  (add-element '((hr)))
  (add-element '((p) "For the semantic field of clothing:"))
  (with-activated-monitor trace-fcg (formulate '((male-person x)
                                                 (property not-smart x)
                                                 (metaphorical-expression metaphor x)
                                                 (semantic-field metaphor clothing)) :cxn-inventory *the-x-est-y-in-the-z*))
  (add-element '((hr)))
  (add-element '((p) "For the semantic field of food:"))
  (with-activated-monitor trace-fcg (formulate '((male-person x)
                                                 (property not-smart x)
                                                 (metaphorical-expression metaphor x)
                                                 (semantic-field metaphor food)) :cxn-inventory *the-x-est-y-in-the-z*))
  (add-element '((hr))))

(defun references ()
  (add-element '((a :name "WD-4")))
  (add-element '((h2) "4.  References"))
  (add-element '((p) "Hoffmann, T. (submitted, this volume). Grammar and Creativity: Cognitive and Psychological Issues."))
  (add-element '((p) "Goldberg, A. (2006). " ((i) "Constructions at Work. The Nature of Generalization in Language.") " Oxford: University Press."))
  (add-element '((hr))))

(defun make-demo ()
  (create-static-html-page "Creativity & computational construction grammar"
    (header)
    (introduction)
    (free-combination)
    (constraint-violation)
    (references)
    )
  )

;; (make-demo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Grammar for 'firefighters cut the man free' ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions hoffmann-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network))

  (def-fcg-cxn resultative-cxn
               ((?verb-unit
                 (subunits (?subject-unit ?object-unit ?oblique-unit)))
                <-
                (?subject-unit
                 (referent ?agent)
                 --
                 (boundaries (leftmost-unit ?leftmost-subject-unit)
                             (rightmost-unit ?rightmost-subject-unit))
                 (syn-cat (lex-class noun) ;;np
                          (agreement (person ?person)
                                     (number ?number))))
                (?verb-unit
                 (sem-valence (agent ?agent))
                 (referent ?resultative-event)
                 (HASH meaning ((event-frame resultative ?resultative-event)
                                (resultative-agent ?resultative-event ?agent)
                                (resultative-patient ?resultative-event ?patient)
                                (resultative-state ?resultative-event ?result)))
                 --
                 (syn-cat (lex-class verb)
                          (agreement (person ?person)
                                     (number ?number)))
                 (HASH form ((meets ?rightmost-subject-unit ?verb-unit)
                             (meets ?verb-unit ?leftmost-object-unit)
                             (meets ?rightmost-object-unit ?oblique-unit))))
                (?object-unit
                 (referent ?patient)
                 --
                 (boundaries (leftmost-unit ?leftmost-object-unit)
                             (rightmost-unit ?rightmost-object-unit))
                 (syn-cat (phrase-type np)))
                (?oblique-unit
                 (referent ?result)
                 (sem-cat (sem-class modifier))
                 --
                 (syn-cat (lex-class adjective))))
               :description "Agent causes Patient to become State by V-ing")

  (def-fcg-cxn cut-cxn
               ((?cut-unit
                 (syn-cat (lex-class verb)
                          (agreement (person ?p)
                                     (number ?nb)))
                 (args (?cut-event ?cutter ?cut-object))
                 (referent ?cut-event)
                 (syn-valence (subject ?subj)
                              (direct-object ?dir-obj))
                 (sem-valence (agent ?cutter)
                              (undergoer ?cut-object)))
                <-
                (?cut-unit
                 (HASH meaning ((event-frame cut ?cut-event)
                                (cutter ?cut-event ?cutter)
                                (cut.object ?cut-event ?cut-object)))
                 --
                 (HASH form ((string ?cut-unit "cut")))))
               :cxn-set lex)

  (def-fcg-cxn firefighter-cxn
               ((?firefighter-unit
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (agreement (person 3)
                                     (number ?nb))))
                <-
                (?firefighter-unit
                 (HASH meaning ((firefighter ?ref)))
                 --
                 (HASH form ((string ?firefighter-unit "firefighter")))
                 ))
               :cxn-set lex)

  (def-fcg-cxn plural-n-cxn
               ((?noun-unit
                 (subunits (?suffix-unit))
                 (boundaries (leftmost-unit ?noun-unit)
                             (rightmost-unit ?suffix-unit)))
                <-
                (?noun-unit
                 (syn-cat (lex-class noun)
                          (agreement (person 3)
                                     (number pl)))
                 --
                 (syn-cat (lex-class noun)))
                (?suffix-unit
                 --
                 (HASH form ((string ?suffix-unit "-s")
                             (meets ?noun-unit ?suffix-unit)))))
               :cxn-set lex)

  (def-fcg-cxn definite-np-cxn
               ((?np-unit
                 (referent ?ref)
                 (sem-cat (sem-function referring-expression))
                 (syn-cat (phrase-type np)
                          (agreement (person ?person)
                                     (number ?number)))
                 (boundaries (leftmost-unit ?art-unit)
                             (rightmost-unit ?noun-unit))
                 (subunits (?art-unit ?noun-unit)))
                <-
                (?art-unit
                 (HASH meaning ((definite ?ref)))
                 --
                 (HASH form ((string ?art-unit "the"))))
                (?noun-unit
                 (referent ?ref)
                 (syn-cat (lex-class noun))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (person ?person)
                                     (number ?number))))
                (?np-unit
                 --
                 (HASH form ((meets ?art-unit ?noun-unit))))))

  (def-fcg-cxn man-sg-cxn
               ((?man-unit
                 (referent ?ref)
                 (syn-cat (lex-class noun)
                          (agreement (person 3)
                                     (number sg))))
                <-
                (?man-unit
                 (HASH meaning ((man ?ref)))
                 --
                 (HASH form ((string ?man-unit "man")))))
               :cxn-set lex)

  (def-fcg-cxn free-cxn
               ((?free-unit
                 (referent ?ref)
                 (syn-cat (lex-class adjective))
                 (sem-cat (sem-class modifier)))
                <-
                (?free-unit
                 (HASH meaning ((free ?ref)))
                 --
                 (HASH form ((string ?free-unit "free")))))
               :cxn-set lex))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Grammar for 'the-sharpest-tool-in-the-box'  ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *type-hierarchy* (make-instance 'type-hierarchy))

  (add-categories '(sharp tool box quick bunny forest smart suit wardrobe crunchy chip bag) *type-hierarchy*)
  (add-categories '(object container positive-property) *type-hierarchy*)

  (add-link 'tool 'object *type-hierarchy*)
  (add-link 'bunny 'object *type-hierarchy*)
  (add-link 'suit 'object *type-hierarchy*)
  (add-link 'chip 'object *type-hierarchy*)

  (add-link 'box 'container *type-hierarchy*)
  (add-link 'forest 'container *type-hierarchy*)
  (add-link 'wardrobe 'container *type-hierarchy*)
  (add-link 'bag 'container *type-hierarchy*)
  
  (add-link 'sharp 'positive-property *type-hierarchy*)
  (add-link 'quick 'positive-property *type-hierarchy*)
  (add-link 'smart 'positive-property *type-hierarchy*)
  (add-link 'crunchy 'positive-property *type-hierarchy*)

  (add-link 'object 'tool *type-hierarchy*)
  (add-link 'object 'bunny *type-hierarchy*)
  (add-link 'object 'suit *type-hierarchy*)
  (add-link 'object 'chip *type-hierarchy*)

  (add-link 'container 'box *type-hierarchy*)
  (add-link 'container 'forest *type-hierarchy*)
  (add-link 'container 'wardrobe *type-hierarchy*)
  (add-link 'container 'bag *type-hierarchy*)

  (add-link 'positive-property 'sharp *type-hierarchy*)
  (add-link 'positive-property 'quick *type-hierarchy*)
  (add-link 'positive-property 'smart *type-hierarchy*)
  (add-link 'positive-property 'crunchy *type-hierarchy*))


(def-fcg-constructions-with-type-hierarchy the-sharpest-tool-in-the-box
  :cxn-inventory *the-sharpest-tool-in-the-box*
  :type-hierarchy *type-hierarchy*
  (def-fcg-cxn he-cxn
               ((?he-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 (syn-cat (lex-class pronoun)))
                <-
                (?he-unit
                 (HASH meaning ((male-person ?x)))
                 --
                 (HASH form ((string ?he-unit "he"))))))

  (def-fcg-cxn copula-cxn
               ((?clause-unit
                 (sem-cat (sem-function proposition))
                 (syn-cat (lex-class clause))
                 (subunits (?subject-unit ?copula-unit ?predicate-unit)))
                <-
                (?subject-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 --
                 (syn-cat (lex-class pronoun)))
                (?copula-unit
                 --
                 (HASH form ((string ?copula-unit "'s"))))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (left-most-unit ?left-most-unit)
                 --
                 (syn-cat (phrase-type predicate)))
                (?clause-unit
                 --
                 (HASH form ((meets ?subject-unit ?copula-unit)
                             (meets ?copula-unit ?left-most-unit))))))

  (def-fcg-cxn sharpest-cxn
               ((?sharpest-unit
                 (syn-cat (lex-id sharp)))
                <-
                (?sharpest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type sharp)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?sharpest-unit "sharpest"))))))

  (def-fcg-cxn quickest-cxn
               ((?quickest-unit
                 (syn-cat (lex-id quick)))
                <-
                (?quickest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type quick)
                          (sem-field animals))
                 --
                 (HASH form ((string ?quickest-unit "quickest"))))))

  (def-fcg-cxn smartest-cxn
               ((?smartest-unit
                 (syn-cat (lex-id smart)))
                <-
                (?smartest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type smart)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?smartest-unit "smartest"))))))

  (def-fcg-cxn crunchiest-cxn
               ((?crunchiest-unit
                 (syn-cat (lex-id crunchy)))
                <-
                (?crunchiest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type crunchy)
                          (sem-field food))
                 --
                 (HASH form ((string ?crunchiest-unit "crunchiest"))))))

  (def-fcg-cxn tool-cxn
               ((?tool-unit
                 (syn-cat (lex-id tool)))
                <-
                (?tool-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type tool)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?tool-unit "tool"))))))

  (def-fcg-cxn bunny-cxn
               ((?bunny-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?bunny-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type bunny)
                          (sem-field animals))
                 --
                 (HASH form ((string ?bunny-unit "bunny"))))))

  (def-fcg-cxn suit-cxn
               ((?suit-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?suit-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type suit)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?suit-unit "suit"))))))

  (def-fcg-cxn chip-cxn
               ((?chip-unit
                 (syn-cat (lex-id chip)))
                <-
                (?chip-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type chip)
                          (sem-field food))
                 --
                 (HASH form ((string ?chip-unit "chip"))))))

  (def-fcg-cxn box-cxn
               ((?box-unit
                 (syn-cat (lex-id box)))
                <-
                (?box-unit
                 (sem-cat (sem-class object)
                          (sem-type box)
                          (sem-field hardware))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?box-unit "box"))))))

  (def-fcg-cxn forest-cxn
               ((?forest-unit
                 (syn-cat (lex-id forest)))
                <-
                (?forest-unit
                 (sem-cat (sem-class object)
                          (sem-type forest)
                          (sem-field animals))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?forest-unit "forest"))))))

  (def-fcg-cxn wardrobe-cxn
               ((?wardrobe-unit
                 (syn-cat (lex-id wardrobe)))
                <-
                (?wardrobe-unit
                 (sem-cat (sem-class object)
                          (sem-type wardrobe)
                          (sem-field clothing))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?wardrobe-unit "wardrobe"))))))

  (def-fcg-cxn bag-cxn
               ((?bag-unit
                 (syn-cat (lex-id bag)))
                <-
                (?bag-unit
                 (sem-cat (sem-class object)
                          (sem-type bag)
                          (sem-field food))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?bag-unit "bag"))))))
 
  (def-fcg-cxn the-cxn
               ((?the-unit
                 (syn-cat (lex-class article)
                          (lex-id the)))
                <-
                (?the-unit
                 (sem-cat (sem-function determiner))
                 --
                 (HASH form ((string ?the-unit "the"))))))

  (def-fcg-cxn in-cxn
               ((?in-unit
                 (syn-cat (lex-class in-preposition)
                          (lex-id in)))
                <-
                (?in-unit
                 (sem-cat (sem-class relation))
                 --
                 (HASH form ((string ?in-unit "in"))))))

  (def-fcg-cxn not-cxn
               ((?not-unit
                 (syn-cat (lex-class not-negation)
                          (lex-id not)))
                <-
                (?not-unit
                 (sem-cat (sem-function negation))
                 --
                 (HASH form ((string ?not-unit "not"))))))

  (def-fcg-cxn not-the-sharpest-tool-in-the-box-cxn
               ((?not-unit
                 (sem-cat (sem-function negation)))
                (?the-1-unit
                 (sem-cat (sem-function determiner)))
                (?sharpest-unit
                 (sem-cat (sem-class modifier)
                          (sem-type positive-property)
                          (sem-field hardware)))
                (?tool-unit
                 (subunits (?sharpest-unit ?the-1-unit))
                 (sem-cat (sem-class object)
                          (sem-type object)
                          (sem-field hardware)))
                (?in-unit
                 (sem-cat (sem-class relation)))
                (?the-2-unit
                 (sem-cat (sem-function determiner)))
                (?box-unit
                 (sem-cat (sem-class object)
                          (sem-type container)
                          (sem-field hardware))
                 (subunits (?in-unit ?the-2-unit)))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (syn-cat (phrase-type predicate))
                 (subunits (?not-unit ?tool-unit ?box-unit))
                 (left-most-unit ?not-unit))
                <-
                (?not-unit
                 --
                 (syn-cat (lex-class not-negation)))
                (?the-1-unit
                 --
                 (syn-cat (lex-class article)))
                (?sharpest-unit
                 --
                 (syn-cat (lex-class adjective)
                          (lex-type superlative)
                          (lex-id sharp)))
                (?tool-unit
                 --
                 (syn-cat (lex-class noun)
                          (lex-id tool)))
                (?in-unit
                 --
                 (syn-cat (lex-class in-preposition)))
                (?the-2-unit
                 --
                 (syn-cat (lex-class article)))
                (?box-unit
                 --
                 (syn-cat (lex-class noun)
                          (lex-id box)))
                (?predicate-unit
                 (HASH meaning ((property not-smart ?x)
                                (metaphorical-expression ?metaphor ?x)
                                (semantic-field ?metaphor hardware)))
                 --
                 (HASH form ((meets ?not-unit ?the-1-unit)
                             (meets ?the-1-unit ?sharpest-unit)
                             (meets ?sharpest-unit ?tool-unit)
                             (meets ?tool-unit ?in-unit)
                             (meets ?in-unit ?the-2-unit)
                             (meets ?the-2-unit ?box-unit)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;; Grammar for 'the x-est-y-in-the-z'          ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(def-fcg-constructions-with-type-hierarchy the-x-est-y-in-the-z
  :cxn-inventory *the-x-est-y-in-the-z*
  :type-hierarchy *type-hierarchy*
  (def-fcg-cxn he-cxn
               ((?he-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 (syn-cat (lex-class pronoun)))
                <-
                (?he-unit
                 (HASH meaning ((male-person ?x)))
                 --
                 (HASH form ((string ?he-unit "he"))))))

  (def-fcg-cxn copula-cxn
               ((?clause-unit
                 (sem-cat (sem-function proposition))
                 (syn-cat (lex-class clause))
                 (subunits (?subject-unit ?copula-unit ?predicate-unit)))
                <-
                (?subject-unit
                 (args (?x))
                 (sem-cat (sem-function referent))
                 --
                 (syn-cat (lex-class pronoun)))
                (?copula-unit
                 --
                 (HASH form ((string ?copula-unit "'s"))))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (left-most-unit ?left-most-unit)
                 --
                 (syn-cat (phrase-type predicate)))
                (?clause-unit
                 --
                 (HASH form ((meets ?subject-unit ?copula-unit)
                             (meets ?copula-unit ?left-most-unit))))))

  (def-fcg-cxn sharpest-cxn
               ((?sharpest-unit
                 (syn-cat (lex-id sharp)))
                <-
                (?sharpest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type sharp)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?sharpest-unit "sharpest"))))))

  (def-fcg-cxn quickest-cxn
               ((?quickest-unit
                 (syn-cat (lex-id quick)))
                <-
                (?quickest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type quick)
                          (sem-field animals))
                 --
                 (HASH form ((string ?quickest-unit "quickest"))))))

  (def-fcg-cxn smartest-cxn
               ((?smartest-unit
                 (syn-cat (lex-id smart)))
                <-
                (?smartest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type smart)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?smartest-unit "smartest"))))))

  (def-fcg-cxn crunchiest-cxn
               ((?crunchiest-unit
                 (syn-cat (lex-id crunchy)))
                <-
                (?crunchiest-unit
                 (syn-cat (lex-class adjective)
                          (lex-type superlative))
                 (sem-cat (sem-class modifier)
                          (sem-type crunchy)
                          (sem-field food))
                 --
                 (HASH form ((string ?crunchiest-unit "crunchiest"))))))

  (def-fcg-cxn tool-cxn
               ((?tool-unit
                 (syn-cat (lex-id tool)))
                <-
                (?tool-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type tool)
                          (sem-field hardware))
                 --
                 (HASH form ((string ?tool-unit "tool"))))))

  (def-fcg-cxn bunny-cxn
               ((?bunny-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?bunny-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type bunny)
                          (sem-field animals))
                 --
                 (HASH form ((string ?bunny-unit "bunny"))))))

  (def-fcg-cxn suit-cxn
               ((?suit-unit
                 (syn-cat (lex-id bunny)))
                <-
                (?suit-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type suit)
                          (sem-field clothing))
                 --
                 (HASH form ((string ?suit-unit "suit"))))))

  (def-fcg-cxn chip-cxn
               ((?chip-unit
                 (syn-cat (lex-id chip)))
                <-
                (?chip-unit
                 (syn-cat (lex-class noun))
                 (sem-cat (sem-class object)
                          (sem-type chip)
                          (sem-field food))
                 --
                 (HASH form ((string ?chip-unit "chip"))))))

  (def-fcg-cxn box-cxn
               ((?box-unit
                 (syn-cat (lex-id box)))
                <-
                (?box-unit
                 (sem-cat (sem-class object)
                          (sem-type box)
                          (sem-field hardware))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?box-unit "box"))))))

  (def-fcg-cxn forest-cxn
               ((?forest-unit
                 (syn-cat (lex-id forest)))
                <-
                (?forest-unit
                 (sem-cat (sem-class object)
                          (sem-type forest)
                          (sem-field animals))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?forest-unit "forest"))))))

  (def-fcg-cxn wardrobe-cxn
               ((?wardrobe-unit
                 (syn-cat (lex-id wardrobe)))
                <-
                (?wardrobe-unit
                 (sem-cat (sem-class object)
                          (sem-type wardrobe)
                          (sem-field clothing))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?wardrobe-unit "wardrobe"))))))

  (def-fcg-cxn bag-cxn
               ((?bag-unit
                 (syn-cat (lex-id bag)))
                <-
                (?bag-unit
                 (sem-cat (sem-class object)
                          (sem-type bag)
                          (sem-field food))
                 (syn-cat (lex-class noun))
                 --
                 (HASH form ((string ?bag-unit "bag"))))))
  
  (def-fcg-cxn the-cxn
               ((?the-unit
                 (syn-cat (lex-class article)
                          (lex-id the)))
                <-
                (?the-unit
                 (sem-cat (sem-function determiner))
                 --
                 (HASH form ((string ?the-unit "the"))))))

  (def-fcg-cxn in-cxn
               ((?in-unit
                 (syn-cat (lex-class in-preposition)
                          (lex-id in)))
                <-
                (?in-unit
                 (sem-cat (sem-class relation))
                 --
                 (HASH form ((string ?in-unit "in"))))))

  (def-fcg-cxn not-cxn
               ((?not-unit
                 (syn-cat (lex-class not-negation)
                          (lex-id not)))
                <-
                (?not-unit
                 (sem-cat (sem-function negation))
                 --
                 (HASH form ((string ?not-unit "not"))))))

  (def-fcg-cxn not-the-x-est-y-in-the-z-cxn
               ((?not-unit
                 (sem-cat (sem-function negation)))
                (?the-1-unit
                 (sem-cat (sem-function determiner)))
                (?x-est-unit
                 (sem-cat (sem-class modifier)
                          (sem-type positive-property)
                          (sem-field ?sem-field)))
                (?y-unit
                 (subunits (?x-est-unit ?the-1-unit))
                 (sem-cat (sem-class object)
                          (sem-type object)
                          (sem-field ?sem-field)))
                (?in-unit
                 (sem-cat (sem-class relation)))
                (?the-2-unit
                 (sem-cat (sem-function determiner)))
                (?z-unit
                 (sem-cat (sem-class object)
                          (sem-type container)
                          (sem-field ?sem-field))
                 (subunits (?in-unit ?the-2-unit)))
                (?predicate-unit
                 (args (?x))
                 (sem-cat (sem-function property))
                 (syn-cat (phrase-type predicate))
                 (subunits (?not-unit ?y-unit ?z-unit))
                 (left-most-unit ?not-unit))
                <-
                (?not-unit
                 --
                 (syn-cat (lex-class not-negation)))
                (?the-1-unit
                 --
                 (syn-cat (lex-class article)))
                (?x-est-unit
                 --
                 (syn-cat (lex-class adjective)
                          (lex-type superlative)))
                (?y-unit
                 --
                 (syn-cat (lex-class noun)))
                (?in-unit
                 --
                 (syn-cat (lex-class in-preposition)))
                (?the-2-unit
                 --
                 (syn-cat (lex-class article)))
                (?z-unit
                 --
                 (syn-cat (lex-class noun)))
                (?predicate-unit
                 (HASH meaning ((property not-smart ?x)
                                (metaphorical-expression ?metaphor ?x)
                                (semantic-field ?metaphor ?sem-field)))
                 --
                 (HASH form ((meets ?not-unit ?the-1-unit)
                             (meets ?the-1-unit ?x-est-unit)
                             (meets ?x-est-unit ?y-unit)
                             (meets ?y-unit ?in-unit)
                             (meets ?in-unit ?the-2-unit)
                             (meets ?the-2-unit ?z-unit)))))))














