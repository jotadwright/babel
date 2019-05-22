;(asdf:operate 'asdf:load-op 'portuguese-grammar)

(in-package :fcg)

(defun my-head-menu ()
  (clear-page)
  (add-element
   '((h1) "A Computational Construction Grammar Approach to Pronominal Clitics in European Portuguese"))    
  (add-element '((p) "This file contains a series of examples discussed in the paper:"))
  (add-element '((p) "Marques, T. and K. Beuls (2016) A Construction Grammar Approach to Pronominal Clitics in European Portuguese. In Proceedings of the 12th International Conference on Computational Processing of the Portuguese language (pp. 239-244). Springer."))
  (add-element '((p :style "color:darkred") "DISCLAIMER: It is recommended to use Firefox or Safari to optimally explore the contents of this page."))
  (add-element '((p) "Fluid Construction Grammar is a bi-directional grammar formalism. For any sentence that you parse you get a semantic network (fully connected if the grammar has recognized the sentence) which can then in turn be used to produce sentences with exactly the same grammar. Or, reversely, you can start with a semantic network, produce it to an utterance and parse it back to a semantic network. This complete cycle of language processing is a key evaluation method for FCG grammars."))
  (add-element '((p) "The examples included in this demo all demonstrate the grammar evaluation cycle starting from parsing, over the semantic network, to production. The constructions used to build up the semantic network or the utterance are shared and thus work in two directions. Their order of application in parsing and production differs, however."))
  (add-element '((p) "You can click on all the boxes in the processing steps below. Clicking twice on a title in the construction application process opens the full application inside that node: the source transient structure and the resulting transient structure. You can then inspect how the construction in question did its job."))
  
  (add-element '((h3)  ((a :href "#focus") "1. Fronted focus ")))
  (add-element '((h3)  ((a :href "#wh-questions") "2. Wh questions ")))
  (add-element '((h3)  ((a :href "#adverbs") "3. Adverbs ")))
  (add-element '((h3)  ((a :href "#quantifiers") "4. Quantifiers ")))
  (add-element '((h3)  ((a :href "#subclauses") "5. Causal conjunction ")))
  )
;(my-head-menu)

(defun show-fronted-focus ()
  (deactivate-all-monitors)
  (add-element '((hr)))
  (let ((enclisis-cxn (find-cxn 'enclisis-cxn *propor-grammar*))
        (fronted-focus-cxn (find-cxn 'clausal-topic=object-cxn *propor-grammar*))
        (predicate-focus-cxn (find-cxn 'predicate-focus=subject-cxn *propor-grammar*))
        (proclisis-cxn (find-cxn 'proclisis-cxn *propor-grammar*)))
        
    (activate-monitor trace-fcg-light)
    (add-element '((a :name "focus")))
    (add-element '((h1) "1. Fronted focus"))
    (add-element '((p) "Clitics appear preverbally if there is another element than the subject in the first position of the sentence (topic). The topic of a sentence is not defined by prosodic stress but instead by a meaning predicate called 'topic' that is by default linked to the subject argument of the main verb. The examples in 1.1 and 1.2 only differ in terms of how the topic slot is filled. The first sentence is uttered with the standard SVO word order with the subject in the focus position and does not trigger proclisis. The second sentence puts focus on the object of the giving event and therefore triggers proclisis."))
  
    (add-element '((h2) "1.1. Topic = subject (enclisis)"))
    (add-element '((p) "The enclisis construction works on a clitic unit (lex-class pronoun) and a verb unit (lex-class verb). There are some additional constraints that the verb cannot be restricted (by a trigger cxn) or have a clitic yet. Moreover, in parsing there is an ordering condition that the verb unit must `meet' the clitic unit (clitic follows verb). In production, this construction will impose this order. Moreover, in both processing directions, it adds a (has-clitic +) feature to the syn-cat of the verb (see contributing part)."))
    (add-element '((p :style "color:red") "To inspect a construction, please click on the circle above the arrow."))
    (add-element (make-html enclisis-cxn :expand-initially t 
                            :wrap-in-paragraph t))
    (add-element '((p) "We now turn to the first example sentence, in which the above introduced enclisis-cxn is used in processing. It follows the active-ditransitive-cxn that has assigned case to the pronouns (clitics are never nominative) and linked the arguments to the verb. After that, the word order constructions assure that the direct object is placed in the predicate focus position (following the verb+clitic) and the subject is put in the first position of the clause (focus)."))
    (comprehend-and-formulate  '("eu" "dei" "te" "este" "livro") :cxn-inventory *propor-grammar*)
    (add-element '((hr)))
    (add-element '((h2) "1.2. Topic = non-subject (proclisis)"))
    (add-element '((p) "When the direct object is put in the topic position of the sentence, proclisis is triggered and the clitic has to precede the main verb. The subject then moves back in the sentence in the scope of the predicate focus, thus following the verb. Predicate focus is a well-known concept in linguistics (Van Valin and La Polla, 1997)."))
    (add-element '((p) "Two word order constructions are responsible for the positioning of the direct object and the subject: the first one makes clear what the clausal focus is (see meaning predicate focus(?ev ?ref)) and because it is not the subject (see the red feature in the ?verb-unit) it adds a (restricted +) feature and a pointer to the trigger unit, being the direct object unit to the verb unit (see contributing part, left). "))
    (add-element (make-html fronted-focus-cxn :expand-initially t 
                                      :wrap-in-paragraph t))
    (add-element '((p) "The second one indicates the predicate focus. The predicate focus typically concerns all the arguments of the main verb that follow it (in traditional formal theories of grammar this would be equal to the VP). The following construction looks for a ?verb-unit and a ?subject-unit and builds a grammatical dependency relation between them. In production, the predicate-focus meaning triggers the application of this construction, if it is the referent of the subject unit (bound to ?y) that is the second element of the predicate-focus argument list. In parsing, there is a requirement that the ?verb)unit precedes the ?subject-unit and the subject unit is linked to the syn-valence subject feature of the verb. Moreover, the subject has nominative case. "))
    (add-element (make-html predicate-focus-cxn :expand-initially t 
                                      :wrap-in-paragraph t))
    (add-element '((p) "The proclisis construction can only apply when the fronted-focus-cxn (see above) has done its work and added a (restricted +) feature to the verb unit in production. In parsing, there is the word order condition stating that the the clitic has to be adjacent to the verb ('meeting' it). In production, this form feature will be added to the transient structure so that it can be used in rendering. The verb unit gets a new grammatical dependent: the clitic unit and a new syntactic category (has-clitic +)."))
    
    (add-element (make-html proclisis-cxn :expand-initially t 
                                      :wrap-in-paragraph t))
    (add-element '((p :style "color:red") "Tip! You can inspect all steps in the application processes (comprehension/parsing and formulation/production) here below. Click twice on the green boxes to inspect what a construction did (compare source and resulting structures). "))
    (comprehend-and-formulate '("este" "livro" "te" "dei" "eu")
                              :cxn-inventory *propor-grammar*)))

;(clear-page)
;(show-fronted-focus)

(defun show-wh-questions ()
  (deactivate-all-monitors)
  (activate-monitor trace-fcg-light)
  (add-element '((hr)))
  (let ((wh-fronted-cxn (find-cxn 'wh-fronted-cxn *propor-grammar*))
        (wh-in-situ-cxn (find-cxn 'wh-in-situ-cxn *propor-grammar*)))
    (add-element '((a :name "wh-questions")))
    (add-element '((h1) "2. Wh Questions"))
    (add-element '((p) "Wh questions that start with a question word which is not the subject of the main verb (such as 'a quem', 'to whom') will automatically trigger proclisis through the constructions that were introduced in (1): clause-topic=object-cxn and predicate-focus=subject-cxn. Yet, in the case of WH questions, the pronominal clitic should always precede the verb, regardless of the position of the subject in the information structure. Therefore, two additional constructions were introduced to monitor the placement of the WH question word: wh-fronted-cxn and wh-in-situ-cxn."))
    (add-element '((p :style "color:red") "To inspect a construction, please click on the circle above the arrow."))
    (add-element '((p) "The wh-fronted-cxn replaces the topic construction and therefore also introduces a meaning predicate that introduces the topic of the clause (the question word). In parsing, it is assured that the question word comes in first position through the boundaries feature in the root unit. This feature contains absolute references to the boundaries of every word in the utterance."))
    (add-element (make-html wh-fronted-cxn :expand-initially t 
                            :wrap-in-paragraph t))
    (add-element '((p) "The wh-in-situ construction functions as a special predicate-focus construction, although it applies before the cliticization constructions do their job. ")) ;;TO DO: explain
    (add-element (make-html wh-in-situ-cxn :expand-initially t 
                            :wrap-in-paragraph t))
    (add-element '((h2) " 2.1. Wh questions trigger proclisis (even as subject)"))
    (add-element '((p) "The question 'who gave you the book' triggers proclisis because a question word precedes the main verb and the wh-fronted-cxn puts a (restricted +) feature in its syn-cat:"))
    (comprehend-and-formulate  '("quem" "te" "deu" "este" "livro" )
                               :cxn-inventory *propor-grammar*) ;;proclisis
    (add-element '((hr)))
    (add-element '((h3) "2.2 Enclisis with question word following the verb"))
    (add-element '((p) "This sentence is an example of enclisis with wh question. As you can see, the relative pronoun is in the end of the sentence: 'Tu deste o a quem?' ('You gave it to whom?'). This time, the enclisis-cxn can freely apply because there were no special trigger constructions active before. This time, the identified 'topic' in the meaning network is the giver of the event (close-to-origo, single)."))
    (add-element '((p :style "color:red") "Due to the syncretic nature of the 'o' word, being used as the pronoun it and the determiner the, there might be a split in the search trees that you will see below. Constructions are shuffled before application so sometimes the wrong reading of 'o' is selected first and the search will backtrack when it cannot find a satisfying solution for the sentence."))
    (comprehend-and-formulate '("tu" "deste" "o" "a_quem")
                              :cxn-inventory *propor-grammar*)
    (add-element '((hr)))

    (add-element '((h2) " 2.3. Proclisis with non-subject question word"))
    (add-element '((p) "The question 'To whom did you give it?' triggers proclisis because the question word does not correspond to the subject but the indirect object. The meaning network that is built by the comprehension process shows that the topic variable (second) is the same referent as the undefined referent to-whom, which is also identified as the givee."))
    (comprehend-and-formulate  '("a_quem" "o" "deste" "tu" )
                               :cxn-inventory *propor-grammar*) ;;proclisis
    ))
;(clear-page)
;(show-wh-questions)
  
(defun show-adverbs ()
  (deactivate-all-monitors)
  (activate-monitor trace-fcg-light)
  (add-element '((hr)))
  (let ((negation-adverb-precedes-verb-cxn (find-cxn 'negation-adverb-precedes-verb-cxn *propor-grammar*))
        (operator-adverb-precedes-verb-cxn (find-cxn 'operator-adverb-precedes-verb-cxn *propor-grammar*)))
    (add-element '((a :name "adverbs")))
    (add-element '((h1) "3. Adverbs"))
    (add-element '((p) "Some types of adverbs can also trigger proclisis. For example, negation operators come always before the verb and are always a proclisis trigger. There is also another class of triggers called operator-like adverbs by Luis and Otoguro 2011 that can activate procliss when they are present before the verb, while some others lack this characteristic. The construction negation-adverb-precedes-verb and operator-adverb-precedes-verb (you can open them below) are very similar. They both add a (restricted +) feature and a link to the trigger to the verb unit. Yet, the former looks for an adverb of the type negation (negation +), and the latter looks for an operator-like adverb (operator-like +). This is important because negation operators have always to appear before the verb, while operator-like adverbs can sometimes come postverbially too, which will not activate proclisis."))
    (add-element '((p :style "color:red") "To inspect a construction, please click on the circle above the arrow."))
    (add-element (make-html negation-adverb-precedes-verb-cxn :expand-initially t 
                            :wrap-in-paragraph t))
    (add-element (make-html operator-adverb-precedes-verb-cxn :expand-initially t 
                            :wrap-in-paragraph t))
    (add-element '((h2) " 3.1. Negation"))
    (add-element '((p) "In this first example, you can see the parsing and production of an example with a negation adverb. This example uses the negation-adverb-precedes-verb construction to activate proclisis. As you can see, in production it will produce exactly the same sentence, because negation comes always in front of the verb."))
    (comprehend-and-formulate  '("ele" "nao" "te" "deu" "este" "livro")
                              :cxn-inventory *propor-grammar*);;proclisis
    (add-element '((hr)))
    (add-element '((h3) "3.2 Operator-like adverbs trigger proclisis"))
    (add-element '((p) "This second example concerns a operator-like adverb. In this case, it comes before the verb, which will lead to the use of the operator-adverb-precedes-verb construction and to the proclisis of the verb. However, this adverb could also come after the verb which activate enclisis, instead of proclisis. Since the meaning network is the same, the production of this sentences will formulate both sentences."))
    (comprehend-and-formulate-all '("eu" "raramente" "o" "leio")
                              :cxn-inventory *propor-grammar*)
   ; (comprehend-and-formulate '("eu"   "leio" "o" "raramente")
   ;                           :cxn-inventory *propor-grammar*)
    (add-element '((hr)))
    (add-element '((h3) "3.3 Example of non-operator-like adverbs"))
    (add-element '((p) "There are also adverbs that will not activate proclisis, even when they come before the verb. This is the case of the adverb 'ontem' (yesterday). The following example shows that if the adverb is not operator-like nor negation, then no trigger construction will be activated, which leads to enclisis instead of proclisis."))
    (comprehend-and-formulate '("eu" "ontem" "vi" "te") 
                              :cxn-inventory *propor-grammar*)
    ))

;(clear-page)
;(show-adverbs)


(defun show-quantifiers ()
  (deactivate-all-monitors)
  (activate-monitor trace-fcg-light)
  (add-element '((hr)))
  (add-element '((a :name "quantifiers")))
  (add-element '((h1) "4. Quantified subjects"))
  (let ((downward-quantification-trigger-cxn (find-cxn 'downward-quantification-trigger-cxn *propor-grammar*)))
  (add-element '((p) "Certain noun quantifiers can also trigger proclisis when quantifying a subject that precedes the verb. Crysmann (2002) classifies the proclitic triggering quanitifers as 'downward entailing qunatifiers', because they seem to have downward monotonicity in terms of sound. To model this trigger, we have a downward-quantification-trigger construction that looks for a nominal noun phrase (the subject) and a quantifier that is connected to that noun phrase. If this quantifier comes before the verb and had a (downward +), then it means that it is a downward entailing quantifier that is triggering proclisis, so a (restricted +) and a link to the trigger is added to the verb."))
  (add-element '((p :style "color:red") "To inspect a construction, please click on the circle above the arrow."))
  (add-element (make-html downward-quantification-trigger-cxn :expand-initially t 
                            :wrap-in-paragraph t))
  (add-element '((h2) " 4.1. Downward entailing quantifiers"))
  (add-element '((p) "This example shows an example of a downward entailing quantifier: 'poucas' (few), which is quantifying the subject of the sentence and comes before the verb. This will lead to the activation of the downward-quantification-trigger that we explained before. Note that the same determination construction as above can be used to group the quantifier and the noun."))
  (add-element '((p) "The quantifier 'poucas' is analysed/formulated by means of two constructions: a morphological one handling the specific form for the feminine plural and a lexical construction for the lemma form and the dictionary meaning. In parsing, the poucas-cxn applies first (matching on the string 'poucas' and adding agreement features and information on the prosodic properties of the form: floating and downward quantification. The lexical construction few-quantifier-cxn then matches on the lex-class and sem-class features, together with the lemma form and adds the meaning. Note that this lexical construction will apply first in production (matching on the meaning). The same is valid for all morph-lex construction pairs in the grammar."))
  (add-element (make-html (find-cxn 'poucas-cxn *propor-grammar*) :expand-initially t 
                            :wrap-in-paragraph t))
  (add-element (make-html (find-cxn 'few-quantifier-cxn *propor-grammar*) :expand-initially t 
                          :wrap-in-paragraph t))
  (comprehend-and-formulate '("poucas" "pessoas" "o" "leem")
                            :cxn-inventory *propor-grammar*);;proclisis
  (add-element '((hr)))
  (add-element '((h3) "4.2 Non-downward entailing quantifiers"))
  (add-element '((p) "This example is very similar to the previous one, but instead of few the quantifier is 'algumas' (some), which is not a downward entailing quantifier. Therefore, the trigger construction will not be activated which will prevent the proclisis from activating, and instead the enclisis construction will be used. We can also notice that we are ignoring the morphological changes to the clitic in this example. Usually, in enclisis, in this situation, the 'o' would became a 'no', because of the nasal sound that precedes it. However, for simplification reasons, we ignore these changes for now. They could always be added in a post-processing phase in the future."))
   (add-element (make-html (find-cxn 'algumas-cxn *propor-grammar*) :expand-initially t 
                            :wrap-in-paragraph t))
  (add-element (make-html (find-cxn 'some-quantifier-cxn *propor-grammar*) :expand-initially t 
                          :wrap-in-paragraph t))
  
  (comprehend-and-formulate  '("algumas" "pessoas" "leem" "o")
                            :cxn-inventory *propor-grammar*)
  ))


;(clear-page)
;(show-quantifiers)


(defun show-subclauses ()
  (deactivate-all-monitors)
  (activate-monitor trace-fcg-light)
  
  (add-element '((hr)))
  (add-element '((a :name "subclauses")))
  (add-element '((h1) "5. Causal conjunction"))
  (let ((causal-conjunction-cxn (find-cxn 'causal-conjunction-cxn *propor-grammar*)))
  (add-element '((p) ""))
  
  (add-element '((h2) " 5.1. Porque"))
  (add-element '((p) "In the following sentence, the main clause is characterized by enclisis (default SVO word order), while the subclause requires proclisis: the clitic 'o' precedes the main verb: 'I gave you the book because you wanted it'. The main trigger for the marked word order in the subclause is the conjunction construction that links the main and the subclause through 'porque' ('because'). It is this construction that adds the dependencies between the units and introduces a (restricted +) feature."))
(add-element '((p :style "color:red") "To inspect a construction, please click on the circle above the arrow."))
  (add-element (make-html causal-conjunction-cxn :expand-initially t 
                            :wrap-in-paragraph t))
  (comprehend-and-formulate '("eu" "dei" "te" "este" "livro" "porque" "tu" "o" "querias")
                            :cxn-inventory *propor-grammar*);;proclisis
  (add-element '((hr)))

  ))

;(show-subclauses)

(create-static-html-page
    "A Computational Construction Grammar Approach to Pronominal Clitics in European Portuguese"
  (clear-page)
  (my-head-menu)
  (set-configuration *fcg-visualization-configuration* :with-search-debug-data t)
  (show-fronted-focus)
  (show-wh-questions)
  (show-adverbs)
  (show-quantifiers)
  (show-subclauses)
  )

