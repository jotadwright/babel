;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo for Constructions and Frames special issue  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :spanish-grammar)
(in-package :fcg)
(define-css 'main
            "p {font-size: 12pt}")
;(define-css 'predicate-network-svg
;            "div.predicate-network-svg {font-size: 20pt}")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup for static HTML FCG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-disabled-monitors
  (multiple-value-bind (meaning cip-solution)
      (formulate '((time-point recalled-point point-1)
                   (simultaneous point-1 dinner-event-1)
                   (event-perspective unbound dinner-event-1)
                   (activity eat dinner-event-1)
                   (eater dinner-event-1 you-1)
                   (person you you-1)
                   (quantity singleton you-1)) :cxn-inventory *spanish-verb-conjugation*)
    (setf *cenabas-final-cfs* (car-resulting-cfs (cipn-car cip-solution)))))

(activate-monitor trace-fcg)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "An open-ended computational construction grammar for Spanish verb conjugation"))
  (add-element '((p) "This web demonstration accompanies the following paper:"))
  (add-element '((p) "Beuls, K. (2017). "((a :href "" :target "_blank") "An open-ended computational construction grammar for Spanish verb conjugation." )" "((i) "Constructions and Frames") "."))
  (add-element '((p) "Abstract: The Spanish verb phrase can take on many forms, depending on the temporal, aspectual and modal interpretation that a speaker wants to convey. At least half a dozen constructions work together to build or analyze even the simplest verb form such as <i>hablo</i> 'I speak'. This paper documents how the complete Spanish verb conjugation system can be operationalised in a computational construction grammar formalism, namely Fluid Construction Grammar. Moreover, it shows how one can, starting from a seed grammar handling regular morphology and grammar, create a productive grammar that can capture systematicity in Spanish verb conjugation that can expand its construction inventory when new verbs are encountered"))
  (add-element '((p) "Explanations on how to interact with an FCG web demonstration can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "WD-1. Grammar design")))
  (add-element '((p)  ((a :href "#WD-2") "WD-2. Verb conjugation.")))
  (add-element '((p)  ((a :href "#WD-3") "WD-3. Stem changes.")))
  ;(add-element '((p)  ((a :href "#WD-4") "WD-4. Irregular verb conjugation")))
  (add-element '((hr))))

; (header)

(defun WD-1 ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "WD-1. Grammar design"))
  (add-element '((p) "The grammar fragment that this demo describes targets the Spanish verb phrase, uniquely focussing on the conjugated verb form. Rather than storing all verb forms individually as holistic chunks, the FCG grammar contains productive rules in the form of constructions, working together to build or analyse a single conjugation. For a simple form such as <i> cenabas </i>, 'you had dinner', the grammar makes use of six constructions to map the verb form to the following meaning representation:"))
   (add-element (predicate-network->svg (shuffle '((time-point recalled-point point-1)
                                                   (simultaneous point-1 dinner-event-1)
                                                   (event-perspective unbound dinner-event-1)
                                                   (activity eat dinner-event-1)
                                                   (eater dinner-event-1 you-1)
                                                   (person you you-1)
                                                   (quantity singleton you-1)))
                                      :only-variables nil :extensional-meanings nil))
   (add-element '((p) "Note: Because we focus on the conjugation of individual verb forms, the grammar fragment only considers intransitive verb meanings. Therefore every verb, including transitive or ditransitive verbs, has only a single role in its meaning representation. "))
  (add-element '((p) "A constituency grammar approach is used to model conjugated verb forms in this grammar fragment. The resulting transient structure after producing the above meaning network looks as follows:"))
  (add-element (make-html-fcg-light *cenabas-final-cfs*
                                    :construction-inventory *spanish-verb-conjugation*
                                    :feature-types (feature-types *spanish-verb-conjugation*)))
  (add-element '((p) "The utterance that can be extracted from this transient structure is ''cen aba s''. Yet, the transient structure contains more units than just the three form bearing morpheme units. Three additional units have been created. The first one collects the stem morpheme and the two suffix morpheme units into a verb unit, containing agreement information and the tense-aspect-mood characterization of the form, together with the meaning of the form and the order of the morphemes (in the form feature). Another unit is made for the unmarked subject, which is a second singular person in this case. Together, the subject and the verb unit are the constituents of the intransitive clause unit."))
  (add-element '((p) "A transient structure is the result of a repeated application process of constructions. In the case of <i> cenabas </i> six constructions did their work and they can be split up into four different types:"))
  (wd-1-1)
  (wd-1-2)
  (wd-1-3)
  (wd-1-4)
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr)))
  )

;(wd-1)

(defun WD-1-1 ()
  (add-element '((a :name "WD-1-1")))
  (add-element '((h3) "WD-1-1. Lexical constructions"))
  (add-element '((p) "These constructions can be compared to a dictionary look-up for a given verb. They contain features such as lex-class, verb-class, sem-class, lemma and meaning. The lexical construction for <i> cenar </i> 'to dine, to have dinner' is included here below. You can now inspect the full construction by clicking on the &#8853; sign and reading the explanations below."))
  (add-element (make-html (find-cxn 'cenar-lex *spanish-verb-conjugation*) :expand-initially t 
             :wrap-in-paragraph nil))
  (add-element '((p) "A lexical construction for a verb lemma consists of two units on the conditional part: the ?cenar-verb unit and the ?cenar-stem unit. The stem unit is going to be a constituent of the verb unit, together with potential suffixes (to be added later). In production, all this lexical construction matches on is the meaning feature in the root unit (accessed by the HASH operator). In comprehension, on the other hand, we expect two units to be there already, linked by a constituency relation, with the stem unit bearing the lemma ``cenar''. The contributing part will merge information into the two units, such as a lex-class, a verb-class (set to 1st verb class, since the lemma is ending on <i> -ar </i>) and initialises the tense-aspect-mood features with variables. The semantic class is inherited from the meaning feature, and the agent of the activity is linked to the eater role.")))

;(wd-1-1)
(defun WD-1-2 ()
  (add-element '((a :name "WD-1-2")))
  (add-element '((h3) "WD-1-2. Stem constructions"))
  (add-element '((p) "Lexical constructions do not contain actual word forms that will be encountered in sentences. This is the task of the stem constructions. For a default stem such as <i> cen- </i>, the corresponding stem construction maps the lemma ''cenar'' into a string feature ''cen'':"))
  (add-element (make-html (find-cxn 'cenar-base-stem *spanish-verb-conjugation*) :expand-initially t 
                          :wrap-in-paragraph t))
  (add-element '((p) "If you inspect the above construction, you will see that there is a red feature inside the phon-cat feature. This colour highlighting indicates a negation of the <tt> stem-realized </tt> feature, meaning that the construction can only apply in production if the phon-cat of the stem unit does NOT contain a <tt> (stem-realized +) </tt> feature. Such a precondition is needed because this is the construction for the base stem of the verb, and thus constitutes a sort of default.")));;LINK TO BELOW

;(wd-1-2)

(defun WD-1-3 ()
  (add-element '((a :name "WD-1-3")))
  (add-element '((h3) "WD-1-3. Suffix constructions"))
  (add-element '((p) "To make the Spanish verb phrase modular, a verb form is analysed into three parts: a verb stem (see above), a tense-aspect-mood suffix and an agreement suffix (e.g. <i> cen </i> <i> -aba </i> <i> -s </i>). The <i> -abas </i> suffix is split into two parts: a morpheme that indicates the tense/aspect/mood of the verb form and a morpheme for the person/number information. The construction here below indeed matches on a verb unit in the transient structure that has a past tense, imperfective aspect and indicative mood of the first verb class (in production); or an <i> -aba </i> suffix immediately adjacent to the verb stem (in comprehension). The contributing part of the construction creates a suffix unit, which is a constituent of the verb unit, and a sibling of the stem unit."))
  (add-element (make-html (find-cxn 'tam-suffix-aba-morph *spanish-verb-conjugation*) :expand-initially t 
                          :wrap-in-paragraph t))
  (add-element '((p) "The person/number information is added by the <tt> s-2sg-morph</tt> construction included here below. Its preconditions in production are the following: a second singular agreement feature, a verb of any verb class that is not future and not perfective. It is also a requirement that the verb unit already has two constituent units. When these conditions are met, the construction makes the verb stem unit finite, as well as creating a second suffix unit that follows the verb stem unit (but does not have to be directly adjacent to it, hence the use of the <tt> precedes</tt> feature)."))
  (add-element (make-html (find-cxn 's-2sg-morph *spanish-verb-conjugation*) :expand-initially t 
                          :wrap-in-paragraph t)))

;(wd-1-3)

(defun WD-1-4 ()
  (add-element '((a :name "WD-1-4")))
  (add-element '((h3) "WD-1-4. Grammatical constructions"))
  (add-element '((p) "Two remaining constructions are involved in the processing of a form such as <i> cenabas </i>: the intransitive second singular construction and the past-imperfective construction. The first one matches on a verb unit with a second singular agreement feature in comprehension and merges a corresponding meaning representation into its structure. Also, a new unit, the VP unit is created that has two dependent units: the verb stem unit itself, as well as the unmarked subject unit."))
  (add-element (make-html (find-cxn 'intransitive-2sg-covert-subject-cxn *spanish-verb-conjugation*) :expand-initially t 
                          :wrap-in-paragraph t))
  (add-element '((p) "Finally, the past imperfective constructions maps a meaning representation onto a syntactic configuration of the tense, aspect and mood features. No contributing part is required."))
  (add-element (make-html (find-cxn 'past-imperfective-indicative-cxn *spanish-verb-conjugation*) :expand-initially t 
                          :wrap-in-paragraph t))
  
  )

; (clear-page)
; (wd-1-4)

(defun WD-2 ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "WD-2. Verb conjugation."))
  (add-element '((p) "Let us now turn to the working of these constructions in an actual production setting. First in line are the constructions that carve out a part of the meaning that needs to be expressed: the <tt>cenar-lex</tt>, the <tt>2sg-covert-subject-cxn</tt> and the <tt>past-imperfective-indicative-cxn</tt>. Then, the morphological constructions can work on this and attach actual forms to these meanings, through the expression of syntactic features."))
  (add-element '((p) "You can inpect each step in the construction application process by clicking twice on the green nodes. The resulting transient structure is shown at the bottom."))
  (formulate '((time-point recalled-point point-1)
               (simultaneous point-1 dinner-event-1)
               (event-perspective unbound dinner-event-1)
               (activity eat dinner-event-1)
               (eater dinner-event-1 you-1)
               (person you you-1)
               (quantity singleton you-1)) :cxn-inventory *spanish-verb-conjugation*)
  (add-element '((hr)))
  (add-element '((p) "Let us inspect the comprehension process of a closely related verb form, <i> cen-aba </i>, which can both be used to say 'I had dinner' or 'he/she had dinner'. In comprehension, the order of construction application is different. First the morphological constructions apply, as they match on string features that are present in the root unit. Then, the syntactic features that these constructions have added for tense and agreement for instance, are conditions for the grammatical constructions to apply."))
  (add-element '((p) "Two solutions are provided when parsing <i> cen-aba </i>. The lack of an overt person/number ending can both be interpreted as a first person singular or a third person singular. There is one construction that covers both cases: <tt> no-marker-1/3sg-morph</tt>. This construction is part of a special construction set (default), that is consulted only after the regular morphological constructions could not apply. Please click on the construction to inspect its features in more detail."))
  (comprehend-all '("cen" "aba") :cxn-inventory *spanish-verb-conjugation*)

  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-2)

(defun WD-3 ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "WD-3. Stem changes."))
  (add-element '((p) "Verb conjugation in Spanish, as other Romance languages, is characterized by a large number of verb paradigms according to which specific infinitives are conjugated. Rather than simply combining the verb stem with one or two suffixes, the actual verb form can often be the result of transformations following from stress patterns or phonetic assimilation processes. "))
  ;(add-element '((h4) "1. Changes in the verb stem"))
  (add-element '((p) "To account for multiple possible realizations of a verb stem (given the same lemma), a range of morphological allostructions needs to be defined, which compete with each other. For instance, for the verb <i>cocer</i> 'to cook', four stem allostructions are implemented:"))
  (add-element (make-html (find-cxn 'cocer-base-stem *spanish-verb-conjugation*)
                          :expand-initially t 
                          :wrap-in-paragraph nil))
  (add-element (make-html (find-cxn 'cocer-cuec-stem *spanish-verb-conjugation*)
                          :expand-initially t 
                          :wrap-in-paragraph nil))
  (add-element (make-html (find-cxn 'cocer-cuez-stem *spanish-verb-conjugation*)
                          :expand-initially t 
                          :wrap-in-paragraph nil))
  (add-element (make-html (find-cxn 'cocer-coz-stem *spanish-verb-conjugation*)
                          :expand-initially t 
                          :wrap-in-paragraph nil))
  (add-element '((p) "Please expand these stem constructions now. You will see that in comprehension, the stem form itself will decide which of the four stem constructions will match: <i>coc- </i>, <i>cuec-</i>, <i>cuez-</i> or <i>coz- </i>. The real difficulty lies in production, where the correct form needs to be selected. The three irregular stem constructions is always tried first, before the default <tt>cocer-base-stem</tt> construction. If an irregular stem construction can match, it will add a <tt>(stem-realized +)</tt> to the stem-unit, which will then prevent the default construction to match (see negated feature)."))
  (add-element '((p) "Let us look at these irregular stem constructions in action."))
  (WD-3-1)
  (WD-3-2)
  (WD-3-3)
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

(defun WD-3-1 ()
  (add-element '((a :name "WD-3-1")))
  (add-element '((h3) "WD-3-1. <i>cuez-o</i> 'I cook'"))
  
  (add-element '((p) "The first verb form that will be produced is <i>cuez-o </i> 'I cook', where the stem receives primary stress and is therefore diphthongized. Stress in Spanish words always falls on the penultimate syllable. Moreover, the final consonant of the original stem <i>-c-</i> turns into a <i>-z-</i> because it is followed by a back vowel (<i>-o</i>)."))
  (formulate '((time-point present-point point-1)
               (simultaneous point-1 cook-event-1)
               (apply-heat cook cook-event-1)
               (cook cook-event-1 me-1)
               (person me me-1)) :cxn-inventory *spanish-verb-conjugation*) ;;pienso
  (add-element '((p) "The construction that is responsible for the application of the correct stem construction is the <tt> stress-cxn-1</tt> construction. If you click on the construction here below, you will see that it is a production-only construction that is looking for a verb unit with a stem and a suffix unit as its constituents, which are directly adjacent to each other. Moreover, the suffix-unit cannot be a tense-aspect-mood morpheme (i.e. not an infix) and cannot carry primary stress. If these conditions are met, the construction will add a <tt>(primary-stress +)</tt> feature to the stem unit, which is a feature that is part of the production lock of the <tt>pensar-piens-stem</tt> construction."))
  (add-element (make-html (find-cxn 'stress-cxn-1 *spanish-verb-conjugation*)
                          :expand-initially t 
                          :wrap-in-paragraph nil))
  (add-element '((hr)))
  )

;(WD-3-1)

(defun WD-3-2 ()
  (add-element '((a :name "WD-3-2")))
  (add-element '((h3) "WD-3-2. <i>cuec-e-s</i> 'You cook'"))
  (add-element '((p) "Another irregular stem of the verb <i>cocer</i> is <i>cuec-</i>. This stem is used in the production of 'you cook', <i>cuec-e-s</i>:"))
  (formulate '((time-point present-point point-1)
               (simultaneous point-1 cook-event-1)
               (apply-heat cook cook-event-1)
               (cook cook-event-1 you-1)
               (person you you-1)
               (quantity singleton you-1)) :cxn-inventory *spanish-verb-conjugation*) 
  (add-element '((p) "A second stress construction is used now, which is specialized on verb forms with two suffixes following the stem. You can click on it to inspect its production locks. It will apply when the person-number suffix does not contain a vowel and the tense-aspect-mood suffix does not receive primary stress."))
  (add-element '((hr))))

(defun WD-3-3 ()
  (add-element '((a :name "WD-3-3")))
  (add-element '((h3) "WD-3-3. <i>coz-a-mos</i> 'We cook' (subjunctive)"))
  (add-element '((p) "A less frequently used irregular stem of the verb <i>cocer</i> is <i>coz-</i>. It only occurs in three slots of the verb conjugation paradigm: in the first plural and second plural of the subjunctive present and in the imperative first plural. Let us look at the subjunctive form 'we cook', <i>coz-a-mos</i>:"))
  (formulate '((time-point present-point point-1)
               (simultaneous point-1 cook-event-1)
               (apply-heat cook cook-event-1)
               (cook cook-event-1 you-1)
               (person we you-1)
               (event-mood subjunctive cook-event-1)
               ) :cxn-inventory *spanish-verb-conjugation*) 
  
  (add-element '((hr))))


;(WD-3-2)
;(wd-3)

(defun make-demo ()
(create-static-html-page "An open-ended computational construction grammar for Spanish verb conjugation"
  (header)
  (wd-1)
  (wd-2)
  (wd-3)
  ))

; (make-demo)



