;(ql:quickload :fcg-english)

(in-package :fcg-english)

(activate-monitor trace-fcg)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Climate change frame extractor"))
  (add-element '((p) "This web demonstration accompanies the following manuscript"))
  (add-element '((p) "K. Beuls (in prep.). A hybrid parser for semantic frame extraction in the climate change debate." ))
  (add-element '((p) "Comments and feedback can be send by e-mail to " ((a :href "mailto:katrien@ai.vub.ac.be") "katrien@ai.vub.ac.be") "."))
  (add-element '((p) "Explanations on how to use this demo can be found " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "WD-1. A hybrid approach.")))

  (add-element '((p)  ((a :href "#WD-2") "WD-2. Two types of constructions.")))
  
  (add-element '((p)  ((a :href "#WD-3") "WD-3. A full FCG analysis in detail.")))
  
  (add-element '((p)  ((a :href "#WD-4") "WD-4. Evaluation: Coverage and accuracy.")))
  
  (add-element '((hr))))

; (header)


(defun WD-1 ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "WD-1. A hybrid approach"))
  (add-element '((p) "The main idea behind this semantic parser is to develop a tool that allows you to extract certain semantic frames from English news paper article texts, more specifically from articles about climate change. The tool itself makes use of a number of NLP preprocessing techniques such as a tokenizer and a state-of-the-art syntactic dependency parser provided by spaCy, as well as a hand-written construction grammar that covers a number of semantic frames from FrameNet, such as Causation, Expectation and Predicting. Our approach falls within the domain of hybrid AI research, as it combines the output of a statistical model with a powerful frame processing engine based on symbol manipulation and heuristic search."))
  (add-element '((p) "Normally, FCG analyses start with a single unit that contains features representing the input in either parsing or production. Typically, these include the words themselves as well as the order in which they occurred in the utterance, or (in production) the meaning that needs to be expressed in terms of a predicate calculus representation. The frame extractor starts instead with a fully fledged syntactic structure, with units for every word in the utterance and features that indicate the part-of-speech tags and the dependency labels. Word order features are also present in the special root-unit."))
  (add-element '((p) "An example initial transient feature structure for the sentence 'Oxygen levels in oceans have fallen 2 % in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks.' is included here below. You can click on the units to see the features that were added in the preprocessing step. To expand the complete feature structure, please click on the encircled '+' sign on the left."))

  (with-disabled-monitor-notifications
    (setf *solution* (second (multiple-value-list (comprehend "Oxygen levels in oceans have fallen 2 % in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks.")))))
  
  (add-element (make-html-fcg-light (initial-cfs (cip *solution*))
                                    :feature-types (feature-types *fcg-constructions*)
                                    :construction-inventory *fcg-constructions*
                                    :expand-units nil))

  (add-element '((p) "Now, our FCG grammar fragment can work on this initial structure to build a semantic frame analysis that contains the target frames we are interested in, with their slot fillers assigned. For the above sentence, the result that we obtain is the following: "))

  (run-pie *solution*)

  (add-element '((p) "The result is a frame set, consisting of one frame, namely an instance of a Causation frame, with four slots that were filled in by the frame processing engine:"))
  (add-element '((ol)
                 ((li) ((b) "Frame evoking element") ": The word(s) that activated the frame, in this case 'due to'.")
                 ((li) ((b) "Effect") ": This slot is here filled with another frame, namely a Change Position on a Scale frame. The effect is thus the fact that oxygen levels in oceans (item) have fallen by 2% (difference) in 50 years (duration).")
                 ((li) ((b) "Cause") ": This slot is filled by the words in the sentence corresponding to the cause units: 'Climate change'.")
                 ((li) ((b) "Affected") ": Finally, the subclause containing the item that is affected by the causation: 'marine habitat and large fish such as tuna and sharks'.")))

  (add-element '((p) "The next part of this web demo explains how such a semantic frame analysis can be built the grammar."))
  
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-1)

(defun WD-2 ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "WD-2. Two types of constructions."))
  (add-element '((p) "To build a semantic frame analysis as the one we saw above, two types of constructions are ativated: morpho-lexical constructions and frame slot fillers."))
  (add-element '((h3) "1) Morpho-lexical constructions"))
  (add-element '((p) "Morpho-lexical constructions trigger when a certain word form that corresponds to what in FrameNet is called a 'Lexical Unit' is detected in the input sentence. For example, in the example sentence that was introduced above, the presence of the conjugated verb form 'fallen' triggers the application of the fallen-morph construction, which in turn activates the fall-verb lexical construction. The lexical construction introduces the frame, which in this case is the 'Change_position_on_a_scale' frame. When a lexical unit is related to multiple frames, all hypotheses will be added to the search space as competitors."))
  
  (add-element '((p) "The two morpho-lexical constructions that activate the Change_position_on_a_scale frame are included here below. You can click on the encircled '+' sign in the middle to expand the construction and see its internal structure."))

  (add-element '((p) "The morphological construction will look for a unit with the string 'fallen' inside and add a number of features to that unit, namely a 'syn-cat' feature with information on the 'lex-class', 'verb-form', 'finiteness' and 'tense-aspect-modality'. It also adds a 'lex-id' feature that signals the lemma of this conjugated verb form."))

  (set-configuration (visualization-configuration *fcg-constructions*)
                     :hide-features '(footprints))
  
  (add-element (make-html (find-cxn 'fall->fallen-morph *fcg-constructions* :hash-key "fallen") :expand-initially t))

  (add-element '((p) "The lexical construction can now do its work as it will match on a unit with a 'lex-class: verb' feature in its 'syn-cat' feature and a 'lex-id' set to 'fall'. Remember that in parsing/comprehension the lower part of the right-hand side of the construction matches first with the transient structure and all the remaining features will be contributed, including the semantic frame and its core frame elements."))

  (add-element '((a :name "fall-verb-lex-cxn")))
  (add-element (make-html (find-cxn 'fall-verb-lex *fcg-constructions* :hash-key 'fall)))

  (add-element '((p) "The 'due to' preposition is a special case here, since we opted to do a single morpho-lexical construction that both matches on the actual form 'due to', as well as adding the semantic frame of Causation to the feature structure. In fact, the X-due-to-Y construction also does the argument linking, and therefore introduces the second type of constructions that the Semantic Frame Extractor uses: frame slot filling constructions."))
  
  (add-element (make-html (find-cxn 'x-due-to-y *fcg-constructions* :hash-key nil)))

  (add-element '((p) "The X-due-to-Y construction matches on four units. Apart from a lexical unit containing the string 'due' with a dependency label 'prep', it looks for a subunit that has a 'pos-tag: in' feature and a 'pcomp' dependency label feature. Further, both the frame slots for Cause as well as Effect are already linked in by this construction: the Effect is linked to the '?event-unit' in the construction, which has to be the parent unit of the '?due-unit' in the syntactic dependency structure, and it needs a 'phrase-type: VP' feature as well as a 'syn-valence' feature. The Cause is syntactically dependent on the '?to-unit' in the dependency structure and is in this case a unit with 'pos-tag: NN' and dependency lagbel 'pobj'. The main role of this construction is thus to detect certain syntactic valency structures and bind the values of the very important 'referent' features of the participating units to the correct slots in the activated frame."))

  (add-element '((h3) "2) Grammatical constructions"))
  (add-element '((p) "Grammatical constructions can in this grammar be seen as 'slot filler constructions' and their main role is to link slots of a certain frame that is active to particular units in the transient structure, by making the value of their referent features the same as the value of the frame slot feature. The X-due-to-Y construction above was already one example of this. I will here include a few additional examples to illustrate the crucial role of these constructions, all of which were needed to analyse the example sentence."))

  (add-element '((h4) "Introducing non-core Frame Elements"))
  
  (add-element '((p) "Lexical constructions typically only contain the core frame elements of a frame. To add non-core frame elements to the analysis, grammatical constructions come into play. An example of such a construction that introduces a non-core frame element for the Causation frame is the X-affecting-Y construction depicted below (click to expand). The construction adds a 'affected' frame element to the causation frame. It does so by matchin on a unit with a 'sem-cat' set to 'frame: causation' and which has a pointer to the frame through its 'referent' feature. By making sure the value of the referent feature of the frame unit, in this case the variable '?frame' is repeated in the new 'meaning' feature, the additional slot '(affected, ?frame, ?affected) is linked to the existing causation frame. Note that this construction is specifically for the lexical unit 'affecting', matching on the string and the 'pos-tag: vbg' feature."))

  (add-element (make-html (find-cxn 'x-affecting-y *fcg-constructions* :hash-key nil)))

  (add-element '((h4) "Generic argument structure constructions"))

  (add-element '((p) "The grammar contains a number of more general argument structure constructions that can be used more broadly to link a verb's arguments to the frame slot fillers it hosts. An example is the intransitive construction. The intransitive construction in this grammar fragment matches on two units: a subject unit, characterized by the 'nsubj' dependency label and its parent unit the VP unit which links the actor in its sem-valence feature to the referent of the subject unit. Through this simple repetition of the variable that gets bound to the actor of the event represented by the verb, the same subject unit and all its lexical material will also be bound to the item slot in the Change_position_on_a_scale frame for example."))

  (add-element (make-html (find-cxn 'intransitive-cxn *fcg-constructions* :hash-key nil)))

  (add-element '((p) "How is that possible? Remember the "((a :href "#fall-verb-lex-cxn") "lexical construction for the verb 'fall'?") " On its contributing part (the construction's left-hand side) it introduced a 'sem-valence' feature to the unit with 'actor: ?x' as its value. The exact same variable '?x' also reappears in the item slot that is part of the meaning feature on the right-hand side. In its definition, the fall-verb-lex is thus seen as an inherently intransitive verb." ))

  (add-element '((h4) "Single slot fillers"))

  (add-element '((p) "An example of a construction that links a single frame element that is not part of the 'sem-valence' feature is the following (click the encircled '+' to expand before reading further):"))

  (add-element (make-html (find-cxn 'change-position-on-a-scale=duration-cxn *fcg-constructions* :hash-key nil)))

  (add-element '((p) "If you click once on the '?x' you will see that it lights up twice in the construction: both as the value of the 'referent' feature of the duration unit (which is the preposition unit with a noun that has a numerical modifier as its dependent) and as the last element in the duration meaning predicate of the verb unit. Syntactically speaking, the verb unit is the parent unit of the preposition that expresses the duration of a frame (that has such a core frame element). "))
  
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

;; (wd-2)


(defun WD-3 ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "WD-3. A full FCG analysis in detail."))
  (add-element '((p) ""))
  (setf *solution* (second (multiple-value-list (comprehend "Oxygen levels in oceans have fallen 2 % in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks."))))
  (run-pie *solution*) ;;replace meaning network with PIE
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

;; (wd-3)

(defun WD-4 ()
  (add-element '((a :name "WD-4")))
  (add-element '((h2) "WD-4. Evaluation."))
  (add-element '((p) ""))
  
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))


;(with-disabled-monitor-notifications (setf *solution* (second (multiple-value-list (comprehend "Oxygen levels in oceans have fallen 2 % in 50 years due to climate change, affecting marine habitat and large fish such as tuna and sharks.")))))

(clear-page)

(defun make-demo ()
 (create-static-html-page "Climate Change Frame Extractor"
  (header)
  (wd-1)
  (wd-2)
  (wd-3)
;  (wd-4)
  ))

; (make-demo)