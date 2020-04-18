;;;;;;;;;;;;;;
;; Load FCG ;;
;;;;;;;;;;;;;;

;; (ql:quickload :fcg) 

(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load example grammar for routine processing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions demo-routine
    :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes))
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the"))))))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse"))))))
  
   (def-fcg-cxn noun-phrase-cxn
                ((?noun-phrase
                  (args (?x))
                  (parent ?parent)
                  (sem-cat (sem-class referring-expression))
                  (syn-cat (lex-class noun-phrase))
                  (subunits (?article ?noun)))
                 <-
                 (?article
                  (args (?x))
                  (sem-cat (sem-class referent))
                  --
                  (syn-cat (lex-class article)))
                 (?noun
                  (args (?x))
                  (sem-cat (sem-class physical-entity))
                  --
                  (syn-cat (lex-class noun)))
                 (?noun-phrase
                  --
                  (HASH form ((precedes ?article ?noun)))))))

;; (comprehend-and-formulate "the mouse")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load example grammar for new lexical cxns  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions demo-new-lexical
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (footprints set))
  :diagnostics (diagnose-unknown-words
                diagnose-unknown-meaning-predicates)
  :hierarchy-features (subunits)
  :repairs (add-lexical-cxn)
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:de-render-mode . :de-render-string-meets-precedes)
                       (:consolidate-repairs . t))
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the")))))
               :cxn-set lex)

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse")))))
               :cxn-set lex)

  (def-fcg-cxn green-cxn
               ((?green-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)))
                <-
                (?green-word
                 (HASH meaning ((green ?x)))                     
                 --
                 (HASH form ((string ?green-word  "green")))))
               :cxn-set lex)
  
  (def-fcg-cxn likes-cxn
               ((?likes-word
                 (args (?x ?y))
                 (parent ?parent)
                 (sem-cat (sem-class relation))
                 (syn-cat (lex-class verb)
                          (type transitive)))
                <-
                (?likes-word
                 (HASH meaning ((deep-affection ?x ?y)))                     
                 --
                 (HASH form ((string ?likes-word  "likes")))))
               :cxn-set lex)
  
  (def-fcg-cxn linguist-cxn
               ((?linguist-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?linguist-word
                 (HASH meaning ((linguist ?x)))                     
                 --
                 (HASH form ((string ?linguist-word  "linguist")))))
               :cxn-set lex)
  
  ;;Grammatical Constructions
  ;; NP -> ART NOUN
   (def-fcg-cxn noun-phrase-cxn
                ((?noun-phrase
                  (args (?x))
                  (parent ?parent)
                  (sem-cat (sem-class referring-expression))
                  (syn-cat (lex-class noun-phrase))
                  (subunits (?article ?noun)))
                 (?noun
                  (footprints (phrasal-cxn)))
                 <-
                 (?article
                  (parent ?noun-phrase)
                  (args (?x))
                  (sem-cat (sem-class referent))
                  --
                  (parent ?noun-phrase)
                  (syn-cat (lex-class article)))
                 (?noun
                  (parent ?noun-phrase)
                  (args (?x))
                  (sem-cat (sem-class physical-entity))
                  (footprints (not phrasal-cxn))
                  --
                  (parent ?noun-phrase)
                  (syn-cat (lex-class noun))
                  (footprints (not phrasal-cxn)))
                 (?noun-phrase
                  --
                  (HASH form ((meets ?article ?noun)))))
                :disable-automatic-footprints t)
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (subunits (?verb)))
                <-
                (?verb
                 (args (?x ?y))
                 (parent ?verb-phrase)
                 (sem-cat (sem-class relation))
                 --
                 (syn-cat (lex-class verb)
                          (type transitive)))))
  
  ;; Transitive-clause -> NP VP NP
  (def-fcg-cxn transitive-clause-cxn
               ((?transitive-clause
                 (args (?x ?y))
                 (sem-cat (sem-class predicating-expression))
                 (syn-cat (lex-class transitive-clause))
                 (subunits (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (subunits (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (subunits (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (subunits (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (subunits (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (subunits (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (subunits (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun ?verb)
                             (meets ?verb ?object-article)))))))

;; (formulate '((linguist o-1) (unique o-1) (deep-affection o-1 o-2) (unique o-2) (mouse o-2)))

;; (formulate '((linguist o-1) (unique o-1) (deep-affection o-1 o-2) (unique o-2) (cat o-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load example grammar for new phrasal constructions  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions demo-new-phrasal
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :hierarchy-features (subunits)
  :diagnostics (diagnose-unconnected-meaning)
  :repairs (add-phrasal-cxn)
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:de-render-mode . :de-render-string-meets-precedes)
                       (:consolidate-repairs . t))
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the")))))
               :cxn-set lex)

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse")))))
               :cxn-set lex)

  (def-fcg-cxn green-cxn
               ((?green-word
                 (args (?x))
                 (parent ?parent)
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)))
                <-
                (?green-word
                 (HASH meaning ((green ?x)))                     
                 --
                 (HASH form ((string ?green-word  "green")))))
               :cxn-set lex)
 
  
  ;;Grammatical Constructions
  ;; NP -> ART NOUN
   (def-fcg-cxn noun-phrase-cxn
                ((?noun-phrase
                  (args (?x))
                  (parent ?parent)
                  (sem-cat (sem-class referring-expression))
                  (syn-cat (lex-class noun-phrase))
                  (subunits (?article ?noun)))
                 (?noun
                  (footprints (phrasal-cxn)))
                 <-
                 (?article
                  (parent ?noun-phrase)
                  (args (?x))
                  (sem-cat (sem-class referent))
                  --
                  (parent ?noun-phrase)
                  (syn-cat (lex-class article)))
                 (?noun
                  (parent ?noun-phrase)
                  (args (?x))
                  (sem-cat (sem-class physical-entity))
                  (footprints (not phrasal-cxn))
                  --
                  (parent ?noun-phrase)
                  (syn-cat (lex-class noun))
                  (footprints (not phrasal-cxn)))
                 (?noun-phrase
                  --
                  (HASH form ((precedes ?article ?noun)))))
                :disable-automatic-footprints t))

;; (comprehend '("the" "green" "mouse"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load example grammar for anti- & pro-unification  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions demo-anti-pro-unification
  :feature-types ((args sequence)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set))
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-structure)
                       (:production-goal-tests :no-applicable-cxns :connected-structure)
                       (:de-render-mode . :de-render-in-one-pole-mode-meets-only)
                       (:shuffle-cxns-before-application . nil))
  :diagnostics (diagnose-no-match)
  :repairs (anti-unify-pro-unify)
  
  ;; Lexical construction for the word "fille"
  (def-fcg-cxn fille-cxn
               ((?girl-unit
                 (args (?x))
                 (sem-cat (sem-class physical-object)
                          (animate +))
                 (syn-cat (lex-class noun)
                          (agreement (number sg)
                                     (gender f))))
                <-
                (?girl-unit
                 (HASH meaning ((person girl ?x)))
                 --
                 (HASH form ((string ?girl-unit "fille"))))))

  (def-fcg-cxn diner-n-cxn
               ((?diner-unit
                 (args (?x))
                 (sem-cat (sem-class physical-object)
                          (animate -))
                 (syn-cat (lex-class noun)
                          (agreement (number sg)
                                     (gender m))))
                <-
                (?diner-unit
                 (HASH meaning ((meal dinner ?x)))
                 --
                 (HASH form ((string ?diner-unit "dîner"))))))
  
  ;; Lexical construction for the word "une"
  (def-fcg-cxn une-cxn
               ((?a-unit
                 (args (?x))
                 (sem-cat (sem-function determiner)
                          (definite -))
                 (syn-cat (lex-class article)
                          (agreement (number sg)
                                     (gender f))))
                <-
                (?a-unit
                 (HASH meaning ((status indefinite ?x)))
                 --
                 (HASH form ((string ?a-unit "une"))))))

  ;; Lexical construction for the word "un"
  (def-fcg-cxn un-cxn
               ((?a-unit
                 (args (?x))
                 (sem-cat (sem-function determiner)
                          (definite -))
                 (syn-cat (lex-class article)
                          (agreement (number sg)
                                     (gender m))))
                <-
                (?a-unit
                 (HASH meaning ((status indefinite ?x)))
                 --
                 (HASH form ((string ?a-unit "un"))))))


  (def-fcg-cxn petit-cxn
               ((?petit-unit
                 (args (?x))
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)
                          (agreement (number sg)
                                     (gender m))))
                <-
                (?petit-unit
                 (HASH meaning ((size small ?x)))
                 --
                 (HASH form ((string ?petit-unit "petit"))))))

  (def-fcg-cxn petite-cxn
               ((?petite-unit
                 (args (?x))
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)
                          (agreement (number sg)
                                     (gender f))))
                <-
                (?petite-unit
                 (HASH meaning ((size small ?x)))
                 --
                 (HASH form ((string ?petite-unit "petite"))))))

  (def-fcg-cxn formidable-cxn
               ((?formidable-unit
                 (args (?x))
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)
                          (agreement (number sg)
                                     (gender ?g))))
                <-
                (?formidable-unit
                 (HASH meaning ((quality splendid ?x)))
                 --
                 (HASH form ((string ?formidable-unit "formidable"))))))

  ;; NP -> Det N
  (def-fcg-cxn np-cxn
               ((?np-unit
                 (args (?args))
                 (sem-cat (sem-function reference)
                          (sem-class physical-object))
                 (syn-cat (agreement (number ?number)
                                     (gender ?gender)))
                 (subunits (?art ?adj ?noun)))
                <-
                (?noun
                 (args (?args))
                 (sem-cat (sem-class physical-object))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?art
                 (args (?args))
                 (sem-cat (sem-function determiner)
                          (definite ?definite))
                 --
                 (syn-cat (lex-class article)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?adj
                 (args (?args))
                 (sem-cat (sem-class property))
                 --
                 (syn-cat (lex-class adjective)
                          (agreement (number ?number)
                                     (gender ?gender))))
                
                (?np-unit
                 --
                 (HASH form ((meets ?adj ?noun)
                             (meets ?art ?adj)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup for static HTML FCG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(activate-monitor trace-fcg)

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Meta-layer Problem Solving for Computational Construction Grammar"))
  (add-element '((p) "This web demo accompanies the paper:"))
  (add-element '((p) "Van Eecke, P. &amp; K. Beuls. (2017). "((a :href "https://ehai.ai.vub.ac.be/assets/pdfs/meta-layer.pdf" :target "_blank") "Meta-layer Problem Solving for Computational Construction Grammar.")" "((i) "The 2017 AAAI Spring Symposium Series. ") "Pages 258-265."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "WD-1. Routine Language Processing.")))
  (add-element '((p)  ((a :href "#WD-2") "WD-2. Meta-Layer Problem Solving: Creating New Lexical Constructions.")))
  (add-element '((p)  ((a :href "#WD-3") "WD-3. Meta-Layer Problem Solving: Inducing New Phrasal Constructions.")))
  (add-element '((p)  ((a :href "#WD-4") "WD-4. Meta-Layer Problem Solving: Generalising and Specialising Constructions using Anti- and Pro-Unification")))
  (add-element '((hr))))

; (header)

(defun WD-1 ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "WD-1. Routine Language Processing"))
  (add-element '((p) "The following example illustrates the routine processing of the utterance 'the mouse'. Clicking twice on the green nodes in the application process will show the transient structures (grey-green boxes) and applied constructions (blue boxes) at each step. Within a construction or a transient structure, clicking on the &#8853; sign will reveal its features."))
  (make-demo-routine-cxns)
  (comprehend '("the" "mouse"))
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-1)

(defun WD-2 ()
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "WD-2. Meta-Layer Problem Solving: Creating New Lexical Constructions."))
  (add-element '((p) "This example shows the creation of a new lexical construction. We try to formulate that a certain linguist likes a certain cat. However, the grammar contains no lexical construction for cat. The diagnostic notices that not all meaning predicates from the input were covered by constructions and the repair makes a new construction for the concept cat. A new string is invented and the syntactic and semantic class are underspecified in the construction."))
  (add-element '((p) "Clicking on the green box with 'noun-phrase-cxn verb-phrase-cxn' will decompact it and reveal the orange node in which the diagnostic triggered and the yellow node in which the repair did its job. As always, clicking twice on the node in the application process shows the transient structure before construction application, the applied construction, and the transient structure after construction application."))
  (make-demo-new-lexical-cxns)
  (formulate '((linguist o-1) (unique o-1) (deep-affection o-1 o-2) (unique o-2) (cat o-2)))
  (add-element '((h3)"Consolidation:"))
  (add-element '((p)"As the branch of the repair in the construction application process above lead to a solution (dark green node), the repair-cxn, in this case the cat-cxn, has been added to the grammar ('consolidation'). This means it can now be used in routine processing. Below, we formulate the same sentence again, and can see that no diagnostics or repairs are triggered now. Also, note that the construction inventory now contains 9 constructions instead of 8."))
  (formulate '((linguist o-1) (unique o-1) (deep-affection o-1 o-2) (unique o-2) (cat o-2)))
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-2)

(defun WD-3 ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "WD-3. Meta-Layer Problem Solving: Inducing New Phrasal Constructions."))
  (make-demo-new-phrasal-cxns)
  (add-element '((p) "The example demonstrates the creation of a new phrasal construction. The input utterance is 'the green mouse'. The grammar covers all words, but only contains a noun phrase construction that consists of an article and a noun. The diagnostic discovers that not all words have been integrated in the structure, and creates a new phrasal construction. The phrasal construction integrates the adjective into the noun phrase, captures its word order constraints and makes its referent identical to that of the noun phrase. The phrasal-cxn node is not shown in yellow, because it is also a solution node and therefore shown in dark green. When expanding it (click twice), you can still see the yellow mention in the status."))
  (comprehend '("the" "green" "mouse"))
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-3)

(defun WD-4 ()
  (add-element '((a :name "WD-4")))
  (add-element '((h2) "WD-4. Meta-Layer Problem Solving: Generalising and Specialising Constructions using Anti- and Pro-Unification"))
  (make-demo-anti-pro-unification-cxns)
  (add-element '((p) "This example shows how anti- and pro-unification can be used to flexibly apply a construction on a transient structure and to learn a new construction from an observation. Concretely, we show how a noun phrase with a 'new'  word order can be learned. The word order of the input utterance `un dîner formidable' (Art Noun Adj) conflicts with the word order specified in the NP-cxn (Art Adj Noun). Anti-unification helps to arrive at an interpretation and then pro-unification is used to consolidate this repair."))
  (add-element '((p) "Let us have a look at the transient structure after the application of all lexical constructions. Click the &#8853; sign to reveail the features. Especially the meets predicates in the root are important for this example."))
  (set-configuration *fcg-constructions* :use-meta-layer nil)
  (multiple-value-bind (form cip)
      (with-disabled-monitor-notifications (comprehend '("un" "dîner" "formidable") :cxn-inventory *fcg-constructions*))
    (setf *ts*  (car-resulting-cfs (cipn-car (get-last-cip-node cip))))
    (add-element (make-html-fcg-light *ts* :feature-types (feature-types *fcg-constructions*)
                                     :construction-inventory *fcg-constructions*)))
  (add-element '((p) "We can see that the noun phrase construction shown below cannot apply. The meets predicates in the NP-unit do not match those of the transient structure:"))
  (add-element (make-html (find-cxn 'np-cxn *fcg-constructions*)))
  (add-element '((hr)))
  (add-element '((p) "Now, we will see how the meta-layer solves the problem by anti- and pro-unifying the noun-phrase construction with the transient structure. The anti-unified and pro-unified construction are first shown for clarity, then the complete analysis with the meta-layer is shown. In the anti-unified construction, we can observe that the word order constraints have been relaxed and now contain unbound variables. In the pro-unified construction, we can see that the word order of the observation has been learned."))
  (set-configuration *fcg-constructions* :use-meta-layer t)
  (set-configuration *fcg-constructions* :consolidate-repairs t)
  ;; anti-unify *np-cxn* with *ts*, then pro-unify the anti-unified-cxn with *ts*
  (comprehend '("un" "dîner" "formidable"))
  (add-element '((hr)))
  (add-element '((p) "Now, we can also use the new noun-phrase-cxn in formulation:"))
  (formulate-all '((quality splendid o-1) (status indefinite o-1) (meal dinner o-1)) :cxn-inventory *fcg-constructions* :n 2)
  (add-element '((p) ((a :href "#start") "Back to menu")))
  (add-element '((hr))))

; (wd-4)

(defun make-demo ()
  (create-static-html-page "Meta-Layer Problem Solving"
    (header)
    (wd-1)
    (wd-2)
    (wd-3)
    (wd-4))
)

; (make-demo)



