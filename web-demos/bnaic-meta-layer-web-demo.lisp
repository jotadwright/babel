

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A library of Meta-Level Diagnostics and Repairs for Fluid Construction Grammar ;;
;; Web Demo BENAIC 2016                                                           ;;
;; Katrien & Paul                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op 'fcg)
(in-package :fcg)
(activate-monitor trace-fcg-light)

(def-fcg-constructions benaic-demo-grammar-lexical
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents set)
                  (dependents set)
                  (footprints set))
  :hierarchy-features (constituents dependents)
  :diagnostics (diagnose-unknown-words
                diagnose-unknown-meaning-predicates)
  :repairs (add-lexical-cxn)
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:node-tests :check-duplicate :update-references)
                       (:priority-mode . :depth-first-prefer-local-bindings)
                       (:cxn-supplier-mode . :simple-queue))
  
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
                  (constituents (?article ?noun)))
                 (?noun
                  (dependents (?article))
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
                  (HASH form ((precedes ?article ?noun ?noun-phrase)))))
                :disable-automatic-footprints t)
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
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
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun-phrase ?verb-phrase ?transitive-clause)
                             (meets ?verb-phrase ?object-noun-phrase ?transitive-clause)))))))

(def-fcg-constructions benaic-demo-grammar-phrasal
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents set)
                  (dependents set)
                  (footprints set))
  :hierarchy-features (constituents dependents)
  :diagnostics (diagnose-unconnected-meaning)
  :repairs (add-phrasal-cxn)
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns ) ;:connected-semantic-network
                       (:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:node-tests :check-duplicate :update-references)
                       (:priority-mode . :depth-first-prefer-local-bindings))
  
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
                  (constituents (?article ?noun)))
                 (?noun
                  (dependents (?article))
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
                  (HASH form ((precedes ?article ?noun ?noun-phrase)))))
                :disable-automatic-footprints t)
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
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
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun ?verb ?transitive-clause)
                             (meets ?verb ?object-article ?transitive-clause)))))))

(def-fcg-constructions benaic-demo-anti-pro-unification
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
  :repairs (anti-unify)
  
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

(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element
   '((h1) "A library of Meta-Level Diagnostics and Repairs for Fluid Construction Grammar (FCG)"))
  (add-element '((h3) "Katrien Beuls, Paul Van Eecke &amp; Luc Steels"))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here. ")))
  (add-element '((hr))))

;; (header)

(defun menu ()
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((bf)((a :href "#0" :style "font-weight:bold") "1. Diagnostics and Repairs in FCG"))))
  (add-element '((p)  ((a :href "#1") "1.1. Routine Processing")))
  (add-element '((p)  ((a :href "#2") "1.2. Meta-level Processing")))
  (add-element '((p)  ((bf)((a :href "#3" :style "font-weight:bold") "2. A Library of Diagnostics and Repairs"))))
  (add-element '((p)  ((a :href "#4") "2.1. Unknown Word")))
  (add-element '((p)  ((a :href "#5") "2.2. Unknown Meaning")))
  (add-element '((p)  ((a :href "#6") "2.3. Non-integrated Meaning Component")))
  (add-element '((p)  ((a :href "#7") "2.4. Matching Conflict")))
  (add-element '((hr))))

;; (menu)


(defun diagn-rep-fcg ()
  (add-element '((p) ((a  :name "0") "")))
  (add-element '((h2) "1. Diagnostics and Repairs in FCG")))

(defun routine-processing ()
  (add-element '((p) ((a  :name "1") "")))
  (add-element '((h3) "1.1. Routine Processing"))
  (add-element '((p) "Let's introduce a very small example grammar that can process some English utterances. It covers transitive clauses, simple noun phrases, simple verb phrases and lexical constructions for mouse, linguist, likes and the. Click on the blue boxes below to see the constructions, then on the &#10753; sign to fully expand them: "))
  (make-benaic-demo-grammar-lexical-cxns)
  (set-configuration *fcg-constructions* :use-meta-layer nil)
  (add-element (make-html *fcg-constructions* :expand-initially t))
  (add-element '((h3) "Example of utterance covered by the grammar:"))
  (add-element '((p) "The example below processes the utterance 'the linguist likes the mouse' in comprehension. It can be entirely handled by FCG's routine processing layer. We can see that the application of different operators (constructions) leads to a solution (the dark green node at the end of the tree). A solution is defined here as a state in which all predicates in the meaning are connected into one network. Click twice (or three times) on the nodes in the application process to expand them completely."))
  (comprehend '("the" "linguist" "likes" "the" "mouse" ))
  (add-element '((h3) "Example of utterance not covered by the grammar:"))
  (add-element '((p) "The second example processes the utterance 'the linguist likes the cat'. This utterance cannot be handled by routine processing as the word 'cat' is not known by the grammar. A solution cannot be found and therefore a partial solution is returned (light green node at the end of the tree)."))
  (comprehend '("the" "linguist" "likes" "the" "cat")))

;; (routine-processing)

(defun meta-level-processing ()
  (add-element '((p) ((a  :name "2") "")))
  (add-element '((h3) "1.2. Meta-level Processing"))
  (add-element '((p) "Now, we will turn on the possibility for FCG to jump to its meta-layer when a problem is diagnosed. We comprehend again the same utterance 'the linguist likes the cat'. Click on the node with the label 'noun-phrase-cxn, verb-phrase-cxn' to decompact it. You can see now that the verb-phrase-cxn node is orange, and the cat-cxn node is yellow. The orange color of the verb-phrase-cxn-node signals that this node triggered a diagnostic (namely the one for unknown words). The yellow color of the next node signals that it was added by a repair. In this case, it was the repair 'add-lexical-cxn' that added a new lexical construction for 'cat'. When expanding the construction, we can see semantic category and syntactic classe are underspecified."))
  (make-benaic-demo-grammar-lexical-cxns)
  (comprehend '("the" "linguist" "likes" "the" "cat"))
  (add-element '((h3)"Consolidation:"))
  (add-element '((p)"As the branch of the repair in the construction application process above lead to a solution (dark green node), the repair-cxn, in this case the cat-cxn, has been added to the grammar ('consolidation'). This means it can now be used in routine processing. Below, we parse the same sentence again, and can see that no diagnostics or repairs are triggered now. Also, note that the construction inventory now contains 9 construction instead of 8."))
  (comprehend '("the" "linguist" "likes" "the" "cat")))

(defun library ()
  (add-element '((hr)))
  (add-element '((p) ((a  :name "3") "")))
  (add-element '((h2) "2. Library of Diagnostics and Repairs"))
  (add-element '((p)"The following examples demonstrate how the diagnostics and repairs from the library can be directly used with Fluid Construction Grammar. Naturally, the possible use-cases are infinitely broader than these few examples.")))

(defun unknown-word ()
  (add-element '((p) ((a  :name "4") "")))
  (add-element '((h3) "2.1. Unknown Word"))
  (add-element '((h4)"Problem:"))
  (add-element '((p)"In comprehension, one of the words in the input cannot be processed by the grammar."))
  (add-element '((h4)"Diagnostic:"))
  (add-element '((p)"No more constructions can apply, but there are some words left in the input."))
  (add-element '((h4)"Repair:"))
  (add-element '((p)"A new, general lexical construction is created for this word. Word class and semantic category are initially underspecified. For its meaning, a new predicate is introduced."))
  (add-element '((h4)"Example:"))
  (make-benaic-demo-grammar-lexical-cxns)
  (add-element '((p)"The following sentence contains two words 'professor' and 'cat' that are not known by the grammar. The diagnostic triggers two times and repair construction for both 'professor' and 'cat' are created. Because a solution is found at the end, the constructions are added to the construction inventory."))
  (comprehend '("the" "professor" "likes" "the" "dog"))
  (add-element '((hr))))

(defun unknown-meaning ()
  (add-element '((p) ((a  :name "5") "")))
  (add-element '((h3) "2.2. Unknown Meaning"))
  (add-element '((h4)"Problem:"))
  (add-element '((p)" In production, one of the meaning components in the input cannot be expressed using the grammar. "))
  (add-element '((h4)"Diagnostic:"))
  (add-element '((p)"No more constructions can apply, but there are some unexpressed meanings left in the input."))
  (add-element '((h4)"Repair:"))
  (add-element '((p)"A new, general lexical construction is created for this meaning. Word class and semantic category are intially underspecified and a new meaing predicate is introduced. "))
  (add-element '((h4)"Example:"))
  (make-benaic-demo-grammar-lexical-cxns)
  (add-element '((p)"The following sentence contains a meaning predicate (elephant object) that is not known by the grammar. The diagnostic triggers and the repair creates a new lexical construction for expressing the elephant. The semantic function and lexical class are underspecified, and a new word is invented as the form for (elephant object)."))
  (formulate '((unique o) (linguist o) (deep-affection o o-2)(unique o-2) (elephant o-2)))
  (add-element '((hr))))

(defun phrasal ()
  (add-element '((p) ((a  :name "6") "")))
  (add-element '((h3) "2.3. Non-integrated Meaning Component"))
(add-element '((h4)"Problem:"))
  (add-element '((p)"At the end of processing, the analysis yields a semantic network with a non-integrated component."))
  (add-element '((h4)"Diagnostic:"))
  (add-element '((p)"No more constructions can apply and the resulting semantic network is not fully connected. "))
  (add-element '((h4)"Repair:"))
  (add-element '((p)"A new phrasal construction is introduced. It captures the observed word order and integrates the meaning component by making it co-referencing. "))
  (add-element '((h4)"Example:"))
  (make-benaic-demo-grammar-phrasal-cxns)
  (add-element '((p)"In the example below, all words are known by the grammar, but it only contains a noun phrase construction that consists of an article and a noun. The diagnostic discovers that not all words have been integrated in the structure, and creates a new phrasal construction. The phrasal construction integrates the adjective into the noun phrase, captures it word order constraints and makes it referent identical to that of the noun phrase. The phrasal-cxn node is not shown in orange, because it is also a solution node and therefore shown in dark green. When expanding it (click twice), you can still see the orange mention in the status."))
  (comprehend '("the" "green" "mouse")))

(defun matching-conflicts ()
  (add-element '((p) ((a  :name "7") "")))
  (add-element '((hr)))
  (add-element '((h3) "2.4. Matching Conflict"))
  (add-element '((h4)"Problem:"))
  (add-element '((p)"At the end of processing, the resulting semantic network is not fully connected because some grammatical constructions could not apply."))
  (add-element '((h4)"Diagnostic:"))
  (add-element '((p)"No more constructions can apply, the resulting semantic network is not fully connected and the other diagnostics did not trigger."))
  (add-element '((h4)"Repair:"))
  (add-element '((p)"The grammatical constructions are anti-unified with the current state and the anti-unified construction with the lowest cost is applied. Then, the resulting construction constrained towards the current state using pro-unification."))
  (add-element '((h4)"Example:"))
  (make-benaic-demo-anti-pro-unification-cxns)
  (add-element '((p) "The word order of the input utterance `un dîner formidable' conflicts with the word order specified in the NP-cxn. Anti-unification helps to arrive at an interpretation and then pro-unification is used to consolidate this repair."))
  (set-configuration *fcg-constructions* :use-meta-layer nil)
  (comprehend '("un" "dîner" "formidable"))
  (add-element '((hr)))
  (set-configuration *fcg-constructions* :use-meta-layer t)
  (add-element '((p) "Original Construction:"))
  (add-element (make-html (find-cxn 'np-cxn *fcg-constructions*)))
  (multiple-value-bind (form cip)
      (with-disabled-monitor-notifications (comprehend '("un" "dîner" "formidable") :cxn-inventory *fcg-constructions*))
    (setf *ts*  (car-resulting-cfs (cipn-car (get-last-cip-node cip)))))
  (add-element '((p) "Transient Structure:"))
  (add-element (make-html-fcg-light *ts* :feature-types (feature-types *fcg-constructions*)))
  ;; anti-unify *np-cxn* with *ts*, then pro-unify the anti-unified-cxn with *ts*
  (comprehend '("un" "dîner" "formidable"))
  (add-element '((hr)))
  (add-element '((p) "Now, we can also use the construction in formulation:"))
  (formulate-all '((quality splendid o-1) (status indefinite o-1) (meal dinner o-1)) :cxn-inventory *fcg-constructions* :n 2))

(defun run-demo ()
       (header)
       (menu)
       (diagn-rep-fcg)
       (routine-processing)
       (meta-level-processing)
       (library)
       (unknown-word)
       (unknown-meaning)
       (phrasal)
       (matching-conflicts))

;; (run-demo)

;; (create-static-html-page "Meta-Level Diagnostics and Repairs for FCG"  (run-demo))
