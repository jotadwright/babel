;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modelling language learners' errors with Fluid Construction Grammar    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; 1. Load and set up FCG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)

; don't forget to open a web browser at http://localhost:8000

;; Larger font for text in <p> tags
(define-css 'main
            "p {font-size: 16pt}")


            

(defun my-head-menu ()
  ; the header file 
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (set-configuration *fcg-constructions* :form-predicates '(meets))
  (add-element
   '((h1) "Modelling language learners' errors with Fluid Construction Grammar"))
  (add-element '((p) "This is a web demo with which we present the efficacy of Fluid Construction Grammar
in accurately modeling language acquisition and learning processes."))
; how to make a link here to: "https:www.fcg-net.org/projects/web-demonstration-guide/"
  (add-element '((p)"Please check our " ((a :href "https:www.fcg-net.org/projects/web-demonstration-guide/") "web demonstration guide ") "to find out more on what you can see in the demo and what you can do with it."))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h1)  ((a :href "#intro") "I. Language acquisition through intention reading and pattern finding")))
  (add-element '((h1)  ((a :href "#fcg") "II. A quick guide to Fluid Construction Grammar")))
  (add-element '((h1)  ((a :href "#case-study") "III. German argument and information structure")))
  (add-element '((h1)  ((a :href "#model-german-grammar") "IV. A computational model for the German grammar")))
  (add-element '((h1)  ((a :href "#errors") "V. Detecting errors and providing feedback")))
  (add-element '((p :style "color:darkred") "DISCLAIMER: It is recommended to use Firefox or Safari to optimally explore the contents of this page.")))

;(my-head-menu)



(defun intro ()
  (add-element '((a :name  "intro")))
  (add-element '((h1) "I. Language acquisition through intention reading and pattern finding"))
  (add-element '((p) "According to usage-based constructionist theories of language acquisition (Tomasello 1992, Lieven 2003 and Goldberg 2019), two main general cognitive mechanisms are triggered during language acquisition:<i> intention reading and pattern finding.</i>"))
  (add-element '((ul) " <li> <b> Intention reading</b> </li> Children from a very young age are able to understand in general
terms the intentions of their interlocutors by analysing gestures and expressions, and by exploiting joint
attention and role comprehension.


                       <li> <b> Pattern finding </b> </li>  Children are able to apply the identification of similarities
and differences developed in their sensory-motor experiences on the structures of language."))

(add-element '((img :src "https://raisingchildren.net.au/__data/assets/image/0024/48057/child-development-relationshipsnarrow.jpg" :width "500" :height "300")))

(add-element '((img :src "https://www.greatschools.org/gk/wp-content/uploads/2015/08/Preschooler-pattern-360x180.jpg" :width "500" :height "300")))
  
  (add-element '((p) "The language structures acquired by children through communicative interactions, also identifiable as <i> modular constructions </i>, can be combined in different ways, according to their functions, and with increasing complexity. Moreover, they can be used for both understanding and generating linguistic utterances. Once a set of constructions and categories has been established, children can unleash their true linguistic <i> creative potential </i> (Van Eecke and Beuls 2018) to readily learn new abstractions when necessary and comprehend and formulate unseen utterances."))
  
  (add-element '((p) "We focused on the operationalization and representation through a computational model of the way language acquisition occurs by exploiting these cognitive capacities and " ((a :href "https:www.fcg-net.org") "Fluid Construction Grammar ."))))


(defun fcg-guide ()
  (add-element '((a :name  "fcg")))
  (add-element '((h1) "II. A quick guide to Fluid Construction Grammar"))
  (add-element '((p) "FCG (Fluid Construction Grammar) represents the only computational construction grammar framework that allows to monitor the <b>bidirectional</b> relations between language's <i> form and meaning </i> in a fully interpretable manner. It can be used to both <b>comprehend </b> given utterances and to <b>formulate </b> new ones."))

(add-element '((img :src "http://localhost/tutorial-call-main/fcg_features.png")))

  
  (add-element '((p) "The fundamental unit of FCG is the <b>construction</b>, which constitutes a linguistic unit with a  <b>semantic </b> part related to <b>meaning</b> and a <b>syntactic</b> part related to <b>form</b>. There can be different types of constructions, such as <i>lexical constructions</i> for nouns with a basic meaning and form, along with some basic semantic (e.g. <i>animacy</i>) and syntactic (e.g. <i>lex-class noun </i>) information, and <i>phrasal ones</i> that combine lexical constructions for nouns with other units, for example determiners or prepositions."))

  (add-element '((p) "These constructions interact together with each other by exploiting the principle of modularity. Just like Lego blocks simpler constructions contribute to build more complex constructions."))
  
  (add-element '((img :src "https://cdn.dribbble.com/users/891352/screenshots/3629157/lego-loop.gif")))

  (add-element '((p) "Concomitantly, constructions can map meaning to semantic categories, but also syntactic categories to a form, namely a word or phrase, and semantic categories to syntactic units. To represent how constructions are used by a language user or learner, they are arranged in a data structure called <i>transient structure </i>. It displays how the processing of a linguistic utterance happens during the application of constructions."))
  
  (add-element '((p) "<b>Some examples of constructions in comprehension and production</b>")))


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

(def-fcg-cxn dog-cxn
                           ; stores also association between "dog" and the construction in *vocabulary*
               ((?dog-word
                 (args (?referent))
                 (sem-cat (sem-class (physobj animate masculine)))
                 (syn-cat (lex-cat noun)
                          (number singular)))
                <-
                (?dog-word
                 (HASH meaning ((animal dog ?referent)))                     
                 --
                 (HASH form ((string ?dog-word  "dog"))))))

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
  (add-element '((p) "1. <i> A construction schema for the article `the'.</i>"))
  (add-element (make-html (find-cxn 'the-cxn cxn-inventory)))
  (add-element '((p) "2. <i> A construction schema for the noun `dog'.</i>"))
  (add-element (make-html (find-cxn 'dog-cxn cxn-inventory)))
  (add-element '((p) "3. <i> A construction schema for a simple noun-phrase. </i>"))
  (add-element (make-html (find-cxn 'simple-np-cxn cxn-inventory)))
  (add-element '((p) "(click on encircled + to zoom in and on encircled dot to zoom out):")))


(defun parse-example-the-dog (cxn-inventory)
  (add-element '((h1) "Comprehending an utterance: 'the dog'."))
  (add-element '((p) "Click once on a box in the application process 
to see the transient structure at that point and once again to see more detail: the transient structure before, the construction
schema that is applied, the transient structure after application (called the resulting structure), the bindings that were
used in the construction application process and the meaning so far (in case of comprehending) or 
the utterance so far (in case of formulating). Details are in a lower-level notation close to the
internal datastructures used by FCG. This notation is documented and used in Steels, L. (2011) Design patterns in Fluid Construction Grammar.
John Benjamins Pub, Amsterdam."))
  (comprehend '("the" "dog") :cxn-inventory cxn-inventory))

(defun parse-example-dog-the (inventory)
  (add-element '((h1) "Example of parsing an ungrammatical utterance: 'dog the'."))
  (add-element '((p) "If 'dog the' is given as input, the lexical constructions for 'dog' and 'the' still apply. However no noun phrase gets
built because the simple-np-cxn cannot apply due to a violation of the ordering constraint."))
  (comprehend '("dog" "the") :cxn-inventory inventory))

(defun produce-example-the-dog (inventory)
  (add-element '((h1) "Producing an utterance: 'the dog'."))
  (add-element '((p) "Producing happens using the exact same construction inventory and the same processing engine, but now the matcher uses
the production-locks rather than the comprehension-locks."))
  (formulate '((specificity definite obj-1)(animal dog obj-1)) :cxn-inventory inventory))

(defun simple-np-example () 
  (let ((inventory (make-basic-grammar-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-simple-np inventory)
   (parse-example-the-dog inventory) (parse-example-dog-the inventory) (produce-example-the-dog inventory)))


(defun German-case-study ()
  (add-element '((a :name "case-study")))
  (add-element '((h1) "III. German argument and information structure"))
  (add-element '((img :src "http://localhost/tutorial-call-main/case_study.png"  :width "500" :height "300")))
  (add-element '((p) "In this research project, we focus on the analysis of 1,487 transcribed and annotated spoken productions of 36 students of German as a second language from the academic program in Languages and Literature at the University of Ghent, collected in an oral elicited imitation task (Baten and Cornillie 2019). The learners heard an audio stimulus containing transitive,ditransitive and intransitive verbs, along with noun and prepositional phrases, which they had to match to one of two displayed pictures with different semantic roles. Subsequently, they needed to produce an oral response describing the selected picture, which should have corresponded to the initially heard utterance meaning (see image below)."))

(add-element '((img :src "http://localhost/tutorial-call-main/example_case.jpg"  :width "500" :height "550")))
(add-element '((p) "However, sometimes the productions of students deviated from the stimuli they received either by argument or information structure, as well as beccause of grammatical <b> errors in the form or meaning of the sentences </b>.")))



(defun German-grammar ()
  (add-element '((a :name "model-german-grammar")))
  (add-element '((h1) "IV. A computational model for the German grammar"))
  (add-element '((p) "One of the distinctive aspects of German is its rich and articulated case system. It can be a rather complex concept to learn, especially for those learners whose native languages do not exhibit such a range of case options. In addition, the presence of syncretism between different cases makes the acquisition, but also the modeling, of this system more challenging."))
  (add-element '((p) "In our case, the cases and the distribution of parts of speech in the sentence were of great importance. Therefore, our task was first to create a model of the German grammar capable of taking into account the case system, the argument structure of transitive, intransitive and ditransitive verbs, but also to encapsulate the features of information structure."))
  (add-element '((p) "We do so exploiting the highly customizable nature of FCG constructions and their versatility, as well as the AMR formalism and van Trijp's case matrices"))

(add-element '((img :src "http://localhost/tutorial-call-main/german_grammar.jpg" :width "650" :height "220"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;:EXAMPLES of DO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-fcg-constructions german-case-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set)
                  (case sequence))
  :fcg-configurations ((:max-nr-of-nodes . 40000)
                       (:hide-features footprints sem-cat form boundaries)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network :connected-structure)
                       ;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :cxn-sets) ;; returns all cxns at once
                       (:node-tests :mal-cxn-applied :restrict-search-depth :restrict-nr-of-nodes :check-duplicate)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-sets) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       ;; cxn sets
                       (:parse-order cxn  mal-cxn)
                       (:production-order cxn mal-cxn )
                       ;; goal tests
                       (:production-goal-tests
                        :no-applicable-cxns :connected-structure
                        :no-meaning-in-root)))


(defmethod cip-node-test ((node cip-node) (mode (eql :mal-cxn-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'mal-cxn)
    (and (push 'mal-cxn-applied (statuses node))
         t)
      t
      ))

(pushnew '((mal-cxn-applied) .  "#eb4034;") *status-colors*)

(def-fcg-cxn der-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nm ?nm - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (?gen - ?gf - ?gp)    ;genitive feminine
                               (?df - ?df - -)      ;sing, masc, fem, neut, plural
                               (?s ?nm ?f - ?gp))))   ;sing, masc, fem, neut, plural

               --
               (HASH form ((string ?the-word "der")))))
             :disable-automatic-footprints t)

(def-fcg-cxn dem-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)      ;sing, masc, fem, neut, plural
                               (+ ?dm - ?dn -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "dem")))))
             :disable-automatic-footprints t)

(def-fcg-cxn das-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nn - - ?nn -)    ;nom, acc, gen, dat  (nom masculine)
                               (?an - - ?an -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (- - - - -)
                               (+ - - + -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "das")))))
             :disable-automatic-footprints t)


(def-fcg-cxn den-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)        
                               (?am ?am - - -)        
                               (- - - - -)          
                               (?dp - - - ?dp)
                               (?am ?am - - ?dp))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "den")))))
             :disable-automatic-footprints t)


(def-fcg-cxn Doktor-cxn
             ((?doctor-word
               (referent ?d)                  
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
                        
              <-
              (?doctor-word
               (HASH meaning ((doctor ?d)))                     
               --
               (HASH form ((string ?doctor-word  "Doktor"))))))




(def-fcg-cxn Buch-cxn
             ((?book-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
                        (sem-cat (animacy inanimate)))
              <-
              (?book-word                            
               (HASH meaning ((book ?b)))                    
               --
               (HASH form ((string ?book-word  "Buch"))))))



(def-fcg-cxn Clown-cxn
             ((?clown-word                        
               (referent ?c)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?clown-word                            
               (HASH meaning ((clown ?c)))                    
               --
               (HASH form ((string ?clown-word  "Clown"))))))

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case))
               (sem-cat (animacy ?animacy))
               (subunits (?article ?noun))
               (boundaries (leftmost-unit ?article)
                           (rightmost-unit ?noun)))
              (?article
               (referent ?x)
               (part-of-noun-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?article
               --
               (syn-cat (lex-class article)
                        (case ?case)))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))
              ))
             :disable-automatic-footprints t)

(def-fcg-cxn verkauft-cxn
             ((?sell-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?v))  
                        
              <-
              (?sell-word                           
               (HASH meaning ((verkaufen-01 ?v)))
               --
               (HASH form ((string ?sell-word  "verkauft"))))))

(def-fcg-cxn ditransitive-argument-structure-cxn
             ((?ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (referent ?arg2))
              
              (?ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --
               )))

(def-fcg-cxn topic-arg0-arg1-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-receiver-unit)
                           (meets ?rightmost-receiver-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (referent ?arg0)
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              ))

(defun display-constructions-german-grammar (cxn-inventory)
  (add-element '((a :name  "noun-phrase")))
  (add-element '((p) "1. <i> Some noun-phrase constructions examples.</i>"))
  (add-element (make-html (find-cxn 'noun-phrase-cxn cxn-inventory)))
  (add-element '((p) "2. <i> A construction schema for the argument structure of the ditransitive utterance `der Doktor verkauft dem Clown das Buch'.</i>"))
  (add-element (make-html (find-cxn 'ditransitive-argument-structure-cxn cxn-inventory)))
  (add-element (make-html (find-cxn 'topic-arg0-arg1-arg2-information-structure-cxn cxn-inventory)))
  )

(defun parse-example-dem-Clown (inventory)
  (add-element '((h1) "Comprehending a dative noun-phrase 'dem Clown'."))
  (comprehend '("dem" "Clown") :cxn-inventory cxn-inventory))

(defun parse-example-ditransitive-arg-structure (inventory)
  (add-element '((h3) "Parsing a ditransitive clause: `der Doktor verkauft dem Clown das Buch'."))
  (comprehend '("der" "Doktor" "verkauft" "dem" "Clown" "das" "Buch") 
         :cxn-inventory inventory))

(defun ditransitive-example () 
  (let ((inventory (make-german-grammar-cxns)))
    (set-configuration inventory :form-predicates '(meets))
   (display-constructions-german-grammar inventory)
   (parse-example-dem-Clown inventory)
   (parse-example-ditransitive-arg-structure inventory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;incorrect receiver case

(def-fcg-cxn incorrect-receiver-in-ditransitive-argument-structure-cxn
             ((?double-accusative-incorrect-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role direct-object))
               (error-cat (error incorrect-case-selection-for-this-argument-role)
                          (reason this-argument-should-be-a-receiver-in-dative-case-not-another-object-in-accusative)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                      (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))) )
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                        )
               (referent ?arg1e)
                --
              (syn-cat 
                (lex-class noun-phrase)
                        )
              (sem-cat (animacy animate))
              (referent ?arg1e))
              
              (?double-accusative-incorrect-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              ;(:arg1 ?v ?arg1e)
                              (:arg1-error ?v ?arg1e)
                              (:arg2 ?v missing-for-error-should-be ?arg1e)
                              ))                  
               --
               ))
             :cxn-set mal-cxn)

(def-fcg-cxn topic-arg0-arg1-arg2-incorrect-acc-information-structure-cxn
             (
              <-
              (?incorrect-argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-receiver-unit)
                           (meets ?rightmost-receiver-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (referent ?arg0)
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              )
             :cxn-set mal-cxn)


(defun total-demo ()
  (my-head-menu)
  (intro)
  (fcg-guide)
  (simple-np-example)
  (German-case-study)
  (German-grammar)
  (ditransitive-example)
  )

; (total-demo)
