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

  
  (add-element '((p) "The fundamental unit of FCG is the construction, which constitutes a linguistic unit with a  <b>semantic </b> part related to <b>meaning</b> and a <b>syntactic</b> part related to <b>form</b>. There can be different types of constructions, such as <i>lexical constructions</i> for nouns with a basic meaning and form, along with some basic semantic (e.g. <i>animacy</i>) and syntactic (e.g. <i>lex-class noun </i>) information, and <i>phrasal ones</i> that combine lexical constructions for nouns with other units, for example determiners or prepositions."))

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



(defun total-demo ()
  (my-head-menu)
  (intro)
  (fcg-guide)
  (simple-np-example)
  )

; (total-demo)
