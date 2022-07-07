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
            "p {font-size: 10pt}")

(defun my-head-menu ()
  ; the header file 
  (clear-page)
  (deactivate-all-monitors) ; just to be sure that there is no other stuff 
  (activate-monitor trace-fcg)
  (set-configuration *fcg-constructions* :form-predicates '(meets))
  (add-element
   '((h1) "Modelling language learners' errors with Fluid Construction Grammar"))
  (add-element '((p) "This is a web demo with which we present the efficacy of Fluid Construction Grammar in accurately modeling language acquisition and learning processes."))
; how to make a link here to: "https:www.fcg-net.org/projects/web-demonstration-guide/"
  (add-element '((p)"Please check our " ((a :href "https:www.fcg-net.org/projects/web-demonstration-guide/") "web demonstration guide ") "to find out more on what you can see in the demo and what you can do with it."))
  (add-element '((p) "This demonstration has the following parts:"))
  (add-element '((h3)  ((a :href "#intro") "I. Language acquisition through intention reading and patter finding")))
  (add-element '((h3)  ((a :href "#fcg") "II. A quick guide to Fluid Construction Grammarn")))
  (add-element '((h3)  ((a :href "#case-study") "III. German argument and information structure")))
  (add-element '((h3)  ((a :href "#model-german-grammar") "IV. A computational model for the German grammar")))
  (add-element '((h3)  ((a :href "#errors") "V. Detecting errors and providing feedback")))
  (add-element '((p :style "color:darkred") "DISCLAIMER: It is recommended to use Firefox or Safari to optimally explore the contents of this page.")))

;(my-head-menu)



(defun intro ()
  (add-element '((a :name  "intro")))
  (add-element '((h2) "I. Language acquisition through intention reading and pattern finding"))
  (add-element '((p) "According to usage-based constructionist theories of language acquisition (Tomasello 1992, Lieven 2003 and Goldberg 2019), two main general cognitive mechanisms are triggered during language acquisition:<i> intention reading and pattern finding.</i>"))
  (add-element '((ul) " <li> <b> Intention reading</b> </li> Children from a very young age are able to understand in general terms the intentions of their interlocutors by analysing gestures and expressions, and by exploiting joint attention and role comprehension.


                       <li> <b> Pattern finding </b> </li>  Children are able to apply the identification of similarities and differences developed in their sensory-motor experiences on the structures of language."))

(add-element '((img :src "https://raisingchildren.net.au/__data/assets/image/0024/48057/child-development-relationshipsnarrow.jpg" :width "500" :height "300")))

(add-element '((img :src "https://www.greatschools.org/gk/wp-content/uploads/2015/08/Preschooler-pattern-360x180.jpg" :width "500" :height "300")))
  
  (add-element '((p) "The language structures acquired by children through communicative interactions, also identifiable as <i> modular constructions </i>, can be combined in different ways, according to their functions, and with increasing complexity. Moreover, they can be used for both understanding and generating linguistic utterances. Once a set of constructions and categories has been established, children can unleash their true linguistic <i> creative potential </i> (Van Eecke and Beuls 2018) to readily learn new abstractions when necessary and comprehend and formulate unseen utterances."))
  
  (add-element '((p) "We focused on the operationalization and representation through a computational model of the way language acquisition occurs by exploiting these cognitive capacities and " ((a :href "https:www.fcg-net.org") "Fluid Construction Grammar ."))))




(defun total-demo ()
  (my-head-menu)
  (intro))

; (total-demo)