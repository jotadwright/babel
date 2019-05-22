
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This File contains a tutorial for using the linguistic pathways visualisation tool ;;
;; File by Sébastien - 08/2017                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(asdf:make :fcg)
(in-package :fcg)



(defparameter *english-grammar* nil)
(defparameter *pancake-grammar* nil)

;; Load English grammar
(asdf:make :english-grammar)
(setf *english-grammar* *fcg-constructions*)

;; Load pancake grammar
(asdf:make :planning)
(in-package :planning)
(load "sharing/fcg-for-planning/pancakes.lisp")
(setf fcg::*pancake-grammar* (make-recipe-cxns))
(in-package :fcg)

;; Show constructional dependencies in web interface
(set-configuration (visualization-configuration *english-grammar*)
                   :show-constructional-dependencies t)
(set-configuration (visualization-configuration *pancake-grammar*)
                   :show-constructional-dependencies t)


(defun header ()
  (clear-page)
  (add-element '((hr)))
  (add-element '((h1) "Constructions at Work!"))
  (add-element '((h1) "Visualising Linguistic Pathways for Computational Construction Grammar."))
  (add-element '((h3) "S&#233;bastien Hoorens, Katrien Beuls and Paul Van Eecke"))
  (add-element '((p) "This web demo accompanies the paper:" ((br))
                 "S. Hoorens, K. Beuls, and P. Van Eecke (2017). Constructions at Work! Visualising Linguistic Pathways for Computational Construction Grammar. "
                 ((i) "Proceedings of the 29th Benelux Conference on Artificial Intelligence (BNAIC 2017). ") "Submitted."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p) ((a :name "start") "Menu:")))
  (add-element '((p)  ((a :href "#WD-1") "1.  Introduction")))
  (add-element '((p)  ((a :href "#WD-2") "2.  Linguistic Pathways in the Web Interface")))
  (add-element '((p)  ((a :href "#WD-3") "3.  Setting Visualisation Options")))
  (add-element '((p)  ((a :href "#WD-4") "4.  Beyond Language Processing")))
  (add-element '((hr))))

(defun introduction ()
  (add-element '((a :name "WD-1")))
  (add-element '((h2) "1.  Introduction"))
  (add-element '((p) "Computational construction grammar combines well known concepts from artificial intelligence, linguistics and computer science into fully operational language processing models. These models allow to map  an utterance to its meaning representation ("
                 ((i) "comprehension")
                 "), as well as to map a meaning representation to an utterance ("
                 ((i) "formulation")
                 "). The processing machinery is based on the unification of usage-patterns that combine morpho-syntactic and semantic information ("
                 ((i) "constructions")
                 ") with intermediate structures that contain all information that is known at a certain point in processing ("
                 ((i) "transient structures")
                 "). Language processing is then implemented as a search process, which searches for a sequence of constructions ("
                 ((i) "a linguistic pathway")
                 ") that successfully transforms an initial transient structure containing the input into a transient structure that qualifies as a goal. For larger grammars, these linguistic pathways become increasingly more complex, which makes them difficult to interpret and debug for the human researcher. In order to accommodate this problem, we present a novel approach to visualising the outcome of constructional language processing. The linguistic pathways are visualised as graphs featuring the applied constructions, why they could apply, with which bindings, and what information they have added. The visualisation tool is concretely implemented for Fluid Construction Grammar, but is also of interest to other flavours of computational construction grammar, as well as more generally to other unification-based search problems of high complexity."))
  (add-element '((hr))))

(defun web-interface ()
  (deactivate-all-monitors)
  (add-element '((a :name "WD-2")))
  (add-element '((h2) "2.  Linguistic Pathways in the Web Interface"))

  (add-element '((p) "We can enable the visualisation for constructional dependencies in the web interface by writing "
                 ((code) "(set-configuration (visualization-configuration *fcg-constructions*) :show-constructional-dependencies t)")
                 ". When hovering over some of the elements in the visualisation, information about the bindings is shown in a tooltip. This is what we get when we comprehend "
                 ((i)"\"the cat will jump\"") " with the "
                 ((a :href "https://www.fcg-net.org/fcg-interactive/" :target "_blank") "English")" grammar:"))  
  (setf *fcg-constructions* *english-grammar*)
  (set-configuration (visualization-configuration *fcg-constructions*) :colored-paths nil)
  (set-configuration (visualization-configuration *fcg-constructions*) :labeled-paths nil)
  (set-configuration (visualization-configuration *fcg-constructions*) :trace-units nil)
  (activate-monitor trace-fcg)
  (comprehend "the cat will jump")
  (deactivate-all-monitors)
  (add-element '((p) "The constructional dependencies graph contains 17 blue node clusters, one for each construc- tion that has been applied. On the left side of the graph, we can see that there are four entry points, i.e. node clusters with no incoming edges. The comprehension locks of these four con- structions, jump-morph, will-morph, the and determiner-operation-cxn, only require strings that are found in the input utterance and do not need any features added by previous construction applications. They create one lexical unit each, which is shown on a green background. After the the construction has applied and created a ?definite-article unit, the determiner-operation- cxn can match its ?determiner unit on this unit and create a new ?np unit. The cat-noun-lex and jump constructions can respectively match on the units created by the jump-morph and cat-noun-morph constructions, adding information to these units, but not creating any new ones."))
  (add-element '((p) "Then, we can see that the lexical units for the modal auxiliary and the lexical main verb are both affected by the marked-modality-cxn, which creates a new ?vp unit. This ?vp unit is then modified by the vp-cxn, non-perfect-cxn and the non-progressive-cxn. The determinednoun-cxn adds information into lexical units for the noun and the definite article, as well as to the ?np unit. The lexical unit for the noun is further affected by the singular-cxn."))
  (add-element '((p) "After that, the subject and verb come together. The subject-verb-cxn matches on the ?np and ?vp units, and creates a new ?clause unit. These three units are then further extended by the declarative-main-clause-verb-cxn and the intrasitive-cxn."))
  (add-element '((hr)))
  (add-element '((br)))
  (add-element '((br)))
  (add-element '((p) "The visualisation also works for formulation. When formulating " ((i)"\"the lazy cat\"") ", we get the following trace:"))
  (activate-monitor trace-fcg)
  (formulate '((ENGLISH-GRAMMAR::CATEGORY ENGLISH-GRAMMAR::LAZY O-290 R-638 REF-638)
               (ENGLISH-GRAMMAR::REFERENT-STATUS ENGLISH-GRAMMAR::IDENTIFIABLE-REF REF-638 O-290)
               (ENGLISH-GRAMMAR::OBJECT ENGLISH-GRAMMAR::CAT R-638 X-3920 REF-638)
               (ENGLISH-GRAMMAR::NUMBER-VALUE ENGLISH-GRAMMAR::SINGULAR R-638)))
  (deactivate-all-monitors)
  (add-element '((hr))))

(defun labeled-graphs ()
  (add-element '((a :name "WD-3")))
  (add-element '((h2) "3.  Setting Visualisation Options"))
  (deactivate-all-monitors)
  (add-element '((p) "There are three options through which the visualisation can be customized: " ((code) ":labeled-paths") ", "
                 ((code) ":colored-paths") " and " ((code) ":trace-units") ". These options can be set in the same manner as we did for "
                 ((code) ":show-constructional-dependencies") ". When setting " ((code) ":colored-paths") " on and setting "
                 ((code) ":labeled-paths") " to show the full edge labels, we obtain the following figure for comprehension of " ((i) "\"the cat\"") ":"))
  (set-configuration (visualization-configuration *fcg-constructions*) :colored-paths t)
  (set-configuration (visualization-configuration *fcg-constructions*) :labeled-paths 'full)
  (set-configuration (visualization-configuration *fcg-constructions*) :trace-units nil)
  (add-element (make-html (application-path "the cat" :direction '<-)))
  
  (add-element '((p) "Below is another example in which " ((code) ":labeled-paths") " and " ((code) ":colored-paths")
                 " have been disabled, and " ((code) ":trace-units") " has been set to " ((code) "'(\"?VP\")")
                 ", for comprehension of " ((i) "\"the cat will jump\"") ":"))
  (set-configuration (visualization-configuration *fcg-constructions*) :colored-paths nil)
  (set-configuration (visualization-configuration *fcg-constructions*) :labeled-paths nil)
  (set-configuration (visualization-configuration *fcg-constructions*) :trace-units '("?VP"))
  (add-element (make-html (application-path "the cat will jump" :direction '<-)))
  (add-element '((hr))))

(defun pancakes ()
  (add-element '((a :name "WD-4")))
  (add-element '((h2) "4.  Beyond Language Processing"))
  (add-element '((p) "Finally, we can also visualise the constructional dependencies that occur during planning. Here is the figure we get for planning with the pancake grammar, setting "
                 ((code) ":labeled-paths") " to " ((code) "'no-bindings") ":"))
  (setf *fcg-constructions* *pancake-grammar*)
  ; (activate-monitor planning::trace-planning)
  (deactivate-all-monitors)
  (set-configuration (visualization-configuration *fcg-constructions*) :colored-paths nil)
  (set-configuration (visualization-configuration *fcg-constructions*) :labeled-paths 'no-bindings)
  (set-configuration (visualization-configuration *fcg-constructions*) :trace-units nil)
  (multiple-value-bind (ranked-plans final-states cip solutions)
      (planning::plan
       :objects
       '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
         (pan p-1) (stove s-1) (tabasco t-1) (knife k-1)
         (bowl b-2) (whisk w-2))
       :goal
       '((pancakes ?pc ?d ?pp ?s)) :n 1)
    (add-element (make-html (analyse-solution (first solutions) '->))))
  (add-element '((p) "We can see that in total, 11 actions have been performed to make the pancakes. 7 actions were take-... actions that could be independently performed based on the initial state. Then, a collect-pancakes-ingredients action matched on the ?flour, ?eggs and ?milk units, creating a new ?pancake-ingredients unit. The make-pancake-dough action then matched on the ?pancake- ingredients, ?whisk and ?bowl units and created a new ?pancake-dough unit. The prepare-pan action matches on the ?pan, ?butter and ?stove units and creates a new ?prepared-pan unit. Finally, the make-pancakes action matches on the ?prepared-pan unit and the pancake-dough unit and creates a new pancakes unit. The arrows are labelled with the features on which the actions match, and the bindings of these features. When looking at the make-pancakes action for example, we can see that it matches on a ?pancake-dough unit that does not have baked among its properties (marked in red in the figure). The make-pancakes construction will add baked to the properties of this unit, and indeed, pancake dough can only be baked a single time."))
  ; (deactivate-all-monitors)
  (add-element '((hr))))

(defun make-demo ()
  (create-static-html-page "Visualising Linguistic Pathways"
    (header)
    (introduction)
    (web-interface)
    (labeled-graphs)
    (pancakes)
    )
    )



;; (make-demo)
