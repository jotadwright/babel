
;;#############################################################################;;
;;                                                                             ;;
;; Web demo 'neural heuristics for scaling constructional language processing' ;;
;;                                                                             ;;
;;#############################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clevr-grammar)

(in-package :clevr-grammar)
(use-package :web-interface)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Neural Heuristics for Scaling Constructional Language Processing."))
  (add-element '((p) "This web demonstration accompanies the paper:"))
  (add-element '((p) "Van Eecke, P., Nevens, J. &amp; Beuls, K. (XXXX). "((a :href "" :target "_blank") "Neural Heuristics for Scaling Constructional Language Processing")". Submitted to "((i) "Computational Linguistics") "."))
  (add-element '((p) "Explanations on how to use this demo are " ((a :href "https://www.fcg-net.org/projects/web-demonstration-guide/" :target "_blank") "here.")))
  (add-element '((hr)))
  (add-element '((p)  ((a :href "#1") "1. Introduction")))
  (add-element '((p)  ((a :href "#2") "2. Comprehension (depth-first strategy)")))
  (add-element '((p)  ((a :href "#3") "3. Comprehension (neural strategy)")))
  (add-element '((p)  ((a :href "#4") "4. Production (depth-first strategy)")))
  (add-element '((p)  ((a :href "#5") "5. Production (neural strategy)")))
  (add-element '((h3) ""))
  (add-element '((hr))))

;; (header-page)

(defun introduction ()
  (add-element '((a :name "1")))
  (add-element '((h2) "1. Introduction"))
  (add-element '((p) "This web demonstration presents full examples of the use of neural heuristics for optimizing the search processes involved in constructional language processing. In particular, it includes contrastive examples of the same utterance processed using the baseline depth-first search strategy with duplicate detection and using the neural strategy, both in the comprehension and production direction."))
  
    (add-element '((hr))))

;; (introduction)

(defun grammar ()
  (add-element '((h2) "2. The cookbook"))
  (add-element '((p) "All good pancakes start with a good recipe. And all good recipes consist of instructions for individual actions and their interdependencies. We will call a recipe, i.e. a sequence of instructions, a plan. Plans can be hierarchical. For baking pancakes for example, you need to make the dough, to prepare a pan, and to bake the dough in the pan. For making dough, you should collect ingredients, put them in a bowl and stir. Collecting ingredients comprises taking eggs, taking milk and taking flour."))
  (add-element '((p) "It is important to distinguish between primitive actions and high-level actions. Primitive actions are 'atomic instructions', i.e. instructions that are not split up into smaller instructions anymore (e.g. 'stir' in our example). High-level actions group one or more primitive actions and/or other high-level actions (e.g. make-pancake-dough or prepare-pan in our example). Besides this, we will assume the following terminology."))
  (add-element '((p) "-> By planning, we mean finding a sequence of primitive actions that lead to a state in which a given goal is satisfied."))
  (add-element '((p) "-> By plan recognition, we mean, given a sequence of primitive actions, retrieving a high-level action that includes (i.e. is the result of) the execution of the primitive actions."))
  (add-element '((p) "-> By plan prediction, we  mean, given a sequence of primitive actions, predicting a high-level action that can have this sequence of primitive actions as a start sequence, as well predicting the remaining primitive actions that will be taken in order to achieve this high-level action."))
  (add-element '((p) "Primitive actions are modelled as form-predicates, goals as meaning predicates. High-level actions are represented with constructions. Below, a construction-inventory containing four actions is shown. Let's have a look for example at the prepare-pan construction. We can see that we take butter (of which the status becomes 'used'), we turn on the stove (of which the states becomes 'heating'), we put the buttern in the pan and put the pan on the stove. The status of pan becomes 'in-use' and 'on-stove'."))
  (add-element (make-html *fcg-constructions* :expand-initially t)))

;; (grammar)

(defun planning-example ()
  (add-element '((hr)))
  (add-element '((h2) "3. Planning: from goal to plan"))
  (add-element '((p) "In planning, the goal and intial state are provided. Then the high-level actions in the construction-inventory can apply. Once the goal is reached, the plan is returned. It is a lot like formulation in language processing, but the 'utterance' to be formulated is here a sequence of primitive actions. For the sake of clarity, high-level goals are included in the plan. They are printed on a new line is capitals. Of course, they are not observable and only the primitive actions need to be executed."))
  (add-element '((p) "In the example below, we specify as a goal that (pancakes ?pc) should be part of the meaning of the final transient structure. The initial structure contains what is available, including possible ingredients and kitchen equipment. The construction inventory contains the possible actions."))
  (add-element '((p) "We can see that in the resulting structure the pancakes were made, and that the tabsco and the knife remained usused. The plan that is returned shows the primitive actions that need to be performed in small letters. The goal of making pancakes can be achieved by the this sequence of actions:  take-butter, put-butter-in-pan, turn-on-stove, put-pan-on-stove, take-eggs, take-flour, take-milk, take-bowl, take-whisk, put-ingredients-in-bowl, stir, put-dough-in-pan, bake-dough-in-pan-on-stove"))
  (plan
 :objects
 '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
   (pan p-1) (stove s1) (tabasco t-1) (knife k-1)
   (bowl b-2) (whisk w-2))
 :goal
 '((pancakes ?pc)) :n 1)
  (add-element '((hr))))

;; (planning-example)

(defun plan-recognition-example ()
  (add-element '((h2) "4. Plan recognition: from plan to goal"))
  (add-element '((p) "In plan recognition, the initial state and the plan (sequence of observed primitive actions) is given. The the actions from the construction-inventory are applied until one of the goal-templates is satisfied."))
  (add-element '((p)"Plan recognition is a lot like comprehension. But instead of a set of strings, a set of primitive actions is given, and instead of a meaning, a goal should needs to be found."))
  (add-element '((p) "In the example below, the give plan is the set of primitive actions to bake pancakes. The initial state consists again of the kitchen equipment and possible ingredients. The goal templates here are (pancakes ?pc) (cake ?c) and (beefsteak-with-fries ?bwf). The goal templates are a way to specify in which predicates you are most interested. It is not necessary to specify goal-templates, the system will then return the highest high-level action as the recognised goal."))
  (add-element '((p) "We can see that the goal that was found is indeed 'make-pancakes', with as subgoals 'prepare-pan', 'make-pancake-dough' and 'collect-pancake-ingredients'."))
  (recognise-plan
 :objects
 '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
   (pan p-1) (stove s1) (tabasco t-1) (knife k-1)
   (bowl b-2) (whisk w-2)) 
 :plan
 '((take-eggs) (take-flour) (take-milk)
   (take-butter) (put-butter-in-pan)
   (turn-on-stove) (put-pan-on-stove)
   (take-bowl) (take-whisk) (put-ingredients-in-bowl)
   (stir) (put-dough-in-pan) (bake-dough-in-pan-on-stove))
  :goal-templates
 '(((pancakes ?pancakes))
   ((cake ?cake))
   ((beefsteak-with-fries ?bwf)))
 :n 1))

;; (plan-recognition-example)

(defun plan-prediction-example ()
  (add-element '((hr)))
  (add-element '((h2) "5. Plan Prediction: from partial plan to hypothesized goal"))
  (add-element '((p) "In plan prediction, the initial state and a partial plan is provided and the task is to find the goal (and the remaining actions). Just like in plan recognition, a set goal-templates can be provided."))
  (add-element '((p) "Plan prediction is implemented as a combination of planning and plan recognition. First, we use the plan recognition direction to emulate the sequence of primitive actions that is provided. Then starting with the resulting state, we plan towards the goal templates. When a goal is found, the remaining actions that need to be performed are returned."))
  (add-element '((p) "In the example below, the plan recognition phase shows us that the provided sequence of primitive actions leads to a state in which the ingredients for pancakes are collected and a pan is prepared. In this state, none of the goal templates is satisfied. Then we plan towards the goal templates, see whether the goal can be achieved and what remaining actions need to be taken."))
  (predict-plan
 :objects
 '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
   (pan p-1) (stove s1) (tabasco t-1) (knife k-1)
   (bowl b-2) (whisk w-2))
 :plan ;; this is a partial plan
 '((take-eggs) (take-flour) (take-milk)
   (take-butter) (put-butter-in-pan)
   (turn-on-stove) (put-pan-on-stove))
 :goal-templates
 '(((pancakes ?pancakes)))
 :n 3))

;; (plan-prediction-example)

(create-static-html-page "Cooking with Fluid Construction Grammar"
(progn
  (header-page)
  (introduction)
  (grammar)
  (planning-example)
  (plan-recognition-example)
  (plan-prediction-example))
)
