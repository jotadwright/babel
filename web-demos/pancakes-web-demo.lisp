
;;###########################################################################;;
;;                                                                           ;;
;; Web demo file for the planning package                                    ;;
;;                                                                           ;;
;;###########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :planning)

(in-package :planning)
(progn
  (deactivate-all-monitors)
  (activate-monitor trace-planning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the example recipe  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions recipe
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (status set))
  :fcg-configurations ((:cxn-supplier . :ordered-by-label-and-score)
                       (:render-mode . :render-final-state)
                       (:queue-mode . :depth-first)
                       (:max-search-depth . 100)
                       (:node-tests :check-duplicate :restrict-search-depth)
                       (:production-goal-tests :goal-reached)
                       (:parse-goal-tests :one-of-goals-reached)
                       (:production-order actions)
                       (:parse-order actions)
                       (:create-initial-structure-mode . :objects-and-fluents)
                       (:plan-extraction-mode . :extract-hierarchical-plan-from-form)
                       (:goal-extraction-mode . :cxn-names-in-order-of-application)
                       (:state-extraction-mode . :same-cfs))
  :visualization-configurations ((:with-search-debug-data . t))
  :disable-automatic-footprints t

(def-fcg-cxn collect-pancake-ingredients
     ((?pancake-ingredients
       (args (?f ?e ?m))
       (type pancake-ingredients)
       (subunits (?flour ?eggs ?milk)))
      (?flour
       (status (used)))
      (?eggs
       (status (used)))
      (?milk
       (status (used)))
      <-
      (?flour
       (referent ?f)
       (object flour)
       (status (NOT used))
       --
       (referent ?f)
       (object flour)
       (status (NOT used)))
      (?eggs
       (referent ?e)
       (object eggs)
       (status (NOT used))
       --
       (referent ?e)
       (object eggs)
       (status (NOT used)))
      (?milk
       (referent ?m)
       (object milk)
       (status (NOT used))
       --
       (referent ?m)
       (object milk)
       (status (NOT used)))
      (?pancake-ingredients
       --
       (HASH form ((take-eggs)
                   (take-flour)
                   (take-milk)))))
  :cxn-set actions)

(def-fcg-cxn prepare-pan
     ((?stove
       (status (heating)))
      (?butter
       (status (used)))
      (?pan
       (subunits (?butter ?stove))
       (status (in-use on-stove)))
      <-
      (?stove
       (referent ?s)
       (object stove)
       --
       (referent ?s)
       (object stove))
      (?pan
       (referent ?p)
       (object pan)
       (status (NOT in-use))
       --
       (referent ?p)
       (object pan)
       (status (NOT in-use))
       (HASH form ((take-butter)
                   (put-butter-in-pan)
                   (turn-on-stove)
                   (put-pan-on-stove))))
      (?butter
       (referent ?b)
       (object butter)
       (status (NOT used))
       --
       (referent ?b)
       (object butter)
       (status (NOT used))))
  :cxn-set actions)

(def-fcg-cxn make-pancake-dough
     ((?pancake-dough
       (args (?d))
       (type pancake-dough)
       (subunits (?ingredients ?bowl ?whisk)))
      (?ingredients
       (status (stirred)))
      (?bowl
       (status (used)))
      <-
      (?ingredients
       (referent ?i)
       (type pancake-ingredients)
       (status (NOT stirred))
       --
       (referent ?i)
       (type pancake-ingredients)
       (status (NOT stirred)))
      (?bowl
       (referent ?b)
       (object bowl)
       (status (NOT used))
       --
       (referent ?b)
       (object bowl)
       (status (NOT used))
       )
      (?whisk
       (referent ?w)
       (object whisk)
      --
      (referent ?w)
      (object whisk))
      (?pancake-dough
       --
       (HASH form ((take-bowl)
                   (take-whisk)
                   (put-ingredients-in-bowl)
                   (stir)))))
  :cxn-set actions)

(def-fcg-cxn make-pancakes
     ((?pancakes
       (meaning ((pancakes ?pc)))
       (type pancakes)
       (subunits (?pancake-dough ?pan))
       (status finished))
      (?pancake-dough
       (status (used)))
      <-
      (?pancake-dough
       (referent ?d)
       (type pancake-dough)
       (status (NOT used))
       --
       (referent ?d)
       (type pancake-dough)
       (status (NOT used)))
      (?pan
       (object pan)
       (status (in-use on-stove))
       (subunits (?stove))
       --
       (object pan)
       (status (in-use on-stove))
       (subunits (?stove)))
      (?stove
       (object stove)
       (status (heating))
       --
       (object stove)
       (status (heating)))
      (?pancakes
       --
       (HASH form ((put-dough-in-pan)
                   (bake-dough-in-pan-on-stove)))))
  :cxn-set actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "Cooking with Fluid Construction Grammar &#9832;"))
  (add-element '((h3) "Brought to you by Paul Van Eecke and Katrien Beuls"))
  (add-element '((hr))))

;; (clear-page)
;; (header-page)

(defun introduction ()
  (add-element '((h2) "1. Planning in Fluid Construction Grammar"))
  (add-element '((p) "The problems of planning and plan recognition are strikingly similar
to the problems of language production and comprehension. In this web demonstration, we show that the representations
and processing techniques used in computational construction grammar can serve as a common framework for implementing
this class of problems. Concretely, we show how Fluid Construction Grammar, a bidirectional computational framework
for constructional language processing, can be used to solve planning, (hierarchical) plan recognition and plan prediction problems with
examples from the domain of cooking. Concretely, we will focus on delicious pancakes and how to bake them."))
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
