
;;###########################################################################;;
;;                                                                           ;;
;; Web demo file for the planning package                                    ;;
;;                                                                           ;;
;;###########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :planning)

(in-package :planning)
(progn
  (deactivate-all-monitors)
  (activate-monitor trace-planning))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the example grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions planning
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :fcg-configurations ((:cxn-supplier . :ordered-by-label-and-score)
                       (:queue-mode . :breadth-first)
                       (:render-mode . :render-final-state)
                       (:max-search-depth . 5)
                       (:production-goal-tests :goal-reached)
                       (:parse-goal-tests :no-applicable-cxns)
                       (:production-order actions)
                       (:parse-order actions)
                       (:create-initial-structure-mode . :objects-and-fluents))
  :visualization-configurations ((:with-search-debug-data . t))

;; Move action
(def-fcg-cxn move-act
     ((?move-unit
       (args (?ag ?l1 ?l2))
       (action move))
      (?agent-at-unit
       (fluent agent-at)
       (meaning ((agent-at ?ag ?l1) >> (agent-at ?ag ?l2)))
       (args (?ag ?l1) >> (?ag ?l2)))
      (?empty-unit
       (fluent empty)
       (meaning ((empty ?l2) >> (empty ?l1)))
       (args (?l2) >> (?l1)))
      <-
      (?connected-unit
       (args (?l1 ?l2))
       (fluent connected)
       --
       (args (?l1 ?l2))
       (fluent connected))
      (?agent-at-unit
       (args (?ag ?l1))
       (fluent agent-at)
       --
       (args (?ag ?l1))
       (fluent agent-at))
      (?empty-unit
       (fluent empty)
       (args (?l2))
       --
       (fluent empty)
       (args (?l2)))
      (?move-unit
       --
       (HASH form ((move ?ag ?l1 ?l2 )))))
  :cxn-set actions
  :disable-automatic-footprints t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web demo                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun header-page ()
  (clear-page)
  ;; Title
  (add-element '((hr)))
  (add-element '((h1) "A constructional approach to planning, plan emulation and plan recognition.")))

;; (clear-page)
;; (header-page)

(defun introduction ()
  (add-element '((hr)))
  (add-element '((h2) "1. Planning in Fluid Construction Grammar"))
  (add-element '((p) "The problems of planning, plan emulation and plan recognition are strikingly similar
to those of language production and comprehension. In this demo, we aim to demonstrate these similarities by
implementing an architecture supporting planning, plan emulation and plan recognition in the language processing
framework Fluid Construction Grammar.")))

;; (introduction)

(defun example-grammar ()
  (add-element '((hr)))
  (add-element '((h2) "2. An example situation"))
  (add-element '((p) "In this demo, we will plan for the following situation. We have a set of agents (a-1 to a-n) and a set of locations (l-1 to l-n). The locations can have different properties. They can be empty (e.g. (empty l-2)) meaning that no agent is at this location. They can also be sleeping-locations (e.g. (sleeping-location l-3)), meaning that an agent can sleep there. They can also be connected to other locations (e.g. (connected l-1 l-2)). Agents can be a certain location (e.g. (agent-at a-1 l1))."))
  (add-element '((p) "The grammar consists of only one construction, the move action. As shown below, the move action can apply if an agent is at a certain location and this location is connected to another location that is empty. If the action applies, it changes the location of the agent and the 'emptyness' of the source and target locations. Note that the => sign denotes 'changes to'. For the moment the overwriting operator is not visualised in sets yet, but also the args change accordingly."))
  (add-element (make-html (first (constructions *fcg-constructions*)))))

;; (example-grammar)

(defun planning-example ()
  (add-element '((hr)))
  (add-element '((h2) "3. Planning: from goal to plan"))
  (add-element '((p) "In planning, the goal and intial state are provided. Then the actions can apply. Once the goal is reached, the plan is returned. It is a lot like formulation in language processing, but the 'utterance' to be formulated is here the set of moves or 'plan'."))
  (add-element '((p) "In the example below, we specify as a goal that agent age0 should find a sleeping-location. The initial structure shows which locations exist, which are empty, at which location the agent is and which locations are sleeping-locations. Then, the move-action can apply. As we are interested in the shortest plan and as we could easily get caught in a circular plan, we search for a solution in a breadth-first manner and limit the number of desired solutions to 3."))
  (add-element '((p) "We can see that the first, shortest, solution is found in only one step: the agent can move to l4, which is a sleeping-location connected to l1. Alternatively, he can move from l1 to l2 and then to l3, which is also a sleeping-location. Finally, he could go to l2, back to l1 and then to l4."))
  (plan
   :objects
   '((location l1) (location l2) (location l3) (location l4)
     (agent age0))
   :fluents
   '((empty l2) (empty l3) (empty l4)
     (sleeping-location l3) (sleeping-location l4)
     (connected l2 l1) (connected l3 l2) (connected l1 l2) (connected l2 l3) (connected l1 l4) (connected l4 l1)
     (agent-at age0 l1))
   :goal
   '((agent-at age0 ?l)
     (sleeping-location ?l)) :n 3)
  )

;; (planning-example)

(defun plan-emulation-example ()
  (add-element '((hr)))
  (add-element '((h2) "4. Plan Emulation: from plan to goal"))
  (add-element '((p) "In plan emulation, the plan and the initial state is provided to the agent, and the agent should find the goal. As it is very difficult and situation-dependent to find which properties of the final state could be meaningful goals (is it more meaningful that a location is empty or that an agent is at a certain location?), we provide a set of possible goals to the agent. These possible goals should be seen as an indication of which properties are more relevant (salient) then others."))
  (add-element '((p)"Plan Emulation is a lot like comprehension. Instead of a set of strings, a set of actions is given, and instead of a meaning, a goal should be found."))
  (add-element '((p) "In the example below, we use the same inital state as above, specify the plan as agent-0 moving from l1 to l2 and then from l2 to l3 and as possible goals that some agent ends up at some sleeping-location."))
  (add-element '((p) "We can see that the agent 'emulates' the two specified move-actions and finds the final state. He checks the final state against the relevant properties and hypothesizes that the goal was that agent0 ended up in sleeping-location l3."))
  (interpret-plan
 :objects
 '((location l1) (location l2) (location l3) (location l4)
     (agent age0))
 :fluents
 '((empty l2) (empty l3) (empty l4)
   (sleeping-location l3) (sleeping-location l4)
   (connected l2 l1) (connected l3 l2) (connected l1 l2) (connected l2 l3) (connected l1 l4) (connected l4 l1)
   (agent-at age0 l1))
 :plan
 '((move age0 l1 l2)
   (move age0 l2 l3))
 :goal-templates
 '(((agent-at ?ag-0 ?loc-0)
   (sleeping-location ?loc-0)))
 :n 3))

;; (plan-emulation-example)

(defun plan-recognition-example ()
  (add-element '((hr)))
  (add-element '((h2) "5. Plan Recognition: from partial plan to hypothesized goal"))
  (add-element '((p) "In plan recognition, the initial state and a partial plan is provided to the agent. The agent should then find the goal. Again, a set of meaningful properties (possible-goals) is provided to the agent."))
  (add-element '((p) "In the implementation, we achieve this by first emulating the partial plan. We then extract the final state and plan to each of the possible goals. Plan recognition is thus a combination of plan emulation and planning. This is a lot like the use of formulation to aid comprehension, as shown in (Van Eecke 2015)."))
  (add-element '((p) "In the example below, we use the same inital state as above, specify the partial plan as agent-0 moving from l1 to l2 and the possible goals that some agent ends up at some sleeping-location."))
  (add-element '((p) "We can see that the agent 'emulates' the move action first. Then, he extracts the final state and plans to the underspecified possible goal. He sees that the agent now has to move from l2 to l3 to end up in sleeping-location l3."))
  (add-element '((p) "Note that plan recognition starts with a plan emulation phase, and then goes to a planning phase."))
  (recognise-plan
 :objects
 '((location l1) (location l2) (location l3) (location l4)
     (agent age0))
 :fluents
 '((empty l2) (empty l3) (empty l4)
   (sleeping-location l3) (sleeping-location l4)
   (connected l2 l1) (connected l3 l2) (connected l1 l2) (connected l2 l3) (connected l1 l4) (connected l4 l1)
   (agent-at age0 l1))
 :plan ;; this is a partial plan
 '((move age0 l1 l2))
 :goal-templates
 '(((agent-at ?ag-1 ?loc-1)
   (sleeping-location ?loc-1)))
 :n 3))

;; (plan-recognition-example)


;(create-static-html-page "Planning with Construction Grammar"
(progn
  (header-page)
  (introduction)
  (example-grammar)
  (planning-example)
  (plan-emulation-example)
  (plan-recognition-example))
