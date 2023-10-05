(in-package :sign-grammar)

;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;

(def-fcg-constructions sign-grammar
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates))
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                       (:max-number-of-nodes . 200)
                       (:max-search-depth . 50)
                       (:render-mode . :set-of-predicates)
                       (:de-render-mode . :set-of-predicates)
                       (:shuffle-cxns-before-application . t)
                       (:draw-meaning-as-network . t)
                       (:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:search-algorithm . :best-first)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       (:cxn-supplier-mode . :all-cxns)
                       (:heuristics :nr-of-applied-cxns))
  
  :visualization-configurations ((:with-search-debug-data . t)
                                 (:remove-empty-units . nil)
                                 (:show-constructional-dependencies . t)
                                 (:labeled-paths . nil)
                                 (:colored-paths . nil)
                                 (:hide-features . nil)
                                 (:hierarchy-features subunits)
                                 (:selected-hierarchy . subunits)
                                 (:select-subfeatures . nil))
  
  )

(def-fcg-cxn nom-cxn
             ((?nom-unit
               (variable ?nom)
               (category queryable-attribute))
             <-
             (?nom-unit
              --
              (hash form
                    ((articulation ?nom-1 nom))))))

(def-fcg-cxn quoi-cxn
             ((?quoi-unit
               (category question-word)
               (variable ?quoi))
             <-
             (?quoi-unit
              --
              (hash form
                    ((articulation ?quoi-1 quoi))))))


(def-fcg-cxn open-question-cxn
             ((?question-unit
                (subunits (?questionword-unit ?body-lean-unit ?brow-unit ?queried-attribute-unit)))
               <-
               (?question-unit
                (hash meaning
                      ((select ?result-0 ?column-1 ?filter-9 ?filter-7)))
                --
                (hash form
                      ((during ?brow-movement ?question-word)
                       (during ?body-lean ?question-word)
                       (before ?queried-attribute ?question-word 120)
                       (during ?brow-movement ?queried-attribute)
                       (during ?body-lean ?queried-attribute)))
                )
               (?queried-attribute-unit
                --
                (category queryable-attribute)
                (variable ?queried-attribute))
               (?questionword-unit
                --
                (category question-word)
                (variable ?question-word))
               (?body-lean-unit
                --
                (hash form
                    ((articulation ?body-lean-1 body-lean)
                     (dir ?body-lean-1 midssp))))
               (?brow-unit
                --
                (hash form
                    ((articulation ?brow-down-1 brow-down))))))

#|


(def-fcg-cxn body-lean-cxn
             ((?body-lean-unit
               (category body-lean)
               (variable ?body-lean)
               (direction ?direction))
             <-
             (?body-lean-unit
              --
              (hash form
                    ((articulation ?body-lean-1 body-lean)
                     (dir ?body-lean-1 ?direction))))))

(def-fcg-cxn brow-down-cxn
             ((?brow-down-unit
               (category brow-movement)
               (variable ?brow-down-1)
               (direction down))
             <-
             (?brow-down-unit
              --
              (hash form
                    ((articulation ?brow-down-1 brow-down))))))

(def-fcg-cxn brow-up-cxn
             ((?brow-up-unit
               (category brow-movement)
               (variable ?brow-up-1)
               (direction up))
             <-
             (?brow-up-unit
              --
              (hash form
                    ((articulation ?brow-up-1 brow-up))))))

 (def-fcg-cxn virginia-cxn
             ((?virginia-unit
               (source ?source)
               (target ?target)
               (form-variable ?virginia)
               (articulation virginia))
              <-
              (?virginia-unit
               (hash meaning
                     ((bind concept ?target virginia)
                      (bind column ?source state_name)))
               --
               (hash form
                     ((articulation ?virginia virginia))))))

(def-fcg-cxn city-cxn
             ((?city-unit
               (source ?source)
               (target ?target)
               (form-variable ?ville)
               (articulation ville))
              <-
              (?virginia-unit
               (hash meaning
                     ((bind column ?target city_name)
                      (bind table ?source city)))
               --
               (hash form
                     ((articulation ?ville ville))))))

(def-fcg-cxn amerique-cxn
             ((?amerique-unit
               (source ?source)
               (target ?target)
               (form-variable ?amerique)
               (articulation amerique))
              <-
              (?amerique-unit
               (hash meaning
                     ((bind column ?target city_name)
                      (bind table ?source city)))
               --
               (hash form
                     ((articulation ?ville ville))))))

(def-fcg-cxn query-biggest-X-in-Y-cxn
             ((?query-biggest-X-in-Y-unit
               (subunits (?X-unit ?Y-unit)))
             <-
             (?X-unit
              (source ?x-source)
              (target ?x-target)
              --
              (form-variable ?x-form)
              (articulation ?x-articulation))
             (?Y-unit
              (source ?y-source)
              (target ?y-target)
              --
              (form-variable ?y-form))
             (?query-biggest-X-in-Y-unit
              (hash meaning
                    ((dot ?column-1 ?alias-0 ?x-source)
                     (dot ?column-2 ?alias-1 ?column-7)
                     (dot ?column-3 ?alias-0 ?y-source)
                     (dot ?column-4 ?alias-0 ?column-7)
                     (dot ?column-5 ?alias-1 ?y-source)
                     (max ?aggregator-0 ?column-2)
                     (as ?filter-0 ?x-target ?alias-1)
                     (equals ?filter-1 ?column-5 ?y-target)
                     (where ?filter-2 ?filter-1)
                     (from ?filter-3 ?filter-0)
                     (equals ?filter-4 ?column-4 ?result-1)
                     (equals ?filter-5 ?column-3 ?y-target)
                     (and ?filter-6 ?filter-4 ?filter-5)
                     (where ?filter-7 ?filter-6)
                     (as ?filter-8 ?x-target ?alias-0)
                     (from ?filter-9 ?filter-8)
                     (select ?result-0 column-1 filter-9 filter-7)
                     (select ?result-1 ?aggregator-0 ?filter-3 ?filter-2)
                     (bind ?column ?column-7 population)
                     (bind ?concept ?alias-0 ?alias-0-content)
                     (bind ?concept ?alias-1 ?alias-1-content)
                     ))
              --
              (hash form
                    ((articulation ?pt-31 pt)
                     (ptr ?pt-31 index)
                     (dir ?pt-31 midssp)
                     (articulation ?il-y-a-19 il-y-a)
                     (articulation ?plusieurs-17 plusieurs)
                     (modification ?x-form multiplicity)
                     (articulation ?x-form-1 ?x-articulation)
                     (articulation ?plus-8 plus)
                     (articulation ?grand-8 grand)
                     (articulation ?brow-up-33 brow-up)
                     (articulation ?brow-up-34 brow-up)
                     (articulation ?brow-down-18 brow-down)
                     (articulation ?body-lean-31 body-lean)
                     (dir ?body-lean-31 lssp)
                     (articulation ?body-lean-32 body-lean)
                     (dir ?body-lean-32 midssp)
                     (before ?y-form ?pt-31 80)
                     (before ?pt-31 ?il-y-a-19 100)
                     (before ?il-y-a-19 ?plusieurs-17 80)
                     (before ?plusieurs-17 ?x-form 20)
                     (before ?x-form ?x-form-1 480)
                     (before ?x-form-1 ?plus-8 10)
                     (before ?plus-8 ?grand-8 30)
                     (during ?brow-up-33 ?pt-31)
                     (during ?brow-up-34 ?x-form-1)
                     (during ?brow-up-34 ?plus-8)
                     (during ?brow-up-34 ?grand-8)
                     (during ?body-lean-31 ?x-form)
                     )))))

 
(def-fcg-cxn what-is-the-biggest-city-in-virginia-cxn
             (
             <-
             (?what-is-the-biggest-city-in-virginia-unit
              (hash meaning
                    ((dot ?column-1 ?alias-0 ?column-6)
                     (dot ?column-2 ?alias-1 ?column-7)
                     (dot ?column-3 ?alias-0 ?column-8)
                     (dot ?column-4 ?alias-0 ?column-7)
                     (dot ?column-5 ?alias-1 ?column-8)
                     (max ?aggregator-0 ?column-2)
                     (as ?filter-0 ?table-0 ?alias-1)
                     (equals ?filter-1 ?column-5 ?comparator-0)
                     (where ?filter-2 ?filter-1)
                     (from ?filter-3 ?filter-0)
                     (equals ?filter-4 ?column-4 ?result-1)
                     (equals ?filter-5 ?column-3 ?comparator-0)
                     (and ?filter-6 ?filter-4 ?filter-5)
                     (where ?filter-7 ?filter-6)
                     (as ?filter-8 ?table-0 ?alias-0)
                     (from ?filter-9 ?filter-8)
                     (select ?result-0 ?column-1 ?filter-9 ?filter-7)
                     (select ?result-1 ?aggregator-0 ?filter-3 ?filter-2)
                     (bind column ?column-8 state_name)
                     (bind column ?column-7 population)
                     (bind column ?column-6 city_name)
                     (bind concept ?comparator-0 virginia)
                     (bind concept ?alias-0 cityalias0)
                     (bind concept ?alias-1 cityalias1)
                     (bind table ?table-0 city)))
              --
              (hash form
                    ((articulation ?virginia-5 virginia)
                     (articulation ?pt-31 pt)
                     (ptr ?pt-31 index)
                     (dir ?pt-31 midssp)
                     (articulation ?il-y-a-19 il-y-a)
                     (articulation ?plusieurs-17 plusieurs)
                     (articulation ?ville-17 ville)
                     (modification ?ville-17 multiplicity)
                     (articulation ?ville-18 ville)
                     (articulation ?plus-8 plus)
                     (articulation ?grand-8 grand)
                     (articulation ?nom-13 nom)
                     (articulation ?quoi-15 quoi)
                     (articulation ?brow-up-33 brow-up)
                     (articulation ?brow-up-34 brow-up)
                     (articulation ?brow-down-18 brow-down)
                     (articulation ?body-lean-31 body-lean)
                     (dir ?body-lean-31 lssp)
                     (articulation ?body-lean-32 body-lean)
                     (dir ?body-lean-32 midssp)
                     (before ?virginia-5 ?pt-31 80)
                     (before ?pt-31 ?il-y-a-19 100)
                     (before ?il-y-a-19 ?plusieurs-17 80)
                     (before ?plusieurs-17 ?ville-17 20)
                     (before ?ville-17 ?ville-18 480)
                     (before ?ville-18 ?plus-8 10)
                     (before ?plus-8 ?grand-8 30)
                     (before ?grand-8 ?nom-13 190)
                     (before ?nom-13 ?quoi-15 120)
                     (during ?brow-up-33 ?pt-31)
                     (during ?brow-up-34 ?ville-18)
                     (during ?brow-up-34 ?plus-8)
                     (during ?brow-up-34 ?grand-8)
                     (during ?brow-down-18 ?nom-13)
                     (during ?brow-down-18 ?quoi-15)
                     (during ?body-lean-31 ?ville-17)
                     (during ?body-lean-32 ?nom-13)
                     (during ?body-lean-32 ?quoi-15))))))



; concept constructions






(def-fcg-cxn what-is-the-X-in-Y-cxn
             ((
               <-
               (?what-is-the-X-in-Y-unit
                (hash meaning
                      ((
|#

