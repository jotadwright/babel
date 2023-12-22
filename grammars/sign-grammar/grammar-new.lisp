(in-package :sign-grammar)

;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;

(def-fcg-constructions sign-grammar
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form-args set-of-predicates)
                  (form set-of-predicates)
                  (meaning-args set)
                  (meaning set-of-predicates))
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
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

;--------------------------
; Classifier constructions
;--------------------------

; 1. Countries --> holistic constructions

(def-fcg-cxn country-usa-1-cxn
             ((?country-usa-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-amerique.frites-1)
                           (lh-rightmost ?ns-amerique.frites-1)
                           (rh-leftmost ?ns-amerique.frites-2)
                           (rh-rightmost ?ns-amerique.frites-2)))
              <-
              (?country-usa-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(USA ?G)))
               (category country-usa-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ))
               )))

(def-fcg-cxn country-brazil-1-cxn
             ((?country-brazil-1-unit
               
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-bresil.cafe-1)
                           (lh-rightmost ?ns-bresil.cafe-1)
                           (rh-leftmost ?ns-bresil.cafe-2)
                           (rh-rightmost ?ns-bresil.cafe-2)))
              <-
              (?country-brazil-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(BRAZIL ?G)))
               (category country-brazil-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-bresil.cafe-1 ns-bresil.cafe)
                           (right-hand-articulation ?ns-bresil.cafe-2 ns-bresil.cafe)
                           (temporal-relation ?ns-bresil.cafe-1 ?ns-bresil.cafe-2 equals))))))

(def-fcg-cxn country-australia-1-cxn
             ((?country-australia-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-australie-1)
                           (lh-rightmost ?ns-australie-1)
                           (rh-leftmost ?ns-australie-2)
                           (rh-rightmost ?ns-australie-2)))
              <-
              (?country-australia-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(AUSTRALIA ?G)))
               (category country-australia-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-australie-1 ns-australie)
                           (right-hand-articulation ?ns-australie-2 ns-australie)
                           (temporal-relation ?ns-australie-1 ?ns-australie-2 equals))))))

(def-fcg-cxn country-canada-1-cxn
             ((?country-canada-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-canada-1)
                           (lh-rightmost ?ns-canada-1)))
              <-
              (?country-canada-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(CANADA ?G)))
               (category country-canada-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-canada-1 ns-canada))))))

(def-fcg-cxn country-india-1-cxn
             ((?country-india-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-inde-1)
                           (lh-rightmost ?ns-inde-1)))
              <-
              (?country-india-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(INDIA ?G)))
               (category country-india-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-inde-1 ns-inde))))))

(def-fcg-cxn country-belgium-1-cxn
             ((?country-belgium-1-unit
               
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-belgique-1)
                           (lh-rightmost ?ns-belgique-1)))
              <-
              (?country-belgium-1-unit
               (HASH meaning ((COUNTRYID ?F ?G)(BELGIUM ?G)))
               (category country-belgium-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-belgique-1 ns-belgique))))))


;2. States --> holistic constructions

(def-fcg-cxn state-alaska-1-cxn
             ((?state-alaska-1-unit
               
               (meaning-args (?F))
               (boundaries (lh-leftmost ?fs-alaska-1)
                           (lh-rightmost ?fs-alaska-1)))
              <-
              (?state-alaska-1-unit
               (HASH meaning ((STATEID ?F ?G)(ALASKA ?G)))
               (category state-alaska-1-cat)
               --
               (HASH form ((left-hand-articulation ?fs-alaska-1 fs-alaska))))))


;3. Cities --> holistic constructions

(def-fcg-cxn cityid-new_york-1-cxn
             ((?cityid-new_york-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?ns-new-york.y-loc-1)
                           (lh-rightmost ?ns-new-york.y-loc-1)))
              <-
              (?cityid-new_york-1-unit
               (HASH meaning ((CITYID ?F ?G)(NEW_YORK ?G)))
               (category cityid-new_york-1-cat)
               --
               (HASH form ((left-hand-articulation ?ns-new-york.y-loc-1 ns-new-york.y-loc)
                           (right-hand-articulation ?ns-new-york.y-loc-2 ns-new-york.y-loc))))))

; Classifier --> item-based construction

(def-fcg-cxn const-slot1-1-cxn
             ((?const-slot1-1-unit
               (subunits (?slot1-unit))
               (category const-slot1-1-cat)
               (meaning-args (?E ?B ?F))
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-grand-entite-geographique-1)
                           (rh-leftmost ?dss-grand-entite-geographique-2)
                           (rh-rightmost ?dss-grand-entite-geographique-2)))
               <-
               (?const-slot1-1-unit
                (HASH meaning ((CONST ?E ?B ?F)))
                --
                (HASH form ((left-hand-articulation ?dss-grand-entite-geographique-1 dss-grand-entite-geographique)
                            (location ?dss-grand-entite-geographique-1 rssp)
                            (right-hand-articulation ?dss-grand-entite-geographique-2 dss-grand-entite-geographique)
                            (location ?dss-grand-entite-geographique-2 rssp)
                            (temporal-relation ?dss-grand-entite-geographique-1 ?dss-grand-entite-geographique-2 equals)
                            (meets ?slot1-lh-right ?dss-grand-entite-geographique-1))))
               (?slot1-unit
                --
                (category const-slot1-1-slot1-cat)
                (meaning-args (?F))
                (boundaries (lh-leftmost ?slot1-lh-left)
                            (lh-rightmost ?slot1-lh-right)))
                ))




;--> possible problems: ;* The same form for america is often used as context for the question, but it does not always have a meaning predicate. Can we add it?
                        ;* What does it mean to have const-id as the meaning for the classifier? Is this something we want?


;how to learn from examples: first, the agent might see an instance of country + classifier for which it learns a holophrase. Then it sees an instance of state + classifier and learns a generalisation from this. The part that is equal in the known holophrase and the observation is const and the classifier. The parts that are different are the sign for the state and the country on the form side, and countryid-usa and stateid-alaska on the meaning side. Then, the item-based to lexical repair can apply to learn the holistic constructions for other countries and/or states. 


(add-categories '(const-slot1-1-slot1-cat
                  country-usa-1-cat
                  country-canada-1-cat
                  country-belgium-1-cat
                  country-brazil-1-cat
                  country-australia-1-cat
                  country-india-1-cat
                  state-alaska-1-cat
                  cityid-new_york-1-cat)
                *fcg-constructions*)

(progn
  (add-link 'country-usa-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'country-canada-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'country-belgium-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'country-brazil-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'country-australia-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'country-india-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'state-alaska-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*)
  (add-link 'cityid-new_york-1-cat 'const-slot1-1-slot1-cat *fcg-constructions*))
