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

;-----------------------;
; Lexical Constructions ;
;-----------------------;

; usa-1-cxn: pairs meaning predicate (USA ?G) to the two-handed sign of ns-amerique.frites

(def-fcg-cxn usa-1-cxn
             ((?usa-1-unit
               (meaning-args (?G))
               (boundaries (lh-leftmost ?ns-amerique.frites-1)
                           (lh-rightmost ?ns-amerique.frites-1)
                           (rh-leftmost ?ns-amerique.frites-2)
                           (rh-rightmost ?ns-amerique.frites-2))
               (category usa-1-cat))
              <-
              (?usa-1-unit
               (HASH meaning ((USA ?G)))
               --
               (HASH form ((left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (temporal-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ))
               )))

; alaska-1-cxn: pairs meaning predicate (ALASKA ?H) to the fingerspelled representation of "Alaska"

(def-fcg-cxn alaska-1-cxn 
             ((?alaska-1-unit
               (meaning-args (?H))
               (boundaries (lh-leftmost ?fs-alaska-1)
                           (lh-rightmost ?fs-alaska-1))
               (category alaska-1-cat))
              <-
              (?alaska-1-unit
               (HASH meaning ((ALASKA ?H)))
               --
               (HASH form ((left-hand-articulation ?fs-alaska-1 fs-alaska)
                           ))
               )))

(def-fcg-cxn new_york-1-cxn 
             ((?new_york-1-unit
               (meaning-args (?H))
               (boundaries (lh-leftmost ?ns-new-york.y-loc-1)
                           (lh-rightmost ?ns-new-york.y-loc-1))
               (category new_york-1-cat))
              <-
              (?new_york-1-unit
               (HASH meaning ((NEW_YORK ?H)))
               --
               (HASH form ((left-hand-articulation ?ns-new-york.y-loc-1 ns-new-york.y-loc)
                           (right-hand-articulation ?ns-new-york.y-loc-2 ns-new-york.y-loc)
                           (temporal-relation ?ns-new-york.y-loc-1 ?ns-new-york.y-loc-2 equals)
                           ))
               )))

;--------------------------;
; item-based constructions ;
;--------------------------;

; slot1-country-id-const-1-cxn: classifier for countries. In the form it matches on the size and shape classifier that in general is used for geographic entities when they function as ground in the semantic structure. This classifier should immediately follow a slot-filler with the right category. The classifier is maintained on the non-dominant hand. In the meaning, it matches on ((CONST ?E ?B ?F)(COUNTRYID ?F ?H)) where ?H is  the argument passed on by the slot. So this construction classifies the referent ?H as a country and retrieves its id as a constant. 

(def-fcg-cxn slot1-countryid-const-1-cxn
             ((?slot1-countryid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?E ?B))
               (category slot1-countryid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.ground-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-countryid-const-1-unit
               (HASH meaning ((CONST ?E ?B ?F)(COUNTRYID ?F ?H)))
               
               --
               (HASH form ((left-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (location ?dss-entite-geographique.ground-1 ?location)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                           (location ?dss-entite-geographique.ground-2 ?location)
                           (temporal-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 starts)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           ))
               )
              (?slot1-unit
               (meaning-args (?H))
               (category slot1-countryid-const-slot1-1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

;slot1-loc-state-1-cxn: classifier verb. The construction expresses existentiality (and location): namely that there exist states (inside something else, identified by the slot). In the form, it matches on the sign il-y-a which is typical for existential constructions, and the size and shape classifier that is used for geographic entities that function as figure in the semantic structure. These signs on the dominant hand occur together with a held sign on the non-dominant hand (identified by the slot). In the meaning, the construction matches on (LOC ?E ?A ?B)(STATE ?E ?A), where ?E and ?B are arguments of the slot. 

(def-fcg-cxn slot1-loc-state-1-cxn
             ((?slot1-loc-state-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?E ?A))
               (category slot1-loc-state-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.figure-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.figure-1))
               (spatial-agreement ?location))
              <-
              (?slot1-loc-state-1-unit
               (HASH meaning ((LOC ?E ?A ?B)(STATE ?E ?A)))
               
               --
               (HASH form ((left-hand-articulation ?il-y-a-1 il-y-a)
                           (left-hand-articulation ?dss-entite-geographique.figure-1 dss-entite-geographique.figure)
                           (location ?dss-entite-geographique.figure-1 ?location)
                           (temporal-relation ?il-y-a-1 ?slot1-rh-right ?relation-1)
                           (temporal-relation ?dss-entite-geographique.figure-1 ?slot1-rh-right ?relation-2)
                           (meets ?slot1-lh-right ?il-y-a-1)
                           (meets ?il-y-a-1 ?dss-entite-geographique.figure-1)
                           ))
               )
              (?slot1-unit
               (meaning-args (?E ?B))
               (category slot1-loc-state-slot1-1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )))


(def-fcg-cxn slot1-stateid-const-1-cxn
             ((?slot1-stateid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?E ?B))
               (category slot1-stateid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-1)))
              <-
              (?slot1-stateid-const-1-unit
               (HASH meaning ((CONST ?E ?B ?F)(STATEID ?F ?H)))
               
               --
               (HASH form ((right-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (location ?dss-entite-geographique.ground-1 ?location)
                           (temporal-relation ?slot1-lh-right ?dss-entite-geographique.ground-1 meets)
                           ))
               )
              (?slot1-unit
               (meaning-args (?H))
               (category slot1-stateid-const-1-slot1-1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-cityid-const-1-cxn
             ((?slot1-cityid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?E ?B))
               (category slot1-cityid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.ground-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-cityid-const-1-unit
               (HASH meaning ((CONST ?E ?B ?F)(CITYID ?F ?H)))
               
               --
               (HASH form ((left-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (location ?dss-entite-geographique.ground-1 ?location)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                            (location ?dss-entite-geographique.ground-2 ?location)
                           (temporal-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 starts)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           (temporal-relation ?slot1-lh-right ?dss-entite-geographique.ground-2 meets)
                           ))
               )
              (?slot1-unit
               (meaning-args (?H))
               (category slot1-cityid-const-1-slot1-1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )))

(def-fcg-cxn slot1-size-answer-1-cxn
             ((?slot1-size-answer-1-unit
               (subunits (?slot1-unit))
               (category slot1-size-answer-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?metre-carre-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-size-answer-1-unit
               (HASH meaning ((ANSWER ?C ?A ?D)(SIZE ?D ?A ?B)))
               --
               (HASH form ((left-hand-articulation ?surface-1 surface)
                           (location ?surface-1 ?location)
                           (left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           (meets ?surface-1 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (temporal-relation ?surface-1 ?slot1-rh-right ?relation-1)
                           ;(temporal-relation ?combien-1 ?slot1-rh-right during)
                           ;(temporal-relation ?metre-carre-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (?E ?B))
               (category slot1-size-answer-slot1-1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )))
             

(add-categories '(usa-1-cat
                  alaska-1-cat
                  new_york-1-cat
                  slot1-countryid-const-1-cat
                  slot1-countryid-const-slot1-1-cat
                  slot1-loc-state-1-cat
                  slot1-loc-state-slot1-1-cat
                  slot1-stateid-const-1-cat
                  slot1-stateid-const-1-slot1-1-cat
                  slot1-cityid-const-1-slot1-1-cat
                  slot1-size-answer-slot1-1-cat
                  slot1-cityid-const-1-cat)
                *fcg-constructions*)

(progn
  (add-link 'usa-1-cat 'slot1-countryid-const-slot1-1-cat *fcg-constructions*)
  (add-link 'slot1-countryid-const-1-cat 'slot1-loc-state-slot1-1-cat *fcg-constructions*)
  (add-link 'alaska-1-cat 'slot1-stateid-const-1-slot1-1-cat *fcg-constructions*)
  (add-link 'new_york-1-cat 'slot1-cityid-const-1-slot1-1-cat *fcg-constructions*)
  (add-link 'slot1-cityid-const-1-cat 'slot1-size-answer-slot1-1-cat *fcg-constructions*)
  (add-link 'slot1-stateid-const-1-cat 'slot1-size-answer-slot1-1-cat *fcg-constructions*))