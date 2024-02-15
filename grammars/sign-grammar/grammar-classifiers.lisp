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
                  (meaning-args default)
                  (meaning set-of-predicates))
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-articulations-in-root)
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
                       (:priority-mode :nr-of-units-matched)
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
;answer(A,largest(A,(state(A),loc(A,B),const(B,countryid(usa)))))
; 11_0 & 11_1
(def-fcg-cxn usa-1-cxn
             ((?usa-1-unit
               (meaning-args (layer ?usa-layer))
               (boundaries (lh-leftmost ?ns-amerique.frites-1)
                           (lh-rightmost ?ns-amerique.frites-1)
                           (rh-leftmost ?ns-amerique.frites-2)
                           (rh-rightmost ?ns-amerique.frites-2))
               (category usa-1-cat))
              <-
              (?usa-1-unit
               (HASH meaning ((USA ?usa-layer)))
               --
               (HASH form ((left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ))
               )))

(def-fcg-cxn usa-2-cxn
             ((?usa-2-unit
               (meaning-args (layer ?usa-layer))
               (boundaries (lh-leftmost ?dans-1)
                           (lh-rightmost ?ns-amerique.frites-1)
                           (rh-leftmost ?dans-2)
                           (rh-rightmost ?ns-amerique.frites-2))
               (category usa-2-cat))
              <-
              (?usa-2-unit
               (HASH meaning ((USA ?usa-layer)))
               --
               (HASH form ((left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (coincides-relation ?dans-1 ?dans-2 equals)
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?dans-1 ?ns-amerique.frites-2)
                           ))
               )))

(def-fcg-cxn slot1-countryid-const-1-cxn
             ((?slot1-countryid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?const-target)
                             (layer ?const-layer))
               (category slot1-countryid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.ground-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-countryid-const-1-unit
               (HASH meaning ((CONST ?const-layer ?const-target ?countryid-layer)(COUNTRYID ?countryid-layer ?slot1-layer)))
               
               --
               (HASH form ((left-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-2 ?location)
                           (coincides-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 starts)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-2)
                           ))
               )
              (?slot1-unit
               (meaning-args (layer ?slot1-layer))
               (category slot1-countryid-const-1-slot1-cat)
               --
               (category slot1-countryid-const-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))

(def-fcg-cxn slot1-loc-state-1-cxn
             ((?slot1-loc-state-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?loc-target)
                             (layer ?slot1-layer))
               (category slot1-loc-state-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.figure-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location))
              <-
              (?slot1-loc-state-1-unit
               (HASH meaning ((LOC ?slot1-layer ?loc-target ?slot1-target)(STATE ?slot1-layer ?loc-target)))
               
               --
               (HASH form ((left-hand-articulation ?il-y-a-1 il-y-a)
                           (left-hand-articulation ?dss-entite-geographique.figure-1 dss-entite-geographique.figure)
                           (modification ?dss-entite-geographique.figure-1 reduplicated)
                           (main-location ?dss-entite-geographique.figure-1 ?location)
                           (minor-location ?dss-entite-geographique.figure-1 i)
                           (coincides-relation ?il-y-a-1 ?slot1-rh-right ?relation-1)
                           (coincides-relation ?dss-entite-geographique.figure-1 ?slot1-rh-right ?relation-2)
                           (meets ?slot1-lh-right ?il-y-a-1)
                           (meets ?il-y-a-1 ?dss-entite-geographique.figure-1)
                           ))
               )
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-loc-state-1-slot1-cat)
               --
               (category slot1-loc-state-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )))

(def-fcg-cxn slot1-largest-1-cxn
             ((?slot1-largest-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?slot1-target)
                             (layer ?largest-layer))
               (category slot1-largest-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?grand.5-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?grand.5-2))
               (spatial-agreement ?location))
              <-
              (?slot1-largest-1-unit
               (HASH meaning ((LARGEST ?largest-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?un-1 un)
                           (left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (left-hand-articulation ?plus.p-1 plus.p)
                           (left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (coincides-relation ?grand.5-1 ?grand.5-2 equals)
                           (coincides-relation ?un-1 ?slot1-rh-right during)
                           (coincides-relation ?pt-1 ?slot1-rh-right finishes)
                           (meets ?slot1-lh-right ?un-1)
                           (meets ?un-1 ?pt-1)
                           (meets ?pt-1 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-1)
                           (meets ?plus.p-1 ?grand.5-2)
                           ))
               )
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-largest-1-slot1-cat)
               --
               (category slot1-largest-1-slot1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)
                           ))
              ))

(def-fcg-cxn slot1-largest-2-cxn
             ((?slot1-largest-2-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?slot1-target)
                             (layer ?largest-layer))
               (category slot1-largest-2-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?pt-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-1))
               (spatial-agreement ?location))
              <-
              (?slot1-largest-2-unit
               (HASH meaning ((LARGEST ?largest-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (coincides-relation ?grand.5-1 ?grand.5-2 equals)
                           (left-hand-articulation ?plus.p-1 plus.p)
                           (left-hand-articulation ?grand.5-3 grand.5)
                           (right-hand-articulation ?grand.5-4 grand.5)
                           (coincides-relation ?grand.5-3 ?grand.5-4 equals)
                           (left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?Location)
                           (modification ?pt-1 circle)
                           (right-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (meets ?slot1-lh-right ?grand.5-1)
                           (meets ?slot1-lh-right ?grand.5-2)
                           (meets ?grand.5-1 ?plus.p-1)
                           (meets ?plus.p-1 ?grand.5-3)
                           (meets ?plus.p-1 ?grand.5-4)
                           (meets ?grand.5-3 ?pt-1)
                           (meets ?grand.5-3 ?dss-entite-geographique.ground-1)
                           (coincides-relation ?pt-1 ?dss-entite-geographique.ground-1 starts)
                           ))
               )
              (?slot1-unit
                (meaning-args (target ?slot1-target)
                              (layer ?slot1-layer))
                (category slot1-largest-2-slot1-cat)
                --
                (category slot1-largest-2-slot1-cat)
                (spatial-agreement ?location)
                (boundaries (lh-leftmost ?slot1-lh-left)
                            (lh-rightmost ?slot1-lh-right)
                            (rh-leftmost ?slot1-rh-left)
                            (rh-rightmost ?slot1-rh-right)
                            ))
              ))

(def-fcg-cxn slot1-answer-1-cxn
             ((?slot1-answer-1-unit
               (subunits (?slot1-unit))
               (category slot1-answer-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?ou-lieu.o-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?ou-lieu.o-2))
               (spatial-agreement ?location))
              <-
              (?slot1-answer-1-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               
               --
               (HASH form ((left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (coincides-relation ?ou-lieu.o-1 ?ou-lieu.o-2 equals)
                           (meets ?slot1-lh-right ?ou-lieu.o-1)
                           (meets ?slot1-lh-right ?ou-lieu.o-2)
                           ))
               )
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-answer-1-slot1-cat)
               --
               (category slot1-answer-1-slot1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)
                           ))
              ))

(def-fcg-cxn slot1-answer-2-cxn
             ((?slot1-answer-2-unit
               (subunits (?slot1-unit))
               (category slot1-answer-2-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?ou-lieu.o-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location))
              <-
              (?slot1-answer-2-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               
               --
               (HASH form ((left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (meets ?slot1-lh-right ?ou-lieu.o-1)
                           (coincides-relation ?ou-lieu.o-1 ?slot1-rh-right finishes)
                           ))
               )
              (?slot1-unit
              (meaning-args (target ?slot1-target)
                            (layer ?slot1-layer))
              (category slot1-answer-2-slot1-cat)
               --
               (category slot1-answer-2-slot1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)
                           ))
              ))

;answer(A,(size(B,A),const(B,stateid(alaska))))
; 15_0_ALASKA

(def-fcg-cxn alaska-1-cxn 
             ((?alaska-1-unit
               (meaning-args (layer ?alaska-layer))
               (boundaries (lh-leftmost ?fs-alaska-1)
                           (lh-rightmost ?fs-alaska-1))
               (category alaska-1-cat))
              <-
              (?alaska-1-unit
               (HASH meaning ((ALASKA ?alaska-layer)))
               --
               (HASH form ((left-hand-articulation ?fs-alaska-1 fs-alaska)
                           ))
               )))

(def-fcg-cxn slot1-stateid-const-1-cxn
             ((?slot1-stateid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?const-target)
                             (layer ?const-layer))
               (category slot1-stateid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?dss-entite-geographique.ground-1)
                           (rh-rightmost ?dss-entite-geographique.ground-1)))
              <-
              (?slot1-stateid-const-1-unit
               (HASH meaning ((CONST ?const-layer ?const-target ?stateid-layer)(STATEID ?stateid-layer ?slot1-layer)))
               
               --
               (HASH form ((right-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           ))
               )
              (?slot1-unit
               (meaning-args (layer ?slot1-layer))
               (category slot1-stateid-const-1-slot1-cat)
               --
               (category slot1-stateid-const-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-size-1-cxn
             ((?slot1-size-1-unit
               (subunits (?slot1-unit))
               (category slot1-size-1-cat)
               (meaning-args (target ?size-target)
                             (layer ?slot1-layer))
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?surface-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)))
              <-
              (?slot1-size-1-unit
               (HASH meaning ((SIZE ?slot1-layer ?slot1-target ?size-target)))
               --
               (HASH form ((left-hand-articulation ?surface-1 surface)
                           (main-location ?surface-1 ?location)
                           (minor-location ?surface-1 i)
                           (coincides-relation ?surface-1 ?slot1-rh-right ?relation-1)
                           (meets ?slot1-lh-right ?surface-1)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-size-1-slot1-cat)
               --
               (category slot1-size-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location))
               ))

(def-fcg-cxn slot1-answer-3-cxn
             ((?slot1-answer-3-unit
               (subunits (?slot1-unit))
               (category slot1-answer-3-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.ground-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-answer-3-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           (left-hand-articulation ?dsh-mesurer-1 dsh-mesurer)
                           (main-location ?dsh-mesurer-1 ?location)
                           (right-hand-articulation ?dsh-mesurer-2 dsh-mesurer)
                           (main-location ?dsh-mesurer-2 ?location)
                           (coincides-relation ?dsh-mesurer-1 ?dsh-mesurer-2 equals)
                           (left-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (modification ?dss-entite-geographique.ground-1 **)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                           (modification ?dss-entite-geographique.ground-2 **)
                           (main-location ?dss-entite-geographique.ground-2 ?location)
                           (coincides-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 equals)
                           (meets ?dsh-mesurer-1 ?dss-entite-geographique.ground-1)
                           (meets ?dsh-mesurer-1 ?dss-entite-geographique.ground-2)
                           (meets ?metre-carre-1 ?dsh-mesurer-1)
                           (meets ?metre-carre-1 ?dsh-mesurer-2)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (coincides-relation ?combien-1 ?slot1-rh-left during)
                           (coincides-relation ?metre-carre-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-answer-3-slot1-cat)
               --
               (category slot1-answer-3-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )
              ))
             
;answer(A,(size(B,A),const(B,cityid('new york',_))))
;16_0_NEW_YORK

(def-fcg-cxn new_york-1-cxn 
             ((?new_york-1-unit
               (meaning-args (layer ?new_york-layer))
               (boundaries (lh-leftmost ?pt-1)
                           (lh-rightmost ?ns-new-york.y-loc-1))
               (spatial-agreement ?location)
               (category new_york-1-cat))
              <-
              (?new_york-1-unit
               (HASH meaning ((NEW_YORK ?new_york-layer)))
               --
               (HASH form ((left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (left-hand-articulation ?ns-new-york.y-loc-1 ns-new-york.y-loc)
                           (right-hand-articulation ?ns-new-york.y-loc-2 ns-new-york.y-loc)
                           (coincides-relation ?ns-new-york.y-loc-1 ?ns-new-york.y-loc-2 equals)
                           (meets ?pt-1 ?ns-new-york.y-loc-1)
                           (meets ?pt-1 ?ns-new-york.y-loc-2)
                           ))
               )))

(def-fcg-cxn slot1-cityid-const-1-cxn
             ((?slot1-cityid-const-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?const-target)
                             (layer ?const-layer))
               (category slot1-cityid-const-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.ground-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-2)))
              <-
              (?slot1-cityid-const-1-unit
               (HASH meaning ((CONST ?const-layer ?const-target ?cityid-layer)(CITYID ?cityid-layer ?slot1-layer ?empty)))
               
               --
               (HASH form ((left-hand-articulation ?dss-entite-geographique.ground-1 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                           (main-location ?dss-entite-geographique.ground-2 ?location)
                           (coincides-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 starts)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-2)
                           ))
               )
              (?slot1-unit
               (meaning-args (layer ?slot1-layer))
               (category slot1-cityid-const-1-slot1-cat)
               --
               (spatial-agreement ?location)
               (category slot1-cityid-const-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )))

(def-fcg-cxn slot1-answer-4-cxn
             ((?slot1-answer-4-unit
               (subunits (?slot1-unit))
               (category slot1-answer-4-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?metre-carre-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)))
              <-
              (?slot1-answer-4-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-answer-4-slot1-cat)
               --
               (category slot1-answer-4-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )
              ))

;answer(A,(elevation(B,A),const(B,placeid('guadalupe peak'))))
;18_0_GUADALUPE-PEAK

(def-fcg-cxn guadalupe_peak-1-cxn
             ((?guadalupe_peak-1-unit
               (meaning-args (layer ?guadalupe_peak-layer))
               (boundaries (lh-leftmost ?fs-guadalupe-1)
                           (lh-rightmost ?fs-guadalupe-1))
               (category guadalupe_peak-1-cat))
               <-
              (?guadalupe_peak-1-unit
               (HASH meaning ((GUADALUPE_PEAK ?guadalupe_peak-layer)))
               --
               (HASH form ((left-hand-articulation ?fs-guadalupe-1 fs-guadalupe)))
               )))

(def-fcg-cxn const-placeid-slot1-1-cxn
             ((?const-placeid-slot1-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?const-target)
                             (layer ?const-layer))
               (category const-placeid-slot1-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?montagne-1)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?montagne-2)
                           (rh-rightmost ?montagne-2)))
              <-
              (?const-placeid-slot1-1-unit
               (HASH meaning ((CONST ?const-layer ?const-target ?placeid-layer)(PLACEID ?placeid-layer ?slot1-layer)))
               
               --
               (HASH form ((left-hand-articulation ?montagne-1 montagne)
                           (main-location ?montagne-1 ?location)
                           (right-hand-articulation ?montagne-2 montagne)
                           (main-location ?montagne-2 ?location)
                           (coincides-relation ?montagne-1 ?montagne-2 starts)
                           (left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (left-hand-articulation ?pt-2 pt)
                           (main-location ?pt-2 ?location)
                           (modification ?pt-2 down-up)
                           (meets ?montagne-1 ?pt-1)
                           (meets ?pt-1 ?pt-2)
                           (meets ?pt-2 ?slot1-lh-left)
                           (coincides-relation ?pt-1 ?montagne-2 during)
                           (coincides-relation ?pt-2 ?montagne-2 during)
                           (coincides-relation ?slot1-lh-left ?montagne-2 during)
                           )))
              (?slot1-unit
               (meaning-args (layer ?slot1-layer))
               (category const-placeid-slot1-1-slot1-cat)
               --
                (category const-placeid-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-elevation-1-cxn
             ((?slot1-elevation-1-unit
               (subunits (?slot1-unit))
               (meaning-args (target ?elevation-target)
                             (layer ?slot1-layer))
               (category slot1-elevation-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?pt-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)))
              <-
              (?slot1-elevation-1-unit
               (HASH meaning ((ELEVATION ?slot1-layer ?slot1-target ?elevation-target)))
               
               --
               (HASH form ((left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (modification ?pt-1 down-up)
                           (meets ?slot1-lh-right ?pt-1)
                           (coincides-relation ?pt-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-elevation-1-slot1-cat)
               --
               (category slot1-elevation-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))

(def-fcg-cxn slot1-answer-5-cxn
             ((?slot1-answer-5-unit
               (subunits (?slot1-unit))
               (category slot1-answer-5-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?metre-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?metre-2)))
              <-
              (?slot1-answer-5-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?metre-1 metre)
                           (right-hand-articulation ?metre-2 metre)
                           (coincides-relation ?metre-1 ?metre-2 equals)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?metre-1)
                           (meets ?combien-1 ?metre-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-answer-5-slot1-cat)
               --
               (category slot1-answer-5-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )
              ))

; 23_RIO-GRANDE
; answer(A,(len(B,A),const(B,riverid('rio grande'))))

(def-fcg-cxn rio_grande-1-cxn 
             ((?rio_grande-1-unit
               (meaning-args (layer ?rio_grande-layer))
               (boundaries (lh-leftmost ?fs-rio-grande-1)
                           (lh-rightmost ?fs-rio-grande-1))
               (category rio_grande-1-cat))
              <-
              (?rio_grande-1-unit
               (HASH meaning ((RIO_GRANDE ?rio_grande-layer)))
               --
               (HASH form ((left-hand-articulation ?fs-rio-grande-1 fs-rio-grande)
                           ))
               )))

(def-fcg-cxn const-riverid-slot1-1-cxn
             ((?const-riverid-slot1-1-unit
               (meaning-args (target ?const-target)
                             (layer ?const-layer))
               (subunits (?slot1-unit))
               (boundaries (lh-leftmost ?eau-1)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?dsm-riviere-1)
                           (rh-rightmost ?nom.1-1))
               (category const-riverid-slot1-1-cat)
               (spatial-agreement ?location))
              <-
              (?const-riverid-slot1-1-unit
               (HASH meaning ((CONST ?const-layer ?const-target ?riverid-layer)(RIVERID ?riverid-layer ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?eau-1 eau)
                           (left-hand-articulation ?dsm-riviere-1 dsm-riviere)
                           (main-location ?dsm-riviere-1 ?location)
                           (right-hand-articulation ?dsm-riviere-2 dsm-riviere)
                           (main-location ?dsm-riviere-2 ?location)
                           (coincides-relation ?dsm-riviere-1 ?dsm-riviere-2 starts)
                           (left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (modification ?pt-1 back-front)
                           (left-hand-articulation ?nom.1-1 nom.1)
                           (right-hand-articulation ?nom.1-2 nom.1)
                           (coincides-relation ?nom.1-1 ?nom.1-2 equals)
                           (meets ?dsm-riviere-1 ?pt-1)
                           (meets ?pt-1 ?nom.1-1)
                           (meets ?pt-1 ?nom.1-2)
                           (meets ?nom.1-1 ?slot1-lh-left)
                           (meets ?eau-1 ?dsm-riviere-1)
                           (meets ?eau-1 ?dsm-riviere-2)
                           (coincides-relation ?pt-1 ?dsm-riviere-2 finishes)
                           ))
               )
              (?slot1-unit
               (meaning-args (layer ?slot1-layer))
                (category const-riverid-slot1-1-slot1-cat)
               --
               (category const-riverid-slot1-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-len-1-cxn
             ((?slot1-len-1-unit
               (meaning-args (target ?len-target)
                             (layer ?slot1-layer))
               (subunits (?slot1-unit))
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-riviere-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?fbuoy-dsm-riviere-1))
               (category slot1-len-1-cat)
               (spatial-agreement ?location))
              <-
              (?slot1-len-1-unit
               (HASH meaning ((LEN ?slot1-layer ?slot1-target ?len-target)))
               --
               (HASH form ((left-hand-articulation ?dss-riviere-1 dss-riviere)
                           (main-location ?dss-riviere-1 ?location)
                           (right-hand-articulation ?fbuoy-dsm-riviere-1 fbuoy-dsm-riviere)
                           (main-location ?fbuoy-dsm-riviere-1 ?location)
                           (coincides-relation ?dss-riviere-1 ?fbuoy-dsm-riviere-1 starts)
                           (meets ?slot1-lh-right ?dss-riviere-1)
                           (meets ?slot1-lh-right ?fbuoy-dsm-riviere-1)
                           ))
               )
              (?slot1-unit
               (meaning-args (layer ?slot1-layer)
                             (target ?slot1-target))
               (category slot1-len-1-slot1-cat)
               --
               (category slot1-len-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))

(def-fcg-cxn slot1-answer-6-cxn
             ((?slot1-answer-6-unit
               (subunits (?slot1-unit))
               (category slot1-answer-6-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?kilometre-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)))
              <-
              (?slot1-answer-6-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?kilometre-1 kilometre)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?kilometre-1)
                           (coincides-relation ?combien-1 ?slot1-rh-right during)
                           (coincides-relation ?kilometre-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-answer-6-slot1-cat)
               --
               (category slot1-answer-6-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )
              ))

; 35_0
; answer(A,(population(B,A),largest(B,city(B))))

(def-fcg-cxn city-1-cxn 
             ((?city-1-unit
               (meaning-args (layer ?city-layer)
                             (target ?city-target))
               (boundaries (lh-leftmost ?dans-1)
                           (lh-rightmost ?ville-1)
                           (rh-leftmost ?dans-2)
                           (rh-rightmost ?ville-2))
               (category city-1-cat))
              <-
              (?city-1-unit
               (HASH meaning ((CITY ?city-layer ?city-target)))
               --
               (HASH form ((left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (coincides-relation ?dans-1 ?dans-2 equals)
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           (left-hand-articulation ?ville-1 ville)
                           (right-hand-articulation ?ville-2 ville)
                           (coincides-relation ?ville-1 ?ville-2 equals)
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?dans-1 ?ns-amerique.frites-2)
                           (meets ?ns-amerique.frites-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?ville-1)
                           (meets ?il-y-a-1 ?ville-2)
               )))))

(def-fcg-cxn slot1-population-largest-1-cxn
             ((?slot1-population-largest-1-unit
               (subunits (?slot1-unit))
               (meaning-args (layer ?largest-layer)
                             (target ?population-target))
               (category slot1-population-largest-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?habiter-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?habiter-2)))
              <-
              (?slot1-population-largest-1-unit
               (HASH meaning ((POPULATION ?largest-layer ?slot1-target ?population-target)(LARGEST ?largest-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?beaucoup.f-1 beaucoup.f)
                           (left-hand-articulation ?personne.humain-1 personne.humain)
                           (right-hand-articulation ?personne.humain-2 personne.humain)
                           (coincides-relation ?personne.humain-1 ?personne.humain-2 equals)
                           (left-hand-articulation ?habiter-1 habiter)
                           (modification ?habiter-1 reduplicated)
                           (right-hand-articulation ?habiter-2 habiter)
                           (modification ?habiter-2 reduplicated)
                           (coincides-relation ?habiter-1 ?habiter-2 equals)
                           (meets ?slot1-lh-right ?beaucoup.f-1)
                           (meets ?beaucoup.f-1 ?personne.humain-1)
                           (meets ?beaucoup.f-1 ?personne.humain-2)
                           (meets ?personne.humain-1 ?habiter-1)
                           (meets ?personne.humain-1 ?habiter-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-population-largest-1-slot1-cat)
               --
               (category slot1-population-largest-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))


(def-fcg-cxn slot1-answer-7-cxn
             ((?slot1-answer-7-unit
               (subunits (?slot1-unit))
               (category slot1-answer-7-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?palm-up-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?palm-up-2)))
              <-
              (?slot1-answer-7-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?palm-up-1 palm-up)
                           (right-hand-articulation ?palm-up-2 palm-up)
                           (coincides-relation ?palm-up-1 ?palm-up-2 equals)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?palm-up-1)
                           (meets ?combien-1 ?palm-up-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-answer-7-slot1-cat)
               --
               (category slot1-answer-7-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))

;35_1
;answer(A,(population(B,A),largest(B,city(B))))

(def-fcg-cxn largest-1-cxn
             ((?largest-1-unit
               (meaning-args (layer ?largest-layer)
                             (target ?largest-target)
                             (embedded-layer ?embedded-layer))
               (boundaries (lh-leftmost ?grand.5-1)
                           (lh-rightmost ?grand.5-1)
                           (rh-leftmost ?grand.5-2)
                           (rh-rightmost ?grand.5-2))
               (category largest-1-cat))
              <-
              (?largest-1-unit
               (HASH meaning ((LARGEST ?largest-layer ?largest-target ?embedded-layer)))
               --
               (HASH form ((left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (coincides-relation ?grand.5-1 ?grand.5-2 equals)
               )))))


(def-fcg-cxn population-1-cxn
             ((?population-1-unit
               (meaning-args (layer ?population-layer)
                             (target ?population-target)
                             (embedded-target ?embedded-target))
               (boundaries (lh-leftmost ?personne.humain-1)
                           (lh-rightmost ?dans-1)
                           (rh-leftmost ?personne.humain-2)
                           (rh-rightmost ?dans-2))
               (category population-1-cat))
              <-
              (?population-1-unit
               (HASH meaning ((POPULATION ?population-layer ?embedded-target ?population-target)))
               --
               (HASH form ((left-hand-articulation ?personne.humain-1 personne.humain)
                           (right-hand-articulation ?personne.humain-2 personne.humain)
                           (coincides-relation ?personne.humain-1 ?personne.humain-2 equals)
                           (left-hand-articulation ?habiter-1 habiter)
                           (modification ?habiter-1 reduplicated)
                           (right-hand-articulation ?habiter-2 habiter)
                           (modification ?habiter-2 reduplicated)
                           (coincides-relation ?habiter-1 ?habiter-2 equals)
                           (left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (coincides-relation ?dans-1 ?dans-2 equals)
                           (meets ?personne.humain-1 ?habiter-1)
                           (meets ?personne.humain-1 ?habiter-2)
                           (meets ?habiter-1 ?dans-1)
                           (meets ?habiter-1 ?dans-2)
               )))))

(def-fcg-cxn slot1-slot2-answer-1-cxn
             ((?slot1-slot2-answer-1-unit
               (subunits (?slot1-unit ?slot2-unit))
               (category slot1-slot2-answer-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?combien-2)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot2-rh-right)))
              <-
              (?slot1-slot2-answer-1-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot2-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?combien-2 combien)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?combien-1 ?slot2-lh-left)
                           (meets ?combien-1 ?slot2-rh-left)
                           (meets ?slot2-lh-right ?combien-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-slot2-answer-1-slot1-cat)
               --
               (category slot1-slot2-answer-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location))
              (?slot2-unit
               (meaning-args (target ?slot2-target)
                             (embedded-target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-slot2-answer-1-slot2-cat)
               --
               (category slot1-slot2-answer-1-slot2-cat)
               (boundaries
                (lh-leftmost ?slot2-lh-left)
                (lh-rightmost ?slot2-lh-right)
                (rh-leftmost ?slot2-rh-left)
                (rh-rightmost ?slot2-rh-right)
               )
               )
              ))

(def-fcg-cxn slot1-city-1-cxn
             ((?slot1-city-1-unit
               (subunits (?slot1-unit))
               (category slot1-city-1-cat)
               (meaning-args (layer ?slot1-layer)
                             (target ?slot1-target))
               (boundaries (lh-leftmost ?dans-1)
                           (lh-rightmost ?ville-1)
                           (rh-leftmost ?dans-2)
                           (rh-rightmost ?ville-2)))
              <-
              (?slot1-city-1-unit
               (HASH meaning ((CITY ?slot1-embedded-layer ?slot1-target)))
               --
               (HASH form ((left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (coincides-relation ?dans-1 ?dans-2 equals)
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           (left-hand-articulation ?ville-1 ville)
                           (right-hand-articulation ?ville-2 ville)
                           (coincides-relation ?ville-1 ?ville-2 equals)
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?dans-1 ?ns-amerique.frites-2)
                           (meets ?ns-amerique.frites-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?il-y-a-1 ?slot1-rh-left)
                           (meets ?slot1-lh-right ?ville-1)
                           (meets ?slot1-lh-right ?ville-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer)
                             (embedded-layer ?slot1-embedded-layer))
                (category slot1-city-1-slot1-cat)
               --
               (category slot1-city-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))

;35_2

(def-fcg-cxn slot1-city-2-cxn
             ((?slot1-city-2-unit
               (subunits (?slot1-unit))
               (category slot1-city-2-cat)
               (meaning-args (layer ?slot1-layer)
                             (target ?slot1-target))
               (boundaries (lh-leftmost ?dans-1)
                           (lh-rightmost ?ville-1)
                           (rh-leftmost ?dans-2)
                           (rh-rightmost ?ville-2)))
              <-
              (?slot1-city-2-unit
               (HASH meaning ((CITY ?slot1-embedded-layer ?slot1-target)))
               --
               (HASH form ((left-hand-articulation ?dans-1 dans)
                           (right-hand-articulation ?dans-2 dans)
                           (coincides-relation ?dans-1 ?dans-2 equals)
                           (left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           (left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 r)
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           (left-hand-articulation ?ville-1 ville)
                           (right-hand-articulation ?ville-2 ville)
                           (coincides-relation ?ville-1 ?ville-2 equals)
                           (meets ?dans-1 ?ns-amerique.frites-1)
                           (meets ?dans-1 ?ns-amerique.frites-2)
                           (meets ?ns-amerique.frites-1 ?pt-1)
                           (meets ?pt1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?slot1-lh-left)
                           (meets ?il-y-a-1 ?slot1-rh-left)
                           (meets ?slot1-lh-right ?ville-1)
                           (meets ?slot1-lh-right ?ville-2)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer)
                             (embedded-layer ?slot1-embedded-layer))
                (category slot1-city-2-slot1-cat)
               --
               (category slot1-city-2-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))


(def-fcg-cxn slot1-population-2-cxn
             ((?slot1-population-2-unit
               (meaning-args (layer ?slot1-layer)
                             (target ?population-target))
               (subunits (?slot1-unit))
               (boundaries (lh-leftmost ?personne.humain-1)
                           (lh-rightmost ?habiter-1)
                           (rh-leftmost ?personne.humain-2)
                           (rh-rightmost ?habiter-2))
               (category slot1-population-2-cat))
              <-
              (?slot1-population-2-unit
               (HASH meaning ((POPULATION ?slot1-layer ?slot1-target ?population-target)))
               --
               (HASH form ((left-hand-articulation ?personne.humain-1 personne.humain)
                           (right-hand-articulation ?personne.humain-2 personne.humain)
                           (coincides-relation ?personne.humain-1 ?personne.humain-2 equals)
                           (left-hand-articulation ?habiter-1 habiter)
                           (modification ?habiter-1 reduplicated)
                           (right-hand-articulation ?habiter-2 habiter)
                           (modification ?habiter-2 reduplicated)
                           (coincides-relation ?habiter-1 ?habiter-2 equals)
                           (meets ?personne.humain-1 ?habiter-1)
                           (meets ?personne.humain-1 ?habiter-2)
                           (meets ?slot1-lh-right ?personne.humain-1)
                           (meets ?slot1-lh-right ?personne.humain-2)
               )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
                (category slot1-population-2-slot1-cat)
               --
               (category slot1-population-2-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )))


(def-fcg-cxn slot1-answer-8-cxn
             ((?slot1-answer-8-unit
               (subunits (?slot1-unit))
               (category slot1-answer-8-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?palm-up-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?combien-2)))
              <-
              (?slot1-answer-8-unit
               (HASH meaning ((ANSWER ?answer-layer ?slot1-target ?slot1-layer)))
               --
               (HASH form ((left-hand-articulation ?combien-1 combien)
                           (right-hand-articulation ?combien-2 combien)
                           (coincides-relation ?combien-1 ?combien-2 equals)
                           (left-hand-articulation ?palm-up-1 palm-up)
                           (meets ?slot1-lh-right ?combien-1)
                           (meets ?slot1-lh-right ?combien-2)
                           (meets ?combien-1 ?palm-up-1)
                           )))
              (?slot1-unit
               (meaning-args (target ?slot1-target)
                             (layer ?slot1-layer))
               (category slot1-answer-8-slot1-cat)
               --
               (category slot1-answer-8-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))



(add-categories '(usa-1-cat
                  usa-2-cat
                  alaska-1-cat
                  new_york-1-cat
                  rio_grande-1-cat
                  slot1-countryid-const-1-cat
                  slot1-countryid-const-1-slot1-cat
                  slot1-loc-state-1-cat
                  slot1-loc-state-1-slot1-cat
                  slot1-stateid-const-1-cat
                  slot1-stateid-const-1-slot1-cat
                  slot1-cityid-const-1-slot1-cat
                  slot1-size-1-slot1-cat
                  slot1-cityid-const-1-cat
                  const-placeid-slot1-1-slot1-cat
                  const-placeid-slot1-1-cat
                  slot1-elevation-1-slot1-cat
                  guadalupe_peak-1-cat
                  slot1-largest-1-slot1-cat
                  slot1-loc-state-1-cat
                  slot1-answer-1-slot1-cat
                  slot1-largest-1-cat
                  slot1-largest-2-cat
                  slot1-largest-2-slot1-cat
                  slot1-largest-2-cat
                  slot1-answer-2-slot1-cat
                  slot1-answer-3-slot1-cat
                  slot1-size-1-cat
                  slot1-answer-4-slot1-cat
                  slot1-answer-5-slot1-cat
                  slot1-elevation-1-cat
                  const-riverid-slot1-1-slot1-cat
                  const-riverid-slot1-1-cat
                  slot1-len-1-slot1-cat
                  slot1-answer-6-slot1-cat
                  slot1-len-1-cat
                  slot1-population-largest-1-slot1-cat
                  city-1-cat
                  slot1-answer-7-slot1-cat
                  slot1-population-largest-1-cat
                  slot1-city-1-slot1-cat
                  largest-1-cat
                  slot1-slot2-answer-1-slot1-cat
                  slot1-city-1-cat
                  slot1-slot2-answer-1-slot2-cat
                  population-1-cat
                  slot1-city-2-slot1-cat
                  slot1-city-2-cat
                  slot1-population-2-slot1-cat
                  slot1-population-2-cat
                  slot1-answer-8-slot1-cat)
                *fcg-constructions*)

(progn
  (add-link 'usa-1-cat 'slot1-countryid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'usa-2-cat 'slot1-countryid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-countryid-const-1-cat 'slot1-loc-state-1-slot1-cat *fcg-constructions*)
  (add-link 'alaska-1-cat 'slot1-stateid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'new_york-1-cat 'slot1-cityid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-cityid-const-1-cat 'slot1-size-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-stateid-const-1-cat 'slot1-size-1-slot1-cat *fcg-constructions*)
  (add-link 'const-placeid-slot1-1-slot1-cat 'guadalupe_peak-1-cat *fcg-constructions*)
  (add-link 'const-placeid-slot1-1-cat 'slot1-elevation-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-largest-1-slot1-cat 'slot1-loc-state-1-cat *fcg-constructions*)
  (add-link 'slot1-answer-1-slot1-cat 'slot1-largest-1-cat *fcg-constructions*)
  (add-link 'slot1-largest-2-slot1-cat 'slot1-loc-state-1-cat *fcg-constructions*)
  (add-link 'slot1-largest-2-cat 'slot1-answer-2-slot1-cat *fcg-constructions*)
  (add-link 'slot1-answer-3-slot1-cat 'slot1-size-1-cat *fcg-constructions*)
  (add-link 'slot1-size-1-slot1-cat 'slot1-cityid-const-1-cat *fcg-constructions*)
  (add-link 'slot1-answer-4-slot1-cat 'slot1-size-1-cat *fcg-constructions*)
  (add-link 'slot1-answer-5-slot1-cat 'slot1-elevation-1-cat *fcg-constructions*)
  (add-link 'const-riverid-slot1-1-slot1-cat 'rio_grande-1-cat *fcg-constructions*)
  (add-link 'const-riverid-slot1-1-cat 'slot1-len-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-answer-6-slot1-cat 'slot1-len-1-cat *fcg-constructions*)
  (add-link 'slot1-population-largest-1-slot1-cat 'city-1-cat *fcg-constructions*)
  (add-link 'slot1-answer-7-slot1-cat 'slot1-population-largest-1-cat *fcg-constructions*)
  (add-link 'slot1-city-1-slot1-cat 'largest-1-cat *fcg-constructions*)
  (add-link 'slot1-slot2-answer-1-slot1-cat 'slot1-city-1-cat *fcg-constructions*)
  (add-link 'slot1-slot2-answer-1-slot2-cat 'population-1-cat *fcg-constructions*)
  (add-link 'largest-1-cat 'slot1-city-2-slot1-cat *fcg-constructions*)
  (add-link 'slot1-city-2-cat 'slot1-population-2-slot1-cat *fcg-constructions*)
  (add-link 'slot1-population-2-cat 'slot1-answer-8-slot1-cat *fcg-constructions*))