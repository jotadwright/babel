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

; 11_0 & 11_1
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
                           (coincides-relation ?ns-amerique.frites-1 ?ns-amerique.frites-2 equals)
                           ))
               )))

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
               (HASH meaning ((CONST ?E ?B ?F)(COUNTRYID ?F ?G)))
               
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
               (meaning-args (?G))
               --
               (category slot1-countryid-const-1-slot1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-loc-state-1-cxn
             ((?slot1-loc-state-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?B ?A))
               (category slot1-loc-state-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?dss-entite-geographique.figure-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?dss-entite-geographique.ground-1))
               (spatial-agreement ?location))
              <-
              (?slot1-loc-state-1-unit
               (HASH meaning ((LOC ?B ?A ?E)(STATE ?B ?A)))
               
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
               (meaning-args (?E ?B))
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
               (meaning-args (?D ?E))
               (category slot1-largest-1-cat)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?grand.5-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?grand.5-2))
               (spatial-agreement ?location))
              <-
              (?slot1-largest-1-unit
               (HASH meaning ((LARGEST ?D ?E ?A)))
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
               (meaning-args (?E ?A))
               --
               (category slot1-largest-1-slot1-cat)
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
               (HASH meaning ((ANSWER ?C ?D ?A)))
               
               --
               (HASH form ((left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (coincides-relation ?ou-lieu.o-1 ?ou-lieu.o-2 equals)
                           (meets ?slot1-lh-right ?ou-lieu.o-1)
                           (meets ?slot1-lh-right ?ou-lieu.o-2)
                           ))
               )
              (?slot1-unit
               (meaning-args (?D ?A))
               --
               (category slot1-answer-1-slot1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)
                           ))
              ))


; 15_0_ALASKA

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
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (coincides-relation ?slot1-lh-right ?dss-entite-geographique.ground-1 meets)
                           ))
               )
              (?slot1-unit
               (meaning-args (?H))
               (category slot1-stateid-const-1-slot1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

;16_0_NEW_YORK

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
                           (coincides-relation ?ns-new-york.y-loc-1 ?ns-new-york.y-loc-2 equals)
                           ))
               )))

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
                           (main-location ?dss-entite-geographique.ground-1 ?location)
                           (right-hand-articulation ?dss-entite-geographique.ground-2 dss-entite-geographique.ground)
                            (main-location ?dss-entite-geographique.ground-2 ?location)
                           (coincides-relation ?dss-entite-geographique.ground-1 ?dss-entite-geographique.ground-2 starts)
                           (meets ?slot1-lh-right ?dss-entite-geographique.ground-1)
                           (coincides-relation ?slot1-lh-right ?dss-entite-geographique.ground-2 meets)
                           ))
               )
              (?slot1-unit
               (meaning-args (?H))
               (category slot1-cityid-const-1-slot1-cat)
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
                           (main-location ?surface-1 ?location)
                           (left-hand-articulation ?combien-1 combien)
                           (left-hand-articulation ?metre-carre-1 metre-carre)
                           (meets ?surface-1 ?combien-1)
                           (meets ?combien-1 ?metre-carre-1)
                           (coincides-relation ?surface-1 ?slot1-rh-right ?relation-1)
                           ;(coincides-relation ?combien-1 ?slot1-rh-right during)
                           ;(coincides-relation ?metre-carre-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (?E ?B))
               (category slot1-size-answer-1-slot1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               (spatial-agreement ?location)
               )))

;18_0_GUADALUPE-PEAK

(def-fcg-cxn guadalupe_peak-1-cxn
             ((?guadalupe_peak-1-unit
               (meaning-args (?F))
               (boundaries (lh-leftmost ?fs-guadalupe-1)
                           (lh-rightmost ?fs-guadalupe-1))
               (category guadalupe_peak-1-cat))
               <-
              (?guadalupe_peak-1-unit
               (HASH meaning ((GUADALUPE_PEAK ?F)))
               --
               (HASH form ((left-hand-articulation ?fs-guadalupe-1 fs-guadalupe)))
               )))

(def-fcg-cxn const-placeid-slot1-1-cxn
             ((?const-placeid-slot1-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?D ?B))
               (category const-placeid-slot1-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?montagne-1)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?montagne-2)
                           (rh-rightmost ?montagne-2)))
              <-
              (?const-placeid-slot1-1-unit
               (HASH meaning ((CONST ?D ?B ?E)(PLACEID ?E ?F)))
               
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
                           (coincides-relation ?pt-1 ?montagne-2 during)
                           (coincides-relation ?pt-2 ?montagne-2 during)
                           (coincides-relation ?slot1-lh-left ?montagne-2 during)
                           )))
              (?slot1-unit
               (meaning-args (?F))
               (category const-placeid-slot1-1-slot1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right))
               )
              ))

(def-fcg-cxn slot1-elevation-1-cxn
             ((?slot1-elevation-1-unit
               (subunits (?slot1-unit))
               (meaning-args (?A ?D))
               (category slot1-elevation-1-cat)
               (spatial-agreement ?location)
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?pt-1)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right)))
              <-
              (?slot1-elevation-1-unit
               (HASH meaning ((ELEVATION ?D ?B ?A)))
               
               --
               (HASH form ((left-hand-articulation ?pt-1 pt)
                           (main-location ?pt-1 ?location)
                           (modification ?pt-1 down-up)
                           (meets ?slot1-lh-right ?pt-1)
                           (coincides-relation ?pt-1 ?slot1-rh-right finishes)
                           )))
              (?slot1-unit
               (meaning-args (?D ?B))
               (category slot1-elevation-1-slot1-cat)
               --
               (boundaries (lh-leftmost ?slot1-lh-left)
                           (lh-rightmost ?slot1-lh-right)
                           (rh-leftmost ?slot1-rh-left)
                           (rh-rightmost ?slot1-rh-right))
               )
              ))







             

(add-categories '(usa-1-cat
                  alaska-1-cat
                  new_york-1-cat
                  slot1-countryid-const-1-cat
                  slot1-countryid-const-1-slot1-cat
                  slot1-loc-state-1-cat
                  slot1-loc-state-1-slot1-cat
                  slot1-stateid-const-1-cat
                  slot1-stateid-const-1-slot1-cat
                  slot1-cityid-const-1-slot1-cat
                  slot1-size-answer-1-slot1-cat
                  slot1-cityid-const-1-cat
                  const-placeid-slot1-1-slot1-cat
                  const-placeid-slot1-1-cat
                  slot1-elevation-1-slot1-cat
                  guadalupe_peak-1-cat
                  slot1-largest-1-slot1-cat
                  slot1-loc-state-1-cat
                  slot1-answer-1-slot1-cat
                  slot1-largest-1-cat)
                *fcg-constructions*)

(progn
  (add-link 'usa-1-cat 'slot1-countryid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-countryid-const-1-cat 'slot1-loc-state-1-slot1-cat *fcg-constructions*)
  (add-link 'alaska-1-cat 'slot1-stateid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'new_york-1-cat 'slot1-cityid-const-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-cityid-const-1-cat 'slot1-size-answer-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-stateid-const-1-cat 'slot1-size-answer-1-slot1-cat *fcg-constructions*)
  (add-link 'const-placeid-slot1-1-slot1-cat 'guadalupe_peak-1-cat *fcg-constructions*)
  (add-link 'const-placeid-slot1-1-cat 'slot1-elevation-1-slot1-cat *fcg-constructions*)
  (add-link 'slot1-largest-1-slot1-cat 'slot1-loc-state-1-cat *fcg-constructions*)
  (add-link 'slot1-answer-1-slot1-cat 'slot1-largest-1-cat *fcg-constructions*))