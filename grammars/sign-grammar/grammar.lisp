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
                  (meaning-args set-of-predicates)
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

(def-fcg-cxn where.5-cxn
             ((?where.5-unit
               (form-args ((lh-unit ?ou-lieu.5-1))))

              <-
              (?where.5-unit
               (category where.5-cat)
               --
               (HASH form ((left-hand-articulation ?ou-lieu.5-1 ou-lieu.5)
                           (right-hand-articulation ?ou-lieu.5-2 ou-lieu.5)
                           (equals ?ou-lieu.5-1 ?ou-lieu.5-2))))))

(def-fcg-cxn where.o-cxn
             ((?where.o-unit
               (form-args ((lh-unit ?ou-lieu.o-1))))
              <-
              (?where.o-unit
               (category where.o-cat)
               --
               (HASH form ((left-hand-articulation ?ou-lieu.o-1 ou-lieu.o)
                           (right-hand-articulation ?ou-lieu.o-2 ou-lieu.o)
                           (equals ?ou-lieu.o-1 ?ou-lieu.o-2))))))


(def-fcg-cxn america-has-X-where-cxn
             ((?america-has-X-where-unit
               (subunits (?x-unit ?where-unit)))
              <-
              (?america-has-X-where-unit
               (HASH meaning ((answer ?B ?A ?C)))
               --
               (HASH form ((left-hand-articulation ?ns-amerique.frites-1 ns-amerique.frites)
                           (right-hand-articulation ?ns-amerique.frites-2 ns-amerique.frites)
                           (left-hand-articulation ?dss-l-vert-rssp-1 dss-l-vert-rssp)
                           (right-hand-articulation ?dss-l-vert-rssp-2 dss-l-vert-rssp)
                           (left-hand-articulation ?il-y-a-1 il-y-a)
                           (meets ?ns-amerique.frites-1 ?dss-l-vert-rssp-1)
                           (meets ?dss-l-vert-rssp-1 ?il-y-a-1)
                           (meets ?il-y-a-1 ?x-lh-leftmost)
                           (meets ?x-lh-rightmost ?where-unit-boundary)
                           (equals ?ns-amerique.frites-1  ?ns-amerique.frites-2)
                           (equals ?dss-l-vert-rssp-1 ?dss-l-vert-rssp-2))))
              (?x-unit
               (category america-has-X-where-cat)
               --
               (category america-has-X-where-cat)
               (form-args ((lh-leftmost ?x-lh-leftmost)
                           (lh-rightmost ?x-lh-rightmost)))
               (meaning-args ((answer ?A)
                              (partial-network ?C)))
               )
              (?where-unit
               --
               (form-args ((lh-unit ?where-unit-boundary)))
               (category where-cat)
               )))

(def-fcg-cxn X-one-is-most-large-cxn
             ((?X-one-is-most-large-unit
               (meaning-args ((answer ?A)
                              (partial-network ?C)))
               (form-args ((lh-leftmost ?x-lh-leftmost)
                           (lh-rightmost ?grand.5-1)))
               (category X-one-is-most-large-cat)
               (subunits (?one-is-most-large-unit ?x-unit)))
              <-
              (?one-is-most-large-unit
               (HASH meaning ((LARGEST ?C ?A ?D)))
               --
               (HASH form ((left-hand-articulation ?un-1 un)
                           (left-hand-articulation ?pt-rssp-1 pt-rssp)
                           (left-hand-articulation ?plus.p-1 plus.p)
                           (left-hand-articulation ?grand.5-1 grand.5)
                           (right-hand-articulation ?grand.5-2 grand.5)
                           (equals ?grand.5-1 ?grand.5-2)))
              )
              (?x-unit
               (meaning-args ((answer ?A)
                              (partial-network ?D)))
               (category X-one-is-most-large-cat)
               --
               (form-args ((lh-leftmost ?x-lh-leftmost)
                           (lh-rightmost ?x-lh-rightmost)
                           (rh-buoy ?x-rh-buoy)))
               (category X-one-is-most-large-cat)
              )
              (?X-one-is-most-large-unit
               --
               (HASH form ((meets ?x-rh-rightmost ?un-1)
                           (during ?un-1 ?x-rh-buoy)
                           (during ?pt-rssp-1 ?x-rh-buoy)
                           (finishes ?plus.p-1 ?x-rh-buoy)))
              )))

(def-fcg-cxn X-one-has-most-people-live-cxn
             ((?X-one-has-most-people-live-unit
               (meaning-args ((answer ?A)
                              (partial-network ?D)))
               (form-args ((lh-leftmost ?x-lh-leftmost)
                           (lh-rightmost ?habiter++-1)))
               (category X-one-has-most-people-live-cat)
               (subunits (?one-has-most-people-live-unit ?x-unit)))
              <-
              (?one-has-most-people-live-unit
               (HASH meaning ((LARGEST ?D ?B ?E)
                              (POPULATION ?E ?A ?B)))
               --
               (HASH form ((left-hand-articulation ?un-1 un)
                           (left-hand-articulation ?pt-rssp-1 pt-rssp)
                           (left-hand-articulation ?plus.p-1 plus.p)
                           (left-hand-articulation ?personne.humain-1 personne.humain)
                           (right-hand-articulation ?personne.humain-2 personne.humain)
                           (left-hand-articulation ?habiter++-1 habiter++)
                           (right-hand-articulation ?habiter++-2 habiter++)
                           (equals ?personne.humain-1 ?personne.humain-2)
                           (equals ?habiter++-1 ?habiter++-2)))
              )
              (?x-unit
               (meaning-args ((answer ?A)
                              (partial-network ?E)))
               (category X-one-has-most-people-live-cat)
               --
               (form-args ((lh-leftmost ?x-lh-leftmost)
                           (lh-rightmost ?x-lh-rightmost)
                           (rh-buoy ?x-rh-buoy)))
               (category X-one-has-most-people-live-cat)
              )
              (?X-one-has-most-people-live-unit
               --
               (HASH form ((meets ?x-rh-rightmost ?un-1)
                           (during ?un-1 ?x-rh-buoy)
                           (during ?pt-rssp-1 ?x-rh-buoy)
                           (finishes ?plus.p-1 ?x-rh-buoy)))
              )))

(def-fcg-cxn states-cxn
             ((?states-unit
               (meaning-args ((answer ?A)
                              (partial-network ?D)))
               (form-args ((lh-leftmost ?dss-bent4-rssp++-1)
                           (lh-rightmost ?dss-bent4-rssp++-1)
                           (rh-buoy ?fbuoy-l-vert-rssp-1)))
               (category states-cat))
              <-
              (?states-unit
               (HASH meaning ((STATE ?D ?A)))
               --
               (HASH form ((left-hand-articulation ?dss-bent4-rssp++-1 dss-bent4-rssp++)
                           (right-hand-articulation ?fbuoy-l-vert-rssp-1 fbuoy-l-vert-rssp)
                           (during ?dss-bent4-rssp++-1 ?fbuoy-l-vert-rssp-1))
               ))))

(def-fcg-cxn capitals-cxn
             ((?capitals-unit
               (meaning-args ((answer ?A)
                              (partial-network ?D)))
               (form-args ((lh-leftmost ?different-1)
                           (lh-rightmost ?pt-rssp++-1)
                           (rh-buoy ?fbuoy-l-vert-rssp-1)))
               (category capitals-cat))
              <-
              (?capitals-unit
               (HASH meaning ((CAPITAL ?D ?A)))
               --
               (HASH form ((left-hand-articulation ?different-1 different)
                           (right-hand-articulation ?different-2 different)
                           (left-hand-articulation ?capitale.mid-1 capitale.mid)
                           (right-hand-articulation ?capitale.mid-2 capitale.mid)
                           (left-hand-articulation ?pt-rssp++-1 pt-rssp++)
                           (right-hand-articulation ?fbuoy-l-vert-rssp-1 fbuoy-l-vert-rssp)
                           (starts ?pt-rssp++-1 ?fbuoy-l-vert-rssp-1))
               ))))

(def-fcg-cxn capitals++-cxn
             ((?capitals-unit
               (meaning-args ((answer ?A)
                              (partial-network ?D)))
               (form-args ((lh-leftmost ?different-1)
                           (lh-rightmost ?pt-rssp++-1)
                           (rh-buoy ?fbuoy-l-vert-rssp-1)))
               (category capitals-cat))
              <-
              (?capitals-unit
               (HASH meaning ((CAPITAL ?D ?A)))
               --
               (HASH form ((left-hand-articulation ?different-1 different)
                           (right-hand-articulation ?different-2 different)
                           (left-hand-articulation ?capitale.mid++-1 capitale.mid++)
                           (right-hand-articulation ?capitale.mid++-2 capitale.mid++)
                           (left-hand-articulation ?pt-rssp++-1 pt-rssp++)
                           (right-hand-articulation ?fbuoy-l-vert-rssp-1 fbuoy-l-vert-rssp)
                           (starts ?pt-rssp++-1 ?fbuoy-l-vert-rssp-1))
               ))))


(add-categories '(capitals-cat
                  states-cat
                  X-one-is-most-large-cat
                  america-has-X-where-cat
                  X-one-has-most-people-live-cat
                  where-cat
                  where.5-cat
                  where.o-cat)
                *fcg-constructions*)

(progn
  (add-link 'X-one-is-most-large-cat 'america-has-X-where-cat *fcg-constructions*)
  (add-link 'X-one-has-most-people-live-cat 'america-has-X-where-cat *fcg-constructions*)
  (add-link 'states-cat 'X-one-has-most-people-live-cat *fcg-constructions*)
  (add-link 'capitals-cat 'X-one-has-most-people-live-cat *fcg-constructions*)
  (add-link 'states-cat 'X-one-is-most-large-cat *fcg-constructions*)
  (add-link 'capitals-cat 'X-one-is-most-large-cat *fcg-constructions*)
  (add-link 'where-cat 'where.5-cat *fcg-constructions*)
  (add-link 'where-cat 'where.o-cat *fcg-constructions*)
  )





