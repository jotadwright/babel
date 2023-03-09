(in-package :sign-grammar)

;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;

(def-fcg-constructions sign-grammar
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (holds set-of-predicates)
                  (boundaries set-of-predicates)
                  (handconfiguration set-of-predicates)
                  (location-params set-of-predicates)
                  
                  )
  :hierarchy-features (subunits)
  :diagnostics ()
  :repairs ()
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                       (:max-number-of-nodes . 200)
                       (:max-search-depth . 50)
                       (:parse-order cxn)
                       (:production-order cxn)
                       (:render-mode . :set-of-predicates)
                       (:de-render-mode . :set-of-predicates)
                       (:form-predicates sign manual non-manual mouthpicture symmetry handshape extfingerdir palmori location movement finger2345 between flathand mod touch shoulders beginpar down arcr replace palmori endpar meets)
                       (:use-meta-layer . nil)
                       (:consolidate-repairs . nil)
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
                                 (:select-subfeatures . nil)))


;;-----------------------;;
;; Lexical Constructions ;;
;;-----------------------;;

(def-fcg-cxn car-cxn
             ((?car-unit
               (referent ?r)
               (ontological-class two-track-vehicle)
               (sem-cat concept)
               (syn-cat noun)
               (right-temporal-boundary ?m-1)
               (left-temporal-boundary ?h-1)
               (eyegaze ?eg-1))
               <-
               (?car-unit
                (hash meaning ((car ?r)))
                --
                (hash form
                      ((ref-point ?ref-1)
                       (focal-point ?ref-1 chest)
                       (bearing ?ref-1 level-ahead)
                       (distance ?ref-1 medial)
                       (ref-point ?ref-2)
                       (focal-point ?ref-2 ?ref-1)
                       (bearing ?ref-2 level-right)
                       (distance ?ref-2 proximal)
                       (ref-point ?ref-3)
                       (focal-point ?ref-3 ?ref-1)
                       (bearing ?ref-3 level-left)
                       (distance ?ref-3 proximal)
                       (hold ?h-1)
                       (hand ?h-1 strong)
                       (handshape ?h-1 fist)
                       (extended-finger-direction ?h-1 left)
                       (palm-orientation ?h-1 left)
                       (position ?ref-2)
                       (hold ?h-2)
                       (hand ?h-2 weak)
                       (handshape ?h-2 fist)
                       (extended-finger-direction ?h-2 right)
                       (palm-orientation ?h-2 right)
                       (position ?ref-3)
                       (local-movement ?m-1)
                       (handconfiguration ?m-1 ?h-1)
                       (movement ?m-1 up-down)
                       (modifier ?m-1 repeated)
                       (local-movement ?m-2)
                       (handconfiguration ?m-2 ?h-2)
                       (movement ?m-2 down-up)
                       (modifier ?m-2 repeated)
                       (mouthpicture ?mp-1)
                       (picture ?mp-1 car)
                       (eye-gaze ?eg-1)
                       (direction ?eg-1 adressee)
                       (meets ?h-1 ?m-1)
                       (meets ?h-2 ?m-2)
                       (equal ?h-1 ?h-2)
                       (equal ?m-1 ?m-2)
                       (overlaps ?h-1 ?mp-1)
                       (overlaps ?mp-1 ?m-1)
                       (overlaps ?h-1 ?eg-1)
                       (overlaps ?eg-1 ?m-1))))))

(def-fcg-cxn child-cxn
             ((?child-unit
               (referent ?r)
               (ontological-class person)
               (sem-cat concept)
               (syn-cat noun)
               (right-temporal-boundary ?m-1)
               (left-temporal-boundary ?h-1)
               (eyegaze ?eg-1))
              <-
              (?child-unit
               (hash meaning ((child ?r)))
               --
               (hash form
                     ((ref-point ?ref-1)
                      (focal-point ?ref-1 chest)
                      (bearing ?ref-1 level-rightward)
                      (distance ?ref-1 medial)
                      (hold ?h-1)
                      (hand ?h-1 strong)
                      (handshape ?h-1 pinchall)
                      (extended-finger-direction ?h-1 up)
                      (palm-orientation ?h-1 up)
                      (position ?ref-1)
                      (local-movement ?m-1)
                      (handconfiguration ?m-1 ?h-1)
                      (movement ?m-1 down-up)
                      (modifier ?m-1 repeated)
                      (mouthpicture ?mp-1)
                      (picture ?mp-1 child)
                      (eye-gaze ?eg-1)
                      (direction ?eg-1 adressee)
                      (meets ?h-1 ?m-1)
                      (overlaps ?h-1 ?mp-1)
                      (overlaps ?mp-1 ?m-1)
                      (overlaps ?h-1 ?eg-1)
                      (overlaps ?eg-1 ?m-1))))))

(def-fcg-cxn vehicle-classifier-cxn
             ((?classifier-unit
               (hand ?hand)
               (initial-position ?ref-1)
               (end-position ?ref-2)
               (syn-cat classifier)
               (left-temporal-boundary ?h-1)
               (right-temporal-boundary ?h-2)
               (handshape flat)
               (extended-finger-direction out)
               (palm-orientation down)
               (movement ?m-1)
               (eye-gaze ?eg-1))
              <-
              (?classifier-unit
               (ontological-class two-track-vehicle)
               --
               (hash form
                     ((hold ?h-1)
                      (hand ?h-1 ?hand)
                      (handshape ?h-1 flat)
                      (extended-finger-direction ?h-1 out)
                      (palm-orientation ?h-1 down)
                      (position ?h-1 ?ref-1)
                      (path-movement ?m-1)
                      (initial-hold ?m-1 ?h-1)
                      (end-hold ?m-1 ?h-2)
                      (hold ?h-2)
                      (hand ?h-2 ?hand)
                      (handshape ?h-2 flat)
                      (extended-finger-direction ?h-2 out)
                      (palm-orientation ?h-2 down)
                      (position ?h-2 ?ref-2)
                      (eye-gaze ?eg-1)
                      (direction ?eg-1 ?ref-2)
                      (meets ?h-1 ?m-1)
                      (meets ?m-1 ?h-2)
                      (overlaps ?h-1 ?eg-1)
                      (overlaps ?eg-1 ?h-2))))))

(def-fcg-cxn person-classifier-cxn
             ((?classifier-unit
               (hand ?hand)
               (syn-cat classifier)
               (initial-position ?ref-1)
               (end-position ?ref-2)
               (left-temporal-boundary ?h-1)
               (right-temporal-boundary ?h-2)
               (handshape finger2)
               (extended-finger-direction up)
               (palm-orientation left)
               (movement ?m-1)
               (eye-gaze ?eg-1))
              <-
              (?classifier-unit
               (ontological-class person)
               --
               (hash form
                     ((hold ?h-1)
                      (hand ?h-1 ?hand)
                      (handshape ?h-1 finger2)
                      (extended-finger-direction ?h-1 up)
                      (palm-orientation ?h-1 left)
                      (position ?h-1 ?ref-1)
                      (path-movement ?m-1)
                      (initial-hold ?m-1 ?h-1)
                      (end-hold ?m-1 ?h-2)
                      (hold ?h-2)
                      (hand ?h-2 ?hand)
                      (handshape ?h-2 finger2)
                      (extended-finger-direction ?h-2 up)
                      (palm-orientation ?h-2 left)
                      (position ?h-2 ?ref-2)
                      (eye-gaze ?eg-1)
                      (direction ?eg-1 ?ref-2))))))

(def-fcg-cxn vehicle-identifier-cxn
             ((?classifier-unit
               (ontological-class two-track-vehicle)
               (noun-eyegaze ?eg-3)
               (noun-hold ?h-5)
               (subunits (?vehicle-unit)))
              <-
              (?classifier-unit
               (referent ?r)
               --
               (left-temporal-boundary ?h-3)
               (handshape flat)
               (extended-finger-direction out)
               (palm-orientation down)
               (hash form ((meets ?m-2 ?h-3)))
               (syn-cat classifier))
              (?vehicle-unit
               (ontological-class two-track-vehicle)
               (referent ?r)
               --
               (right-temporal-boundary ?m-2)
               (syn-cat noun)
               (eyegaze ?eg-3)
               (left-temporal-boundary ?h-5)
               )))

(def-fcg-cxn person-identifier-cxn
             ((?classifier-unit
               (noun-eyegaze ?eg-3)
               (noun-hold ?h-5)
               (ontological-class person)
               (?approaches-unit
               (subunits (?person-unit))))
              <-
              (?classifier-unit
               (referent ?r)
               --
               (left-temporal-boundary ?h-3)
               (handshape finger2)
               (extended-finger-direction up)
               (palm-orientation left)
               (hash form ((meets ?m-2 ?h-3)))
               (syn-cat classifier))
              (?person-unit
               (ontological-class person)
               (referent ?r)
               --
               (right-temporal-boundary ?m-2)
               (syn-cat noun)
               (eyegaze ?eg-3)
               (left-temporal-boundary ?h-5)
               )))

(def-fcg-cxn x-approaches-y-cxn
             ((?x-unit
               (referent ?x))
              (?y-unit
               (referent ?y))
              (?approaches-unit
               (subunits (?x-unit ?y-unit)))
              <-
              (?x-unit
               --
               (right-temporal-boundary ?h-7)
               (hand ?hand-1)
               (handshape ?handshape-1)
               (extended-finger-direction ?extended-finger-direction-1)
               (palm-orientation ?palm-orientation-1)
               (initial-position ?ref-6)
               (end-position ?ref-5)
               (noun-eyegaze ?eg-3)
               (noun-hold ?h-5)
               (syn-cat classifier)
               (hash form
                     ((ref-point ?ref-5)
                      (focal-point ?ref-5 chest)
                      (bearing ?ref-5 downward-rightward)
                      (distance ?ref-5 medial))))
              (?approaches-unit
               (hash meaning
                     ((approach.01 ?a)
                      (arg1 ?a ?x)
                      (arg2 ?a ?y)))
               --
               (hash form
                     ((hold ?h-8)
                      (hand ?h-8 ?hand)
                      (handshape ?h-8 ?handshape-1)
                      (extended-finger-direction ?h-8 ?extended-finger-direction-1)
                      (palm-orientation ?h-8 ?palm-orientation-1)
                      (position ?h-8 ?ref-3)
                      (eye-gaze ?eg-5)
                      (direction ?eg-5 strong-hand)
                      (path-movement ?m-6)
                      (initial-hold ?m-6 ?h-7)
                      (end-hold ?m-6 ?h-8)
                      (meets ?h-7 ?m-6)
                      (meets ?m-6 ?h-8)
                      (overlaps ?m-6 ?eg-5)
                      (overlaps ?eg-5 ?h-8)
                      (finishes ?h-8 ?h-4)
                      (before ?m-3 ?h-5)
                      (before ?eg-2 ?eg-3))))
              (?y-unit
               --
               (right-temporal-boundary ?h-4)
               (syn-cat classifier)
               (handshape ?handshape-2)
               (extended-finger-direction ?extended-finger-direction-2)
               (palm-orientation ?palm-orientation-2)
               (movement ?m-3)
               (eye-gaze ?eg-2)
               (initial-position ?ref-3)
               (end-position ?ref-4)
               (hash form
                     ((ref-point ?ref-4)
                      (focal-point ?ref-4 chest)
                      (bearing ?ref-4 downward-leftward)
                      (distance ?ref-4 medial))))))
              
                      
