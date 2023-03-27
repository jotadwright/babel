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
                  (noun-boundaries set-of-predicates)
                  (classifier-boundaries set-of-predicates)
                  (classifier-configurations set-of-predicates)
                  (noun-configurations set-of-predicates)
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
                                 (:select-subfeatures . nil)))


;;-----------------------;;
;; Lexical Constructions ;;
;;-----------------------;;

(def-fcg-cxn ball-cxn 
             ((?ball-unit 
               (referent ?b)
               (syn-cat noun)
               ;(sem-cat concept)
               ;(ontological-class round-object)
               (salient-movf ?movf-2)
               ;(sign-var ?sign-1)
               )
              <- 
              (?ball-unit 
               (HASH meaning ((ball ?b)))
               -- 
               (HASH form ((SIGN ?SIGN-1)
                           (NON-MANUAL ?SIGN-1 ?NM-1)
                           (MOUTHPICTURE ?NM-1 "ball")
                           (MANUAL ?SIGN-1 ?M-1)
                           (SYMMETRY ?M-1 ?SYM-1)
                           (HAMSYMMLR ?SYM-1 ?SYMF-1)
                           (HANDSHAPE ?M-1 ?HS-1)
                           (HAMFINGER2345 ?HS-1 ?HSF-1)
                           (HAMBETWEEN ?HS-1 ?HSF-2)
                           (HAMFLATHAND ?HS-1 ?HSF-3)
                           (HAMMOD ?HSF-3 HAMFINGERBENDMOD)
                           (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                           (HAMEXTFINGEROL ?EXT-1 ?EXTF-1)
                           (PALM-ORIENTATION ?M-1 ?ORI-1)
                           (HAMPALMDL ?ORI-1 ?ORIF-1)
                           (LOCATION ?M-1 ?LOC-1)
                           (HAMTOUCH ?LOC-1 ?LOCF-1)
                           (HAMSHOULDERS ?LOC-1 ?LOCF-2)
                           (MOVEMENT ?M-1 ?MOV-1)
                           (HAMPARBEGIN ?MOV-1 ?MOVF-1)
                           (HAMMOVED ?MOV-1 ?MOVF-2)
                           (HAMARCR ?MOV-1 ?MOVF-3)
                           (HAMREPLACE ?MOV-1 ?MOVF-4)
                           (PALM-ORIENTATION ?M-1 ?ORI-2)
                           (HAMPALMU ?ORI-2 ?ORIF-2)
                           (HAMPAREND ?ORI-2 ?ORIF-3)
                           (LOCATION ?M-1 ?LOC-2)
                           (HAMTOUCH ?LOC-2 ?LOCF-3)
                           (MEETS ?SYMF-1 ?HSF-1)
                           (MEETS ?HSF-1 ?HSF-2)
                           (MEETS ?HSF-2 ?HSF-3)
                           (MEETS ?HSF-3 ?EXTF-1)
                           (MEETS ?EXTF-1 ?ORIF-1)
                           (MEETS ?ORIF-1 ?LOCF-1)
                           (MEETS ?LOCF-1 ?LOCF-2)
                           (MEETS ?LOCF-2 ?MOVF-1)
                           (MEETS ?MOVF-1 ?MOVF-2)
                           (MEETS ?MOVF-2 ?MOVF-3)
                           (MEETS ?MOVF-3 ?MOVF-4)
                           (MEETS ?MOVF-4 ?ORIF-2)
                           (MEETS ?ORIF-2 ?ORIF-3)
                           (MEETS ?ORIF-3 ?LOCF-3))))))

(def-fcg-cxn big-cxn 
             ((?modifier-unit
               (syn-cat adjective))
              (?noun-unit
               (subunits (?modifier-unit)))
              <- 
              (?noun-unit 
               (referent ?r)
               --
               (syn-cat noun)
               (salient-movf ?movf))
              
              (?modifier-unit
               (HASH meaning ((mod ?r big)))
               --
               (HASH form ((hammod ?movf hamlargemod)))))) 

; construction for modifier small
(def-fcg-cxn small-cxn 
             ((?modifier-unit
               (syn-cat adjective))
              (?noun-unit
               (subunits (?modifier-unit)))
              <- 
              (?noun-unit 
               (referent ?r)
               --
               (syn-cat noun)
               (salient-movf ?movf))
              
              (?modifier-unit
               (HASH meaning ((mod ?r small)))
               --
               (HASH form ((hammod ?movf hamsmallmod))))))

(def-fcg-cxn car-cxn
             ((?car-unit
               (referent ?r)
               (ontological-class two-track-vehicle)
               (sem-cat concept)
               (syn-cat noun)
               (boundaries ((right-manual-boundary ?lm-1)
                            (left-manual-boundary ?h-1))))
               <-
               (?car-unit
                (hash meaning ((car ?r)))
                --
                (hash form
                      ((displacement ?dp-1)
                       (bearing ?dp-1 ?b-5)
                       (focal-site ?b-5 chest)
                       (x-direction ?b-5 ahead)
                       (y-direction ?b-5 level)
                       (z-distance ?dp-1 medial)
                       (hold ?h-1)
                       (articulator ?h-1 strong-hand)
                       (handconfiguration ?h-1 ?hc-1)
                       (placement ?h-1 ?pl-1)
                       (orientation ?h-1 ?or-1)
                       (shape ?hc-1 fist)
                       (bearing ?pl-1 ?b-1)
                       (focal-site ?b-1 ?dp-1)
                       (z-distance ?pl-1 medial)
                       (x-direction ?b-1 right)
                       (y-direction ?b-1 level)
                       (reference-bearing ?or-1 ?b-1)
                       (x-direction ?or-1 right)
                       (y-direction ?or-1 upward)
                       (local-movement ?lm-1)
                       (articulator ?lm-1 strong-hand)
                       (handconfiguration ?lm-1 ?hc-1)
                       (placement ?lm-1 ?pl-1)
                       (orientation ?lm-1 ?or-1)
                       (initial-movement ?lm-1 down-left)
                       (end-movement ?lm-1 up-right)
                       (modifier ?lm-1 repeat-several)
                       (hold ?h-2)
                       (articulator ?h-2 weak-hand)
                       (symmetry-reference ?h-2 ?h-1)
                       (symmetry-relation ?h-2 vertical)
                       (local-movement ?lm-2)
                       (articulator ?lm-2 weak-hand)
                       (symmetry-reference ?lm-2 ?lm-1)
                       (symmetry-relation ?lm-2 vertical)
                       (mouthpicture ?mp-1)
                       (articulator ?mp-1 mouth)
                       (picture ?mp-1 "car")
                       (eyegaze ?eg-1)
                       (articulator ?eg-1 eyes)
                       (direction ?eg-1 adressee)
                       (meets ?h-1 ?lm-1)
                       (meets ?h-2 ?lm-2)
                       (equal ?h-1 ?h-2)
                       (equal ?lm-1 ?lm-2)
                       (overlaps ?h-1 ?mp-1)
                       (overlaps ?mp-1 ?lm-1)
                       (overlaps ?h-1 ?eg-1)
                       (overlaps ?eg-1 ?lm-1))))))

(def-fcg-cxn child-cxn
             ((?child-unit
               (referent ?r)
               (ontological-class person)
               (sem-cat concept)
               (syn-cat noun)
               (boundaries ((right-manual-boundary ?lm-4)
                            (left-manual-boundary ?h-5))))
              <-
              (?child-unit
               (hash meaning ((child ?r)))
               --
               (hash form
                     ((hold ?h-5)
                      (articulator ?h-5 strong-hand)
                      (handconfiguration ?h-5 ?hc-4)
                      (shape ?hc-4 pinchall)
                      (placement ?h-5 ?pl-4)
                      (bearing ?pl-4 ?b-4)
                      (focal-site ?b-4 sternum-right)
                      (x-direction ?b-4 ahead)
                      (y-direction ?b-4 level)
                      (z-distance ?pl-4 medial)
                      (orientation ?h-5 ?or-4)
                      (reference-bearing ?or-4 ?b-4)
                      (x-direction ?or-4 ahead)
                      (y-direction ?or-4 level)
                      (local-movement ?lm-4)
                      (articulator ?lm-4 strong-hand)
                      (handconfiguration ?lm-4 ?hc-4)
                      (placement ?lm-4 ?pl-4)
                      (orientation ?lm-4 ?or-4)
                      (initial-movement ?lm-4 down-left)
                      (end-movement ?lm-4 up-right)
                      (modifier ?lm-4 repeat-several)
                      (mouthpicture ?mp-2)
                      (articulator ?mp-2 mouth)
                      (picture ?mp-2 "child")
                      (eyegaze ?eg-3)
                      (articulator ?eg-3 eyes)
                      (direction ?eg-3 adressee)
                      (meets ?h-5 ?lm-4)
                      (overlaps ?h-5 ?mp-2)
                      (overlaps ?mp-2 ?lm-4)
                      (overlaps ?h-5 ?eg-3)
                      (overlaps ?eg-3 ?lm-4))))))

(def-fcg-cxn vehicle-classifier-cxn
             ((?identified-vehicle-unit
               (ontological-class two-track-vehicle)
               (classifier-configurations ((handshape flat)
                                           (articulator ?hand)
                                           (handconfiguration ?hc-3)
                                           (x-direction ahead)
                                           (y-direction level)
                                           (placement-bearing ?b-3) 
                                           (movement-origin ?pl-2)
                                           (movement-end-point ?pl-3)))
               (subunits (?vehicle-unit ?classifier-unit))
               (boundaries ((left-manual-boundary ?h-5)
                            (right-manual-boundary ?h-4)))
               (syn-cat identified-classifier))
              (?classifier-unit
               (boundaries ((left-manual-boundary ?h-3)
                            (right-manual-boundary ?h-4)))
               (syn-cat classifier))
              <-
              (?identified-vehicle-unit
               (referent ?r)
               --
               )
              (?classifier-unit
               (referent ?r)
               --
               (hash form ((hold ?h-3)
                           (articulator ?h-3 ?hand)
                           (handconfiguration ?h-3 ?hc-3)
                           (placement ?h-3 ?pl-2)
                           (orientation ?h-3 ?or-3)
                           (shape ?hc-3 flat)
                           (path-movement ?pm-3)
                           (articulator ?pm-3 ?hand)
                           (handconfiguration ?pm-3 ?hc-3)
                           (orientation ?pm-3 ?or-3)
                           (reference-bearing ?or-3 ?b-3)
                           (x-direction ?or-3 ahead)
                           (y-direction ?or-3 level)
                           (path ?pm-3 ?p-2)
                           (origin ?p-2 ?pl-2)
                           (end-point ?p-2 ?pl-3)
                           (hold ?h-4)
                           (articulator ?h-4 weak-hand)
                           (handconfiguration ?h-4 ?hc-3)
                           (placement ?h-4 ?pl-3)
                           (orientation ?h-4 ?or-3)
                           (eyegaze ?eg-2)
                           (articulator ?eg-2 eyes)
                           (direction ?eg-2 ?pl-3)
                           (meets ?h-3 ?pm-3)
                           (meets ?pm-3 ?h-4)
                           (overlaps ?h-3 ?eg-2)
                           (overlaps ?eg-2 ?h-4)
                           (meets ?m-2 ?h-3))))
              (?vehicle-unit
               (ontological-class two-track-vehicle)
               (referent ?r)
               (sem-cat concept)
               --
               (boundaries ((left-manual-boundary ?h-5)
                            (right-manual-boundary ?m-2)))
               (syn-cat noun))))

(def-fcg-cxn person-classifier-cxn
             ((?identified-person-unit
               (classifier-configurations ((handconfiguration ?hc-6)
                                           (articulator ?hand)
                                           (x-direction ahead)
                                           (y-direction up)
                                           (placement-bearing ?b-1) 
                                           (movement-origin ?pl-7)
                                           (movement-end-point ?pl-5)))
               (syn-cat identified-classifier)
               (subunits (?person-unit ?classifier-unit))
               (boundaries ((left-manual-boundary ?h-5)
                            (right-manual-boundary ?h-7))))
              (?classifier-unit
               (boundaries ((left-manual-boundary ?h-6)
                            (right-manual-boundary ?h-7)))
               (syn-cat classifier))
              <-
              (?identified-person-unit
               (referent ?r)
               --
               )
              (?classifier-unit
               (referent ?r)
               --
               (hash form ((hold ?h-6)
                           (articulator ?h-6 ?hand)
                           (handconfiguration ?h-6 ?hc-6)
                           (placement ?h-6 ?pl-7)
                           (orientation ?h-6 ?or-5)
                           (shape ?hc-6 finger2)
                           (reference-bearing ?or-5 ?b-1)
                           (x-direction ?or-5 ahead)
                           (y-direction ?or-5 up)
                           (path-movement ?pm-5)
                           (articulator ?pm-5 ?hand)
                           (handconfiguration ?pm-5 ?hc-5)
                           (orientation ?pm-5 ?or-5)
                           (path ?pm-5 ?p-3)
                           (initial-placement ?p-3 ?pl-7)
                           (end-placement ?p-3 ?pl-5)
                           (hold ?h-7)
                           (articulator ?h-7 strong-hand)
                           (handconfiguration ?h-7 ?hc-6)
                           (orientation ?h-7 ?or-5)
                           (placement ?h-7 ?pl-5)
                           (eyegaze ?eg-4)
                           (articulator ?eg-4 eyes)
                           (direction ?eg-4 ?pl-5)
                           (meets ?h-6 ?pm-5)
                           (meets ?pm-5 ?h-7)
                           (overlaps ?h-6 ?eg-4)
                           (overlaps ?eg-4 ?h-7)
                           (meets ?m-2 ?h-6 ))))
              (?person-unit
               (ontological-class person)
               (sem-cat concept)
               (referent ?r)
               --
               (boundaries ((right-manual-boundary ?m-2)
                            (left-manual-boundary ?h-5)))
               (syn-cat noun)
               )))

(def-fcg-cxn x-approaches-y-cxn
             ((?x-unit
               (referent ?x))
              (?y-unit
               (referent ?y))
              (?approaches-unit
               (subunits (?x-unit ?y-unit)))
              (?x-classifier-unit
               (referent ?x))
              (?y-classifier-unit
               (referent ?y))
              <-
              (?x-unit
               --
               (classifier-configurations ((handconfiguration ?hc-6)
                                           (articulator strong-hand)
                                           (x-direction ?x-dir)
                                           (y-direction ?y-dir)
                                           (placement-bearing ?b-2)
                                           (movement-origin ?pl-1)
                                           (movement-end-point ?pl-2)))
               (syn-cat identified-classifier)
               (subunits (?x-noun-unit ?x-classifier-unit))
               (boundaries ((left-manual-boundary ?h-11)
                            (right-manual-boundary ?h-10)))
               (hash form
                     ((bearing ?pl-1 ?b-1)
                      (focal-site ?b-1 right-sternum)
                      (x-direction ?b-1 ahead)
                      (y-direction ?b-1 level)
                      (z-distance ?pl-1 medial)
                      (bearing ?pl-2 ?b-2)
                      (focal-site ?b-2 ?pl-1)
                      (x-direction ?b-2 leftward)
                      (y-direction ?b-2 downward)
                      (z-distance ?pl-2 medial))))
              (?x-classifier-unit
               --
               (boundaries ((left-manual-boundary ?h-12)))
               (syn-cat classifier))
              (?approaches-unit
               (hash meaning
                     ((approach.01 ?a)
                      (arg1 ?a ?x)
                      (arg2 ?a ?y)))
               --
               (hash form
                     ((path-movement ?pm-6)
                      (articulator ?pm-6 strong-hand)
                      (handconfiguration ?pm-6 ?hc-6)
                      (orientation ?pm-6 ?or-6)
                      (path ?pm-6 ?p-1)
                      (origin ?p-1 ?pl-2)
                      (end-point ?p-1 ?pl-4)
                      (reference-bearing ?or-6 ?p-1)
                      (x-direction ?or-6 ?x-dir)
                      (y-direction ?or-6 ?y-dir)
                      (hold ?h-8)
                      (handconfiguration ?h-8 ?hc-6)
                      (placement ?h-8 ?pl-4)
                      (orientation ?h-8 ?or-6)
                      (eyegaze ?eg-5)
                      (articulator ?eg-5 eyes)
                      (direction ?eg-5 strong-hand)
                      (meets ?pm-6 ?h-8)
                      (overlaps ?pm-6 ?eg-5)
                      (overlaps ?eg-5 ?h-8)
                      (finishes ?h-8 ?h-9)
                      (meets ?h-10 ?pm-6)
                      (during ?h-11 ?h-9)
                      (during ?h-12 ?h-9))))
              (?y-unit
               --
               (classifier-configurations ((articulator weak-hand)
                                           (movement-origin ?pl-3)
                                           (placement-bearing ?b-4)
                                           (movement-end-point ?pl-4)))
               (boundaries ((right-manual-boundary ?h-9)))
               (syn-cat identified-classifier)
               (hash form
                     ((bearing ?pl-3 ?b-3)
                      (x-direction ?b-3 ahead)
                      (y-direction ?b-3 level)
                      (z-distance ?pl-3 medial)
                      (focal-site ?b-3 left-chest)
                      (bearing ?pl-4 ?b-4)
                      (focal-site ?b-4 ?pl-3)
                      (x-direction ?b-4 rightward)
                      (y-direction ?b-4 downward)
                      (z-distance ?pl-4 medial)))
               (subunits (?y-noun-unit ?y-classifier-unit)))
              (?y-classifier-unit
               --
               (syn-cat classifier))
             ))

              
                      
