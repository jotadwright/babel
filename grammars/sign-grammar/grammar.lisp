(in-package :sign-grammar)


;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;
; Make sure the de-render-mode is :de-render-signs 
;(configure-grammar )
(def-fcg-constructions sign-grammar
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
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
                       (:max-number-of-nodes . 500)
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
                                 (:hierarchy-features subunits)
                                 (:selected-hierarchy . subunits)
                                 (:select-subfeatures . nil)))



;;-----------------------;;
;; Lexical Constructions ;;
;;-----------------------;;

;; ball-cxn
;; meaning = (ball b)
;; form = the hamnosys form predicates for ball

(def-fcg-cxn ball-cxn 
             ((?ball-unit 
               (referent ?b)
               (sem-cat ref-expression)
               (syn-cat noun)
               (salient-movf ?movf-2)
               (CL-form hamfist))
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

;; ball-cxn
;; meaning = (car c)
;; form = the hamnosys form predicates for car

(DEF-FCG-CXN CAR-CXN
             ((?CAR-UNIT
               (REFERENT ?R)
               (SEM-CAT ref-expression)
               (sem-class two-track-vehicle)
               (SYN-CAT NOUN)
               (CL-form hamflathand)
               (handconfiguration ((handedness two-handed))))
              <-
              (?CAR-UNIT
               (HASH MEANING ((CAR ?R)))
               --
               (HASH FORM
                     ((SIGN ?SIGN-1)
                      (NON-MANUAL ?SIGN-1 ?NM-1)
                      (MOUTHPICTURE ?NM-1 "car")
                      (MANUAL ?SIGN-1 ?M-1)
                      (SYMMETRY ?M-1 ?SYM-1)
                      (HAMSYMMLR ?SYM-1 ?SYMF-1)
                      (HAMMOD ?SYMF-1 HAMFINGERSTRAIGHTMOD)
                      (HAMMOD ?SYMF-1 HAMLARGEMOD)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFIST ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERUO ?EXT-1 ?EXTF-1)
                      (HAMEXTFINGEROL ?EXT-1 ?EXTF-2)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMUL ?ORI-1 ?ORIF-1)
                      (LOCATION ?M-1 ?LOC-1)
                      (HAMSHOULDERS ?LOC-1 ?LOCF-1)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMSEQBEGIN ?MOV-1 ?MOVF-1)
                      (HAMMOVED ?MOV-1 ?MOVF-2)
                      (HAMMOD ?MOVF-2 HAMSMALLMOD)
                      (HAMMOVEU ?MOV-1 ?MOVF-3)
                      (HAMMOD ?MOVF-3 HAMSMALLMOD)
                      (HAMSEQEND ?MOV-1 ?MOVF-4)
                      (HAMREPEATFROMSTART ?MOV-1 ?MOVF-5)
                      (MEETS ?SYMF-1 ?HSF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?EXTF-2)
                      (MEETS ?EXTF-2 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?MOVF-1)
                      (MEETS ?MOVF-1 ?MOVF-2)
                      (MEETS ?MOVF-2 ?MOVF-3)
                      (MEETS ?MOVF-3 ?MOVF-4)
                      (MEETS ?MOVF-4 ?MOVF-5))))))

;; ball-cxn
;; meaning = (child b)
;; form = the hamnosys form predicates for child

(DEF-FCG-CXN CHILD-CXN
             ((?CHILD-UNIT (REFERENT ?R)
                           (SEM-CAT ref-expression)
                           (sem-class person)
                           (SYN-CAT NOUN)
                           (CL-form hamfinger2)
                           (handconfiguration ((handedness one-handed))))
              <-
              (?CHILD-UNIT
               (HASH MEANING ((CHILD ?R)))
               --
               (HASH FORM
                     ((SIGN ?SIGN-1)
                      (NON-MANUAL ?SIGN-1 ?NM-1)
                      (MOUTHPICTURE ?NM-1 "child")
                      (MANUAL ?SIGN-1 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMPINCHALL ?HS-1 ?HSF-1)
                      (HAMMOD ?HSF-1 HAMFINGERSTRAIGHTMOD)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERUO ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMU ?ORI-1 ?ORIF-1)
                      (LOCATION ?M-1 ?LOC-1)
                      (HAMCHEST ?LOC-1 ?LOCF-1)
                      (HAMLRAT ?LOC-1 ?LOCF-2)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (HAMMOD ?MOVF-1 HAMSMALLMOD)
                      (HAMREPEATFROMSTART ?MOV-1 ?MOVF-2)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?LOCF-2)
                      (MEETS ?LOCF-2 ?MOVF-1)
                      (MEETS ?MOVF-1 ?MOVF-2))))))

;;--------------------------;;
;; Classifier constructions ;;
;;--------------------------;;

;; CL-left-placement-cxn
;; meaning: referent of the noun in the placement in the arg-2 of an unknown relation
;; form: classifier left in the signing-space (on dominant hand)

(def-fcg-cxn left-placement-cxn
             ((?placement-unit
               (sem-cat ref-expression)
               (syn-cat NP)
               (referent ?r)
               (placement left)
               (subunits (?classifier-unit)))
              (?classifier-unit
               (sem-cat ref-expression)
               (referent ?r)
               (syn-cat classifier)
               (placement left)
               (handshape-boundary ?hsf-1)
               (orientation-boundary ?orif-1)
               (handconfiguration ((handedness one-handed))))
              <-
              (?placement-unit
               (hash meaning ((arg2 ?x ?r)))
               --
               )
              (?classifier-unit
               --
               (hash form
                      ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)
                       (NON-MANUAL ?SIGN-2 ?NM-1)
                       (EYEGAZE ?NM-1 "LD")
                       (SYMMETRY ?M-1 ?SYM-1)
                       (HAMNONDOMINANT ?SYM-1 ?SYMF-1)
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMLRAT ?LOC-1 ?LOCF-1)
                       (HAMCHEST ?LOC-1 ?LOCF-2)
                       (MEETS ?SYMF-1 ?HSF-1)
                       (MEETS ?ORIF-2 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)
                       (MEETS ?LOCF-2 ?MOVF-1))))))

;; CL-left-placement-cxn
;; meaning: referent of the noun in the placement in the arg-1 of an unknown relation
;; form: classifier left in the signing space (on non-dominant hand)

;; Put the holds in here as well????

(def-fcg-cxn right-placement-cxn
             ((?placement-unit
               (sem-cat ref-expression)
               (syn-cat NP)
               (referent ?r)
               (placement right)
               (subunits (?classifier-unit)))
              (?classifier-unit
               (sem-cat ref-expression)
               (referent ?r)
               (syn-cat classifier)
               (placement right)
               (handshape-boundary ?orif-1)
               (handconfiguration ((handedness one-handed))))
               <-
               (?placement-unit
                (hash meaning ((arg1 ?x ?r)))
                --
                )
               (?classifier-unit
                --
                (hash form
                      ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)
                       (NON-MANUAL ?SIGN-2 ?NM-1)
                       (EYEGAZE ?NM-1 "RD")
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMCHEST ?LOC-1 ?LOCF-1)
                       (HAMLRAT ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)
                       (MEETS ?LOCF-2 ?MOVF-1)))
                ))
               )

(def-fcg-cxn vehicle-hold-cxn
             ((?hold-unit-1
               (subunits (?hold-unit-2)))
              (?hold-unit-2
               (subunits (?hold-unit-3)))
              (?classifier-unit
               (subunits (?hold-unit-1)))
              (?placement-unit-1
               (footprints (complete)))
              <-
              (?placement-unit-1
               (sem-cat ref-expression)
               (referent ?r)
               (placement left)
               (subunits (?classifier-unit))
               (footprints (NOT complete))
               --
               (syn-cat np))
              (?classifier-unit
               (referent ?r)
               (sem-cat ref-expression)
               
               (placement left)
               (sem-class two-track-vehicle)
               --
               (syn-cat classifier)
               (handconfiguration ((handshape hamflathand)
                                   (extended-finger-direction hamextfingero)
                                   (palm-orientation hampalmd)))
               (form ((SIGN ?SIGN-1))))
              (?placement-unit-2
               (sem-cat ref-expression)
               (placement right)
               (subunits (?dominant-unit-1 ?dominant-unit-2))
               --
               (syn-cat np))
              (?dominant-unit-1
               (sem-cat ref-expression)
               --
               (handconfiguration ((handedness one-handed)))
               (syn-cat noun)
               (form ((SIGN ?SIGN-5)))
               (hash form
                     ((HAND ?SIGN-5 "strong")
                      (MEETS ?SIGN-1 ?SIGN-5))))
              (?hold-unit-1
               --
               (hash form
                     ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFLATHAND ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERO ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMD ?ORI-1 ?ORIF-1)
                      (LOCATION ?M-1 ?LOC-1)
                      (HAMLRAT ?LOC-1 ?LOCF-1)
                      (HAMCHEST ?LOC-1 ?LOCF-2)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?LOCF-2)
                      (COINCIDES ?SIGN-5 ?SIGN-2)
                      (HAND ?SIGN-2 "weak"))))
              (?dominant-unit-2
               (sem-cat ref-expression)
               (subunits (?dominant-unit-3))
               --
               (handconfiguration ((handedness one-handed)))
               (form ((SIGN ?SIGN-6)
                      (MEETS ?SIGN-5 ?SIGN-6)))
               (hash form
                     ((HAND ?SIGN-6 "strong"))))
              (?hold-unit-2
               --
               (hash form
                     ((SIGN ?SIGN-3)
                      (MANUAL ?SIGN-3 ?M-2)
                      (HANDSHAPE ?M-2 ?HS-2)
                      (HAMFLATHAND ?HS-2 ?HSF-2)
                      (EXTENDED-FINGER-DIRECTION ?M-2 ?EXT-2)
                      (HAMEXTFINGERO ?EXT-2 ?EXTF-2)
                      (PALM-ORIENTATION ?M-2 ?ORI-2)
                      (HAMPALMD ?ORI-2 ?ORIF-2)
                      (MEETS ?HSF-2 ?EXTF-2)
                      (MEETS ?EXTF-2 ?ORIF-2)
                      (LOCATION ?M-2 ?LOC-2)
                      (HAMLRAT ?LOC-2 ?LOCF-3)
                      (HAMCHEST ?LOC-2 ?LOCF-4)
                      (MEETS ?ORIF-2 ?LOCF-3)
                      (MEETS ?LOCF-3 ?LOCF-4)
                      (COINCIDES ?SIGN-6 ?SIGN-3)
                      (HAND ?SIGN-3 "weak"))))
              (?dominant-unit-3
               (sem-cat ref-expression)
               --
               (handconfiguration ((handedness one-handed)))
               (form ((SIGN ?SIGN-7)
                      (MEETS ?SIGN-6 ?SIGN-7)))
               (hash form
                     ((HAND ?SIGN-7 "strong"))))
              (?hold-unit-3
               --
               (hash form
                     ((SIGN ?SIGN-4)
                      (MANUAL ?SIGN-4 ?M-3)
                      (HANDSHAPE ?M-3 ?HS-3)
                      (HAMFLATHAND ?HS-3 ?HSF-3)
                      (EXTENDED-FINGER-DIRECTION ?M-3 ?EXT-3)
                      (HAMEXTFINGERO ?EXT-3 ?EXTF-3)
                      (PALM-ORIENTATION ?M-3 ?ORI-3)
                      (HAMPALMD ?ORI-3 ?ORIF-3)
                      (MEETS ?HSF-3 ?EXTF-3)
                      (MEETS ?EXTF-3 ?ORIF-3)
                      (LOCATION ?M-3 ?LOC-3)
                      (HAMLRAT ?LOC-3 ?LOCF-5)
                      (HAMCHEST ?LOC-3 ?LOCF-6)
                      (MEETS ?ORIF-3 ?LOCF-5)
                      (MEETS ?LOCF-5 ?LOCF-6)
                      (COINCIDES ?SIGN-7 ?SIGN-4)
                      (HAND ?SIGN-4 "weak"))))))

(def-fcg-cxn person-hold-cxn
             ((?hold-unit-1
               (subunits (?hold-unit-2)))
              (?hold-unit-2
               (subunits (?hold-unit-3)))
              (?classifier-unit
               (subunits (?hold-unit-1))
               (footprints (complete)))
              <-
              (?classifier-unit
               (referent ?r)
               (sem-cat ref-expression)
               (placement left)
               (sem-class person)
               (footprints (NOT complete))
               --
               (syn-cat classifier)
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (form ((SIGN ?SIGN-1))))
              (?dominant-unit-1
               (sem-cat ref-expression)
               --
               (handconfiguration ((handedness one-handed)))
               (form ((SIGN ?SIGN-5)))
               (hash form
                     ((HAND ?SIGN-5 "strong")
                      (MEETS ?SIGN-1 ?SIGN-5))))
              (?hold-unit-1
               --
               (hash form
                     ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFINGER2 ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALML ?ORI-1 ?ORIF-1)
                      (LOCATION ?M-1 ?LOC-1)
                      (HAMLRAT ?LOC-1 ?LOCF-1)
                      (HAMCHEST ?LOC-1 ?LOCF-2)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?LOCF-2)
                      (COINCIDES ?SIGN-5 ?SIGN-2)
                      (HAND ?SIGN-2 "weak"))))
              (?dominant-unit-2
               (sem-cat ref-expression)
               --
               (handconfiguration ((handedness one-handed)))
               (form ((SIGN ?SIGN-6)
                      (MEETS ?SIGN-5 ?SIGN-6)))
               (hash form
                     ((HAND ?SIGN-6 "strong"))))
              (?hold-unit-2
               --
               (hash form
                     ((SIGN ?SIGN-3)
                      (MANUAL ?SIGN-3 ?M-2)
                      (HANDSHAPE ?M-2 ?HS-2)
                      (HAMFINGER2 ?HS-2 ?HSF-2)
                      (EXTENDED-FINGER-DIRECTION ?M-2 ?EXT-2)
                      (HAMEXTFINGERU ?EXT-2 ?EXTF-2)
                      (PALM-ORIENTATION ?M-2 ?ORI-2)
                      (HAMPALML ?ORI-2 ?ORIF-2)
                      (MEETS ?HSF-2 ?EXTF-2)
                      (MEETS ?EXTF-2 ?ORIF-2)
                      (LOCATION ?M-2 ?LOC-2)
                      (HAMLRAT ?LOC-2 ?LOCF-3)
                      (HAMCHEST ?LOC-2 ?LOCF-4)
                      (MEETS ?ORIF-2 ?LOCF-3)
                      (MEETS ?LOCF-3 ?LOCF-4)
                      (COINCIDES ?SIGN-6 ?SIGN-3)
                      (HAND ?SIGN-3 "weak"))))
              (?dominant-unit-3
               (sem-cat ref-expression)
               --
               (handconfiguration ((handedness one-handed)))
               (form ((SIGN ?SIGN-7)
                      (MEETS ?SIGN-6 ?SIGN-7)))
               (hash form
                     ((HAND ?SIGN-7 "strong")
                      )))
              (?hold-unit-3
               --
               (hash form
                     ((SIGN ?SIGN-4)
                      (MANUAL ?SIGN-4 ?M-3)
                      (HANDSHAPE ?M-3 ?HS-3)
                      (HAMFINGER2 ?HS-3 ?HSF-3)
                      (EXTENDED-FINGER-DIRECTION ?M-3 ?EXT-3)
                      (HAMEXTFINGERU ?EXT-3 ?EXTF-3)
                      (PALM-ORIENTATION ?M-3 ?ORI-3)
                      (HAMPALML ?ORI-3 ?ORIF-3)
                      (MEETS ?HSF-3 ?EXTF-3)
                      (MEETS ?EXTF-3 ?ORIF-3)
                      (LOCATION ?M-3 ?LOC-3)
                      (HAMLRAT ?LOC-3 ?LOCF-5)
                      (HAMCHEST ?LOC-3 ?LOCF-6)
                      (MEETS ?ORIF-3 ?LOCF-5)
                      (MEETS ?LOCF-5 ?LOCF-6)
                      (COINCIDES ?SIGN-7 ?SIGN-4)
                      (HAND ?SIGN-4 "weak"))))))
            
; hamflathand placement
; meaning: there is a referent that is a vehicle
; form: a classifier follows a noun. The noun refers to a person

(def-fcg-cxn vehicle-classifier-cxn
             ((?placement-unit
               (subunits (?classifier-unit ?noun-unit))
               (referent ?r)
               (sem-class two-track-vehicle)
               (boundaries ((leftmost-boundary ?sign-1)
                            (rightmost-boundary ?sign-2)))
               (footprints (class-identified)))
              (?classifier-unit
               (orientation-boundary ?orif-1)
               (handshape-boundary ?hsf-1)
               (sem-class two-track-vehicle)
               (handconfiguration ((handshape hamflathand)
                                  (extended-finger-direction hamextfingero)
                                  (palm-orientation hampalmd))))
              
              <-
              (?placement-unit
               (sem-cat ref-expression)
               (referent ?r)
               (footprints (NOT class-identified))
               --
               (syn-cat NP)
               (subunits (?classifier-unit)))
              (?noun-unit
                (referent ?r)
                (sem-cat ref-expression)
                (sem-class two-track-vehicle)
                --
                (syn-cat noun)
                (form ((SIGN ?SIGN-1))))
              (?classifier-unit
               (sem-cat ref-expression)
               (referent ?r)
               --
               (form ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)))
               (hash form
                     ((HANDSHAPE ?M-1 ?HS-1)
                      (HAMFLATHAND ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERO ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMD ?ORI-1 ?ORIF-1)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?SIGN-1 ?SIGN-2)
                      )
                     ))))

(def-fcg-cxn person-classifier-cxn
             ((?placement-unit
               (subunits (?classifier-unit ?noun-unit ))
               (referent ?r)
               (sem-class person)
               (boundaries ((leftmost-boundary ?sign-1)
                            (rightmost-boundary ?sign-2)))
               (footprints (class-identified)))
              (?classifier-unit
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (orientation-boundary ?orif-1)
               (sem-class person))
              
              <-
              (?placement-unit
               (sem-cat ref-expression)
               (referent ?r)
               (footprints (NOT class-identified))
               --
               (syn-cat NP)
               (subunits (?classifier-unit)))
              (?noun-unit
                (referent ?r)
                (sem-cat ref-expression)
                (sem-class person)
                --
                (syn-cat noun)
                (form ((SIGN ?SIGN-1))))
              (?classifier-unit
               (sem-cat ref-expression)
               (referent ?r)
                --
                (form ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)))
                (hash form
                      ((HANDSHAPE ?M-1 ?HS-1)
                       (HAMFINGER2 ?HS-1 ?HSF-1)
                       (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                       (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                       (PALM-ORIENTATION ?M-1 ?ORI-1)
                       (HAMPALMl ?ORI-1 ?ORIF-1)
                       (MOVEMENT ?M-1 ?MOV-1)
                       (HAMMOVED ?MOV-1 ?MOVF-1)
                       (MEETS ?HSF-1 ?EXTF-1)
                       (MEETS ?EXTF-1 ?ORIF-1)
                       (MEETS ?SIGN-1 ?SIGN-2))
                ))))

(def-fcg-cxn person-movement-cxn
             ((?movement-unit
               (sem-cat ref-expression)
               (syn-cat classifier)
               (referent ?r)
               (placement right)
               (handconfiguration ((handedness one-handed))))
              (?classifier-unit
               (subunits (?movement-unit))
               (footprints (complete)))
              <-
              (?classifier-unit
               (sem-cat ref-expression)
               (sem-class person)
               (referent ?r)
               (footprints (NOT complete))
               --
               (syn-cat classifier)
               (placement right)
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (form ((sign ?sign-1))))
              (?movement-unit
               --
               (hash form
                     ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFINGER2 ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALML ?ORI-1 ?ORIF-1)
                      (LOCATION ?M-1 ?LOC-1)
                      (HAMCHEST ?LOC-1 ?LOCF-1)
                      (HAMLRAT ?LOC-1 ?LOCF-2)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?LOCF-2)
                      (MEETS ?LOCF-2 ?MOVF-2)
                      (MEETS ?SIGN-1 ?SIGN-2))))))

(def-fcg-cxn approaches-cxn
             ((?approach-unit
               (phrase-type clause)
               (sem-cat ref-expression)
               (subunits (?agent-placement-unit ?destination-placement-unit)))
              <-
              (?agent-unit
               (referent ?f)
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (placement right)
               (form ((sign ?sign-1)
                      (manual ?sign-1 ?m-1)
                      (movement ?m-1 ?mov-1)
                      )))
              (?agent-placement-unit
               (referent ?f)
               (sem-cat ref-expression)
               (meaning ((arg1 ?a ?f)))
               --
               (syn-cat NP)
               (placement right))
              (?destination-unit
               (referent ?g)
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (placement left))
              (?destination-placement-unit
               (referent ?g)
               (sem-cat ref-expression)
               (meaning ((arg2 ?a ?g)))
               --
               (syn-cat NP)
               (placement left))   
              (?approach-unit
               (hash meaning
                     ((approach.01 ?a)))
               --
               (hash form
                     ((hammovel ?mov-1 ?movf-1)
                      (non-manual ?sign-1 ?nm-1)
                      (eyegaze ?nm-1 "HC"))))))


;;------------------------;;
;; Modifier constructions ;;
;;------------------------;;

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

; (add-lex-cxns *BSL-lexical-signs*)

