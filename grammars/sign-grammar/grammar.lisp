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

;; A classifier is placed on the left (first a noun is signed, followed by a classifier with the same referent that is placed
;; left in the signing space by a downwards movement and an eye-gaze). The form of the classifier is defined by the lexical sign for the noun



(def-fcg-cxn CL-left-placement
             ((?placement-unit
               (placement left))
              (?classifier-unit
               (placement left)
               (handconfiguration ((handedness one-handed))))
              <-
              (?placement-unit
               (sem-cat ref-expression)
               --
               (syn-cat NP)
               (subunits (?noun-unit ?classifier-unit)))
              (?classifier-unit
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (orientation-boundary ?orif-1)
               (form ((SIGN ?SIGN-2)
                      (MANUAL ?SIGN-2 ?M-1)))
               (hash form
                      ((NON-MANUAL ?SIGN-2 ?NM-1)
                       (EYEGAZE ?NM-1 "LD")
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMLRAT ?LOC-1 ?LOCF-1)
                       (HAMCHEST ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2))))))

;; The same construction as before, but for the right location in signing space

(def-fcg-cxn CL-right-placement-cxn
             ((?placement-unit
               (placement right))
              (?classifier-unit
               (placement right)
               (handconfiguration ((handedness one-handed))))
               <-
               (?placement-unit
                (sem-cat ref-expression)
                --
                (syn-cat NP)
                (subunits (?noun-unit ?classifier-unit)))
               (?classifier-unit
                (sem-cat ref-expression)
                --
                (syn-cat classifier)
                (orientation-boundary ?orif-1)
                (form ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)))
                (hash form
                      ((NON-MANUAL ?SIGN-2 ?NM-1)
                       (EYEGAZE ?NM-1 "RD")
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMCHEST ?LOC-1 ?LOCF-1)
                       (HAMLRAT ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)))
                ))
               )

;; A classifier is held left in the signing space (it is the continuation of a placement classifier).


(def-fcg-cxn CL-left-hold-cxn
             ((?hold-unit
               (placement left))
              <-
              (?hold-unit
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (orientation-boundary ?orif-1)
               (form ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)))
                (hash form
                      ((LOCATION ?M-1 ?LOC-1)
                       (HAMLRAT ?LOC-1 ?LOCF-1)
                       (HAMCHEST ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2))))
              (?classifier-unit
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (placement left)
               (subunits (?hold-unit)))))

(def-fcg-cxn CL-left-hold-2-cxn
             ((?hold-unit
               (placement left))
              <-
              (?hold-unit
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (orientation-boundary ?orif-1)
               (form ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)))
                (hash form
                      ((LOCATION ?M-1 ?LOC-1)
                       (HAMLRAT ?LOC-1 ?LOCF-1)
                       (HAMCHEST ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2))))
              (?classifier-unit
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (placement left)
               (subunits (?hold-unit)))))
               

; same construction for right location in signing space

(def-fcg-cxn CL-right-hold-cxn
              ((?hold-unit
               (placement right))
               <-
               (?hold-unit
                (sem-cat ref-expression)
                --
                (syn-cat classifier)
                (orientation-boundary ?orif-1)
                (form ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)))
                (hash form
                      ((LOCATION ?M-1 ?LOC-1)
                       (HAMCHEST ?LOC-1 ?LOCF-1)
                       (HAMLRAT ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)))
                
                )
               (?classifier-unit
                (sem-cat ref-expression)
                --
                (syn-cat classifier)
                (placement right)
                (subunits (?hold-unit)))))
              

(def-fcg-cxn CL-hamflathand-placement-cxn
             ((?placement-unit
               (subunits (?noun-unit ?classifier-unit))
               (referent ?r)
               (sem-cat ref-expression)
               (syn-cat NP))
              (?classifier-unit
               (referent ?r)
               (sem-cat ref-expression)
               (orientation-boundary ?orif-1)
               (movement-boundary ?movf-1)
               (syn-cat classifier)
               (handconfiguration ((handshape hamflathand)
                                  (extended-finger-direction hamextfingero)
                                  (palm-orientation hampalmd))))
              
              <-
              (?noun-unit
                (referent ?r)
                (sem-cat ref-expression)
                (sem-class two-track-vehicle)
                --
                (CL-form hamflathand)
                (syn-cat noun)
                (form ((SIGN ?SIGN-1))))
              (?classifier-unit
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
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?SIGN-1 ?SIGN-2)
                      )
                ))))
               

(def-fcg-cxn CL-hamflathand-hold-cxn
             ((?hold-unit
               (handconfiguration ((handshape hamflathand)
                                   (extended-finger-direction hamextfingero)
                                   (palm-orientation hampalmd)
                                   (handedness one-handed)))
               (referent ?r)
               (syn-cat classifier)
               (sem-cat ref-expression)
               (orientation-boundary ?orif-1))
              (?classifier-unit
               (subunits (?hold-unit)))
              
              <-
              (?classifier-unit
               (referent ?r)
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (handconfiguration ((handshape hamflathand)
                                   (extended-finger-direction hamextfingero)
                                   (palm-orientation hampalmd)))
               (form ((SIGN ?SIGN-1))))
              (?hold-unit
               --
               (hash form
                     ((SIGN ?SIGN-3)
                      (MANUAL ?SIGN-3 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFLATHAND ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERO ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMD ?ORI-1 ?ORIF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (COINCIDES ?SIGN-2 ?SIGN-3)
                      (HAND ?SIGN-3 "weak"))))
              (?dominant-hand-unit
               (sem-cat ref-expression)
               --
               (form ((SIGN ?SIGN-2)))
               (handconfiguration ((handedness one-handed)))
               (hash form
                     (
                      (HAND ?SIGN-2 "strong"))))))


(def-fcg-cxn CL-hamfinger2-placement-cxn
             ((?placement-unit
               (subunits (?noun-unit ?classifier-unit))
               (referent ?r)
               (sem-cat ref-expression)
               (syn-cat NP))
              (?classifier-unit
               (referent ?r)
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (sem-cat ref-expression)
               (orientation-boundary ?orif-1)
               (movement-boundary ?movf-1)
               (syn-cat classifier))
              
              <-
              (?noun-unit
                (referent ?r)
                (sem-cat ref-expression)
                (sem-class person)
                --
                (CL-form hamfinger2)
                (syn-cat noun)
                (form ((SIGN ?SIGN-1))))
              (?classifier-unit
                --
                (hash form
                      ((SIGN ?SIGN-2)
                       (MANUAL ?SIGN-2 ?M-1)
                       (HANDSHAPE ?M-1 ?HS-1)
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


(def-fcg-cxn CL-hamfinger2-hold-cxn
             ((?hold-unit
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (referent ?r)
               (orientation-boundary ?orif-1))
              <-
              (?classifier-unit
               (referent ?r)
               --
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml)))
               (form ((SIGN ?SIGN-3))))
              (?hold-unit
               --
               (hash form
                     ((SIGN ?SIGN-1)
                      (MANUAL ?SIGN-1 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFINGER2 ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALML ?ORI-1 ?ORIF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (COINCIDES ?SIGN-1 ?SIGN-2)
                      (HAND ?SIGN-1 weak))))
              (?dominant-hand-unit
               (sem-cat ref-expression)
               --
               (form ((SIGN ?SIGN-2)))
               (handconfiguration ((handedness one-handed)))
               (hash form
                     ((MEETS ?SIGN-3 ?SIGN-2)
                      (HAND ?SIGN-2 strong))))))

;; The classifier-verb construction for movement
;; in this case the movement is performed from left to right, so the referent that was placed on the left of the signing space
;; is the arg-0 of the movement and the referent that was placed on the right is the arg-2

(def-fcg-cxn CL-hamfinger2-movement-cxn
             ((?movement-unit
               (sem-cat ref-expression)
               (syn-cat classifier)
               (referent ?r)
               (orientation-boundary ?orif-1)
               (handconfiguration ((handedness one-handed))))
              (?classifier-unit
               (subunits (?movement-unit)))
              <-
              (?classifier-unit
               (sem-cat ref-expression)
               (referent ?r)
               --
               (syn-cat classifier)
               (handconfiguration ((handshape hamfinger2)
                                   (extended-finger-direction hamextfingeru)
                                   (palm-orientation hampalml))))
              (?movement-unit
               (hash form
                     ((SIGN ?SIGN-1)
                      (MANUAL ?SIGN-1 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFINGER2 ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALML ?ORI-1 ?ORIF-1)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1))))))

(def-fcg-cxn CL-rl-movement-cxn
             ((?movement-unit
               (syn-cat clause)
               (sem-cat ref-expression)
               (subunits (?ground-unit ?figure-unit)))
              <-
              (?figure-unit
               (referent ?f)
               (sem-cat ref-expression)
               --
               (orientation-boundary ?orif-1)
               (syn-cat classifier)
               (placement right)
               (form ((sign ?sign-1)
                      (manual ?sign-1 ?m-1)
                      (movement ?m-1 ?mov-1)))
               (hash form
                     ((hammovel ?mov-1 ?movf-1))))
              (?ground-unit
               (referent ?g)
               (sem-cat ref-expression)
               --
               (syn-cat classifier)
               (placement left))
                     
              (?movement-unit
               (hash meaning
                     ((move.01 ?m)
                      (arg0 ?m ?f)
                      (arg2 ?m ?g)))
               --
               )))

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

