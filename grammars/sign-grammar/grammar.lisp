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
                                 (:hide-features footprints)
                                 (:select-subfeatures . nil)))


;;---------------;;
;; Constructions ;;
;;---------------;;

; Lexical construction for ball
(def-fcg-cxn ball-cxn 
             ((?ball-unit 
               (referent ?b)
               (sem-cat identifier)
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

;; lexical construction for car
(DEF-FCG-CXN CAR-CXN
             ((?CAR-UNIT
               (REFERENT ?R)
               (SEM-CAT IDENTIFIER)
               (SYN-CAT NOUN)
               (CL-form hamflathand))
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

; lexical construction for child
(DEF-FCG-CXN CHILD-CXN
             ((?CHILD-UNIT (REFERENT ?R)
                           (SEM-CAT IDENTIFIER)
                           (SYN-CAT NOUN)
                           (CL-form hamfinger2))
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
                      (HAMMOD ?LOCF-1 HAMLRAT)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (HAMMOD ?MOVF-1 HAMSMALLMOD)
                      (HAMREPEATFROMSTART ?MOV-1 ?MOVF-2)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1)
                      (MEETS ?ORIF-1 ?LOCF-1)
                      (MEETS ?LOCF-1 ?MOVF-1)
                      (MEETS ?MOVF-1 ?MOVF-2))))))

(def-fcg-cxn CL-hamflathand-cxn
             ((?classifier-unit
               (referent ?r)
               (orientation ?ORIF-1))
              <-
              (?noun-unit
               (referent ?r)
               (CL-form hamflathand)
               --
               )
              (?classifier-unit
               --
               (hash form
                     ((SIGN ?SIGN-1)
                      (MANUAL ?SIGN-1 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFLATHAND ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERO ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMD ?ORI-1 ?ORIF-1)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1))))))

(def-fcg-cxn CL-hamfinger2-cxn
             ((?classifier-unit
               (referent ?r)
               (orientation ?ORIF-1))
              <-
              (?noun-unit
               (referent ?r)
               (CL-form hamfinger2)
               --
               )
              (?classifier-unit
               --
               (hash form
                     ((SIGN ?SIGN-1)
                      (MANUAL ?SIGN-1 ?M-1)
                      (HANDSHAPE ?M-1 ?HS-1)
                      (HAMFINGER2 ?HS-1 ?HSF-1)
                      (EXTENDED-FINGER-DIRECTION ?M-1 ?EXT-1)
                      (HAMEXTFINGERU ?EXT-1 ?EXTF-1)
                      (PALM-ORIENTATION ?M-1 ?ORI-1)
                      (HAMPALMDL ?ORI-1 ?ORIF-1)
                      (MOVEMENT ?M-1 ?MOV-1)
                      (HAMMOVED ?MOV-1 ?MOVF-1)
                      (MEETS ?HSF-1 ?EXTF-1)
                      (MEETS ?EXTF-1 ?ORIF-1))))))



(def-fcg-cxn CL-left-cxn
             ((
               <-
               (?classifier-unit
                (form ((SIGN ?SIGN-1)
                       (MANUAL ?SIGN-1 ?M-1)
                       (MOVEMENT ?M-1 ?MOV-1)
                       (HAMMOVED ?MOV-1 ?MOVF-1)))
                (location left)
                (orientation ?ORIF-1)
                --
                (hash form
                      ((NON-MANUAL ?SIGN-1 ?NM-1)
                       (EYEGAZE ?NM-1 "LD")
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMLRAT ?LOC-1 ?LOCF-1)
                       (HAMCHEST ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)
                       (MEETS ?LOCF-2 ?MOVF-1)))))))



(def-fcg-cxn CL-right-cxn
             ((
               <-
               (?classifier-unit
                (form ((SIGN ?SIGN-1)
                       (MANUAL ?SIGN-1 ?M-1)
                       (MOVEMENT ?M-1 ?MOV-1)
                       (HAMMOVED ?MOV-1 ?MOVF-1)))
                (location right)
                (orientation ?ORIF-1)
                --
                (hash form
                      ((NON-MANUAL ?SIGN-1 ?NM-1)
                       (EYEGAZE ?NM-1 "RD")
                       (LOCATION ?M-1 ?LOC-1)
                       (HAMCHEST ?LOC-1 ?LOCF-1)
                       (HAMLRAT ?LOC-1 ?LOCF-2)
                       (MEETS ?ORIF-1 ?LOCF-1)
                       (MEETS ?LOCF-1 ?LOCF-2)
                       (MEETS ?LOCF-2 ?MOVF-1)))))))

(def-fcg-cxn non-dominant-hand-cxn
             ((
               <-
               (?sign-unit
                (form ((SIGN ?SIGN-1)
                       (MANUAL ?SIGN-1 ?M-1)))
                (hand weak)
                (handshape-boundary ?HSF-1)
                --
                (hash form
                      ((SYMMETRY ?M-1 ?SYM-1)
                       (HAMNONDOMINANT ?SYM-1 ?SYMF-1)
                       (MEETS ?SYMF-1 ?HSF-1)))))))

(def-fcg-cxn simultaneous-cxn
             ((
               <-
               (?strong-hand-unit
                (form (SIGN ?SIGN-1))
                --
                (hash form
                      ((HAND ?SIGN-1 "strong"))))
               (?weak-hand-unit
                (form (SIGN ?SIGN-2))
                --
                (hash form
                      ((HAND ?SIGN-2 "weak"))))
               (?simultaneous-unit
                (HASH form
                      ((COINCIDES ?SIGN-1 ?SIGN-2))))))) 
               

; construction for modifier big
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

