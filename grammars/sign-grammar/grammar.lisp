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
               (syn-cat noun)
               (salient-movf ?movf-2))
              <- 
              (?ball-unit 
               (HASH meaning ((ball ?b)))
               -- 
               (HASH form ((sign ?sign-1)
                           (manual ?sign-1 ?m-1)
                           (non-manual ?sign-1 ?nm-1)
                           (mouthpicture ?nm-1 "ball")
                           (symmetry ?m-1 ?sym-1)
                           (hamsymmlr ?sym-1 ?symf-1)
                           (handshape ?m-1 ?hs-1)
                           (hamfinger2345 ?hs-1 ?hsf-1)
                           (meets ?hsf-1 ?hsf-2)
                           (hambetween ?hs-1 ?hsf-2)
                           (meets ?hsf-2 ?hsf-3)
                           (hamflathand ?hs-1 ?hsf-3)
                           (hammod ?hsf-3 hamfingerbendmod)
                           (extended-finger-direction ?m-1 ?ext-1)
                           (hamextfingerol ?ext-1 ?extf-1)
                           (palm-orientation ?m-1 ?ori-1)
                           (hampalmdl ?ori-1 ?orif-1)
                           (location ?m-1 ?loc-1)
                           (hamtouch ?loc-1 ?locf-1)
                           (meets ?locf-1 ?locf-2)
                           (hamshoulders ?loc-1 ?locf-2)
                           (movement ?m-1 ?mov-1)
                           (meets ?movf-1 ?movf-2)
                           (hamparbegin ?mov-1 ?movf-1)
                           (hammoved ?mov-1 ?movf-2)
                           (meets ?movf-2 ?movf-3)
                           (hamarcr ?mov-1 ?movf-3)
                           (meets ?movf-3 ?movf-4)
                           (hamreplace ?mov-1 ?movf-4)
                           (palm-orientation ?m-1 ?ori-2)
                           (precedes ?ori-1 ?ori-2)
                           (hampalmu ?ori-2 ?orif-2)
                           (meets ?orif-2 ?orif-3)
                           (hamparend ?ori-2 ?orif-3)
                           (location ?m-1 ?loc-2)
                           (precedes ?loc-1 ?loc-2)
                           (hamtouch ?loc-2 ?locf-3))))))


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
