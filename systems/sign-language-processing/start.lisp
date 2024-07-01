(ql:quickload :slp)
(in-package :slp)

;(monitors::activate-monitor trace-fcg)
;(monitors::activate-monitor trace-irl)

;------------------------
; Grammar configurations
;------------------------
;To make a grammar using slp, use the render and de-render mode 'set-of-predicates'

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
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:node-tests :check-duplicate :restrict-nr-of-nodes :restrict-search-depth)
                       (:max-number-of-nodes . 200)
                       (:max-search-depth . 50)
                       (:render-mode . :set-of-predicates) ; set de-render mode to set-of-predicates
                       (:de-render-mode . :set-of-predicates) ; set de-render mode to set-of-predicates
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

;------------------------------------
; + comprehending signed utterances +
;------------------------------------

; setting handedness configuration
(setf *dominant-hand*
  'RH
  ;'LH
  )

;(comprehend (xmls->predicates (read-elan "/Users/liesbetdevos/Projects/geoquery-sign/250-dataset/38_24_0.eaf")))