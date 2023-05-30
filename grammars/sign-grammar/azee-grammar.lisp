(in-package :sign-grammar)

;;------------------------;;
;; Grammar configurations ;;
;;------------------------;;

(def-fcg-constructions azee-sign-grammar
  :cxn-inventory *azee-sign-grammar*
  :feature-types ((subunits set)
                  (args set)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (holds set-of-predicates)
                  (boundaries set-of-predicates))
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

;; ile

(def-fcg-cxn island-cxn
             ((?island-unit
               (referent ?i)
               (category concept)
               (az-block ?block-1))
               <-
               (?island-unit
                (hash meaning ((island ?i)))
                --
                (hash form
                      ((az-block ?block-1)                  
                       (az-expr ?block-1 ile)
                       (duration ?block-1 1))))))

(def-fcg-cxn indonesia-cxn
             ((?indonesia-unit
               (referent ?i)
               (category concept)
               (az-block ?block-1))
               <-
               (?indonesia-unit
                (hash meaning ((indonesia ?i)))
                --
                (hash form
                      ((az-block ?block-1)
                       (az-expr ?block-1 indonesie)
                       (duration ?block-1 1))))))

(def-fcg-cxn info-about-cxn
             ((?topic-unit
               (subunits (?info-unit)))
             <-
             (?topic-unit
              (hash meaning ((arg1 ?b ?i-2)
                             (be.03 ?b)))
              (referent ?i-2)
              --
              (category concept)
              (az-block ?block-2))
             (?info-unit
              (hash meaning ((arg2 ?b ?i-1)))
              (referent ?i-1)
              --
              (category concept)
              (az-block ?block-1)
              (hash form ((az-block block-3)
                          (az-expr block-3 chin-up-no-focus)
                          (duration block-3 1)
                          (orient block-3 ori-1)
                          (bone ori-1 head)
                          (axis ori-1 NRM)
                          (mode ori-1 along)
                          (addvect ori-1 fwd)
                          (scalevect ori-1 0.2)
                          (direction ori-1 up)
                          (synchronisation block-2 sync-1)  
                          (attribute sync-1 start)
                          (reference-end sync-1 block-1)
                          (value sync-1 .2)
                          (synchronisation block-3 sync-2)   
                          (attribute sync-2 start)
                          (reference-start sync-2 block-1)
                          (value sync-2 -.2)
                          (synchronisation block-3 sync-3)   
                          (attribute sync-3 end)
                          (reference-start block-2)
                          (value sync-3 .1))))))

)

