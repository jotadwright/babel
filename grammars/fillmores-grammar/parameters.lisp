(ql:quickload :fcg)
(in-package :fcg)

(defvar *fillmores-cxns*)

(def-fcg-constructions fillmores-grammar
  :cxn-inventory *fillmores-cxns*
  :hashed t
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (sequences set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (dependents sequence)
                  (args sequence)
                  (footprints set)
                  (boundaries sequence))
  :fcg-configurations ((:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       ;; goal tests for comprehension
                       (:parse-goal-tests
                        :no-sequence-in-root
                        ;; :no-applicable-cxns ;; succeeds if no more cxns can apply
                        :connected-semantic-network ;; succeeds if the semantic network is fully connected
                        :connected-structure) ;; succeeds if all units are connected
                       ;; goal tests for formulation
                       (:production-goal-tests
                        :no-applicable-cxns ;; succeeds if node is fully expanded and no cxns could apply to its children
                        :no-meaning-in-root)) ;; succeeds if no meaning predicates remain in root
  #|:visualization-configurations ((:show-constructional-dependencies . nil)
                                   (:hide-features . nil)
                                   ;(:hide-features . (footprints superunits))
                                   (:with-search-debug-data . t))|#
  :hierarchy-features (subunits dependents))

(activate-monitor trace-fcg)