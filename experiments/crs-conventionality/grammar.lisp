(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                        ;;
;; Code implementing grammars             ;;
;;                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric make-initial-grammar (agent)
  (:documentation "Initialises the grammar of an agent."))

#|
 
(defmethod make-initial-grammar ((agent naming-game-agent))
  "Initialises the grammar of a naming game agent."
  (let* ((grammar-name (make-const "grammar")))
    (setf (grammar agent) (eval `(def-fcg-constructions ,grammar-name
                                   :cxn-inventory ,grammar-name
                                   :feature-types ((form set-of-predicates :handle-regex-sequences)
                                                   (meaning set-of-predicates))
                                   :fcg-configurations (
                                                        (:node-expansion-mode . :full-expansion)
                                                        ;; for using heuristics
                                                        (:consolidate-repairs . t)
                                                        (:de-render-mode . :de-render-sequence-predicates)
                                                        (:render-mode . :render-sequences)
                                                        (:category-linking-mode . :neighbours)
                                                        (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :no-sequence-in-root)
                                                        (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)))))))

|#