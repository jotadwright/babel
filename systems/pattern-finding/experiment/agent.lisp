(in-package :pf)

(defclass pattern-finding-agent (agent)
  ((meaning :initarg :meaning :initform nil
            :accessor meaning :type (or null entity number)
            :documentation "The meaning representation for the current utterance")
   (grammar :initarg :grammar :accessor grammar :initform nil
            :type (or null fcg-construction-set)
            :documentation "The agent's grammar")))

(defun make-pattern-finding-agent (experiment)
  (make-instance 'pattern-finding-agent
                 :experiment experiment
                 :grammar (make-agent-cxn-set experiment)))

(defun make-agent-cxn-set (experiment)
  (with-configurations ((meaning-representation :meaning-representation)
                        (form-representation :form-representation)
                        (max-nr-of-nodes :max-number-of-nodes)
                        (mark-holophrases :mark-holophrases)
                        (category-linking-mode :category-linking-mode)
                        (initial-cxn-score :initial-cxn-score)
                        (initial-link-weight :initial-categorial-link-weight)
                        (repairs :repairs)
                        (max-au-cost :max-au-cost)
                        (allow-cxns-with-no-strings :allow-cxns-with-no-strings)
                        (cxn-supplier-mode :learner-cxn-supplier)
                        (anti-unification-mode :anti-unification-mode)
                        (partial-analysis-mode :partial-analysis-mode)
                        (repair-recursively :repair-recursively)) experiment
    (let* ((grammar-name (make-const "pattern-finding-grammar"))
           (cxn-inventory
            (eval `(def-fcg-constructions ,grammar-name
                     :cxn-inventory ,grammar-name
                     :hashed ,(case form-representation
                                (:string+meets t)
                                (:sequences nil))
                     :feature-types (,(case form-representation
                                        (:string+meets '(form set-of-predicates))
                                        (:sequences '(form set-of-predicates :handle-regex-sequences)))
                                     (meaning set-of-predicates)
                                     (form-args sequence)
                                     (meaning-args sequence)
                                     (subunits set)
                                     (footprints set))                     
                     :fcg-configurations ((:construction-inventory-processor-mode . :heuristic-search)
                                          (:node-expansion-mode . :full-expansion)
                                          (:cxn-supplier-mode . ,cxn-supplier-mode)
                                          (:search-algorithm . :best-first)
                                          (:heuristics :nr-of-applied-cxns :cxn-score)
                                          (:heuristic-value-mode . :sum-heuristics-and-parent)

                                          (:de-render-mode . ,(case form-representation
                                                                (:string+meets :de-render-string-meets-no-punct)
                                                                (:sequences :de-render-sequence)))
                                          (:render-mode . ,(case form-representation
                                                             (:string+meets :generate-and-test)
                                                             (:sequences :render-sequences)))

                                          (:meaning-representation-formalism . ,meaning-representation)
                                          (:form-representation-formalism . ,form-representation)
                                          (:parse-order routine-apply-first routine-apply-last)
                                          (:production-order routine-apply-first routine-apply-last)
                                          (:hash-mode . :hash-string-meaning)
                                          (:node-tests :restrict-nr-of-nodes
                                                       :restrict-search-depth
                                                       :check-duplicate-strict)
                                          (:parse-goal-tests :no-strings-in-root
                                                             :no-applicable-cxns
                                                             :connected-semantic-network
                                                             :connected-structure
                                                             :non-gold-standard-meaning)
                                          (:max-nr-of-nodes . ,max-nr-of-nodes)
                                          (:original-max-nr-of-nodes . ,max-nr-of-nodes)
                                          (:mark-holophrases . ,mark-holophrases)
                                          (:category-linking-mode . ,category-linking-mode)
                                          (:update-categorial-links . t)
                                          (:consolidate-repairs . t)
                                          (:use-meta-layer . t)
                                          (:initial-cxn-score . ,initial-cxn-score)
                                          (:initial-categorial-link-weight . ,initial-link-weight)
                                          (:ignore-transitive-closure . t)
                                          (:max-au-cost . ,max-au-cost)
                                          (:allow-cxns-with-no-strings . ,allow-cxns-with-no-strings)
                                          (:ignore-nil-hashes . nil)
                                          (:anti-unification-mode . ,anti-unification-mode)
                                          (:partial-analysis-mode . ,partial-analysis-mode)
                                          (:repair-recursively . ,repair-recursively))
                     :diagnostics (pf::diagnose-non-gold-standard-meaning
                                   pf::diagnose-non-gold-standard-utterance)
                     :repairs ,repairs
                     :visualization-configurations ((:show-constructional-dependencies . nil)
                                                    (:show-categorial-network . nil))))))
      cxn-inventory)))
     


