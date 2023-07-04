(in-package :pattern-finding-old)

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
  (with-configurations ((cxn-supplier-mode :learner-cxn-supplier)
                        (de-render-mode :de-render-mode)
                        (max-nr-of-nodes :max-number-of-nodes)
                        (mark-holophrases :mark-holophrases)
                        (meaning-representation :meaning-representation)
                        (category-linking-mode :category-linking-mode)
                        (initial-cxn-score :initial-cxn-score)
                        (initial-link-weight :initial-categorial-link-weight)
                        (repairs :repairs)
                        (max-au-cost :max-au-cost)) experiment
    (let* ((grammar-name (make-const "pattern-finding-grammar"))
           (cxn-inventory
            (eval `(def-fcg-constructions ,grammar-name
                     :cxn-inventory ,grammar-name
                     :hashed t
                     :feature-types ((form-args sequence)
                                     (meaning-args sequence)
                                     (form set-of-predicates)
                                     (meaning set-of-predicates)
                                     (subunits set)
                                     (footprints set))
                     :fcg-configurations ((:cxn-supplier-mode . ,cxn-supplier-mode)
                                          (:parse-order routine)
                                          (:production-order routine)
                                          (:hash-mode . :hash-string-meaning)
                                          (:node-tests :restrict-nr-of-nodes
                                                       :restrict-search-depth
                                                       :check-duplicate)
                                          (:parse-goal-tests :no-strings-in-root
                                                             :no-applicable-cxns
                                                             :connected-semantic-network
                                                             :connected-structure
                                                             :non-gold-standard-meaning)
                                          (:de-render-mode . ,de-render-mode)
                                          (:max-nr-of-nodes . ,max-nr-of-nodes)
                                          (:original-max-nr-of-nodes . ,max-nr-of-nodes)
                                          (:mark-holophrases . ,mark-holophrases)
                                          (:meaning-representation-formalism . ,meaning-representation)
                                          (:render-mode . :generate-and-test)
                                          (:category-linking-mode . ,category-linking-mode)
                                          (:update-categorial-links . t)
                                          (:consolidate-repairs . t)
                                          (:use-meta-layer . t)
                                          (:initial-cxn-score . ,initial-cxn-score)
                                          (:initial-categorial-link-weight . ,initial-link-weight)
                                          (:ignore-transitive-closure . t)
                                          (:max-au-cost . ,max-au-cost)
                                          (:ignore-nil-hashes . nil))
                     :diagnostics (pf::diagnose-non-gold-standard-meaning
                                   pf::diagnose-non-gold-standard-utterance)
                     :repairs ,repairs
                     :visualization-configurations ((:show-constructional-dependencies . nil)
                                                    (:show-categorial-network . t))))))
      cxn-inventory)))
     


