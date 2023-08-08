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
  (with-configurations ((meaning-representation :meaning-representation)
                        (form-representation :form-representation)
                        (max-nr-of-nodes :max-number-of-nodes)
                        (mark-holophrases :mark-holophrases)
                        (category-linking-mode :category-linking-mode)
                        (initial-cxn-score :initial-cxn-score)
                        (initial-link-weight :initial-categorial-link-weight)
                        (repairs :repairs)
                        (max-au-cost :max-au-cost)
                        (cxn-supplier-mode :learner-cxn-supplier)) experiment
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
                     :fcg-configurations ((:de-render-mode . ,(case form-representation
                                                                (:string+meets :de-render-string-meets-no-punct)
                                                                (:sequences :de-render-sequence)))
                                          (:render-mode . ,(case form-representation
                                                             (:string+meets :generate-and-test)
                                                             (:sequences :render-sequences)))
                                          (:cxn-supplier-mode . ,cxn-supplier-mode)

                                          (:meaning-representation-formalism . ,meaning-representation)
                                          (:form-representation-formalism . ,form-representation)

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
                                          (:ignore-nil-hashes . nil))
                     :diagnostics (pf::diagnose-non-gold-standard-meaning
                                   pf::diagnose-non-gold-standard-utterance)
                     :repairs ,repairs
                     :visualization-configurations ((:show-constructional-dependencies . nil)
                                                    (:show-categorial-network . t))))))
      cxn-inventory)))
     


