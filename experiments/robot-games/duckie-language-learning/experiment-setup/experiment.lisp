(in-package :duckie-language-learning)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(define-configuration-default-value :meaning-representation :irl)

(define-configuration-default-value :observation-sample-mode :train) ; train, debug, development or evaluation
(define-configuration-default-value :number-of-epochs 1) ; how many times the training data is concatenated in random variations
(define-configuration-default-value :de-render-mode :de-render-string-meets)

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)

(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.3)

(define-configuration-default-value :evaluation-grammar nil)
(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :remove-cxn-on-lower-bound nil)
(define-configuration-default-value :categorial-network-export-interval 1)
(define-configuration-default-value :initial-categorial-link-weight 0.0)

(define-configuration-default-value :determine-interacting-agents-mode :hearer-only) ;; TODO (conflict with hearer in interaction.lisp if not set)
(define-configuration-default-value :learner-cxn-supplier :hashed-and-scored)
(define-configuration-default-value :category-linking-mode :neighbours)
(define-configuration-default-value :learning-strategy :optimal-form-coverage) ;or: by-score
(define-configuration-default-value :categorial-link-repair-mode-comprehension :no-path-required)

;; debug formatting
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :result-display-interval 100)

;; --------------
;; + Experiment +
;; --------------
(defclass duckie-language-learning-experiment (experiment)
  ()
  (:documentation "A duckiebot language learning experiment"))

;; --------------
;; + Experiment +
;; --------------
(defclass duckie-language-learning-simulation-experiment (duckie-language-learning-experiment)
  ()
  (:documentation "A duckiebot language learning experiment in simulation"))

(defmethod initialize-instance :after ((experiment duckie-language-learning-simulation-experiment) &key)
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-instance 'duckie-language-learning-simulation-agent))))

;; --------------
;; + Experiment +
;; --------------
(defclass duckie-language-learning-world-experiment (duckie-language-learning-experiment)
  ()
  (:documentation "A duckiebot language learning experiment in simulation"))

(defmethod initialize-instance :after ((experiment duckie-language-learning-world-experiment) &key)
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-instance 'duckie-language-learning-world-agent))))

;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------
(defmethod determine-interacting-agents ((experiment duckie-language-learning-experiment)
                                         interaction (mode (eql :hearer-only)) &key)
  (let ((agent (first (population experiment))))
    (setf (interacting-agents interaction) (list agent)
          (discourse-role agent) 'hearer)
    (setf (utterance agent) nil
          (communicated-successfully agent) nil)))
