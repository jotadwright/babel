(in-package :duckie-language-learning)
;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------


;; Finding the data
(define-configuration-default-value :meaning-representation :irl)

(define-configuration-default-value :de-render-mode :de-render-string-meets)

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :remove-cxn-on-lower-bound nil)
(define-configuration-default-value :categorial-network-export-interval 1)
(define-configuration-default-value :initial-categorial-link-weight 0.0)

(define-configuration-default-value :learner-cxn-supplier :hashed-and-scored)
(define-configuration-default-value :category-linking-mode :neighbours)


;; Misc
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :result-display-interval 100)

;; --------------
;; + Experiment +
;; --------------

(defclass duckie-language-learning-experiment (experiment)
  ()
  (:documentation "A duckiebot language learning experiment"))

(defmethod initialize-instance :after ((experiment duckie-language-learning-experiment) &key)
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-instance 'duckie-language-learning-agent))))


;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------

(defmethod determine-interacting-agents ((experiment duckie-language-learning-experiment)
                                         interaction (mode (eql :hearer-only)) &key)
  (let ((agent (first (population experiment))))
    (setf (interacting-agents interaction) (list agent)
          (discourse-role agent) 'hearer)
    (setf (utterance agent) nil
          (communicated-successfully agent) nil)
  ;(notify interacting-agents-determined experiment interaction)
  ))