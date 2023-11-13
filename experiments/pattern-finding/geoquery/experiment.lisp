(in-package :pf-for-sql)

(defclass pf-for-sql-experiment (experiment) 
  ((corpus :initarg :corpus :initform nil 
           :accessor corpus :type list
           :documentation "A list of question-answer (the answer is predicate network here) pairs."))
  (:documentation "A grammar learning experiment regarding geoquery for sql."))

(defclass pf-for-sql-agent (agent)
  ((grammar :initarg :grammar :initform nil
            :type (or null fcg-construction-set)
            :accessor grammar)))

;;------------------------;;
;; Setting up interaction ;;
;;------------------------;;

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

;; Learning Operators
(define-configuration-default-value :repairs 
                                    '(add-categorial-links
                                      add-item-based                                   
                                      add-holophrase))

(defun make-agent-cxn-set (experiment)
  (with-configurations ((repairs :repairs)) experiment
    (let* ((grammar-name (make-const "pf-for-sql-grammar"))
           (cxn-inventory
            (eval `(fcg:def-fcg-constructions ,grammar-name
                     :cxn-inventory ,grammar-name
                     :hashed t
                     :feature-types ((form set-of-predicates :handle-regex-sequences)
                                     (meaning set-of-predicates)
                                     (form-args sequence)
                                     (meaning-args sequence)
                                     (subunits set)
                                     (footprints set))     
                     :fcg-configurations (;; to activate heuristic search
                                          (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                                          (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                                          (:cxn-supplier-mode . :hashed) ;; use hashing
                                          ;; for using heuristics
                                          (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                                          (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                                          (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                      ; (:hash-mode . :hash-string-meaning)
                                          (:de-render-mode . :de-render-sequence)
                                          (:render-mode . :render-sequences)
                                          (:category-linking-mode . :neighbours)
                                          (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)
                                          )
                     :diagnostics (pf::diagnose-non-gold-standard-meaning
                                   pf::diagnose-non-gold-standard-utterance)
                     :repairs ,repairs))))
      cxn-inventory)))

(defmethod make-agents ((experiment pf-for-sql-experiment))
  "A method that creates two agents in the experiment : a tutor and a learner."
  (let ((agents (loop for i from 1 to 2
                      for agent-id = (read-from-string (format nil "agent-~d" i))
                        collect (make-instance 'pf-for-sql-agent
                                               :id agent-id
                                               :experiment 'experiment))))
    (setf (population experiment) agents)
    (setf tutor-agent (first (population experiment)))
    (setf learner-agent (second (population experiment)))
    (setf (discourse-role tutor-agent) 'tutor-agent)
    (setf (discourse-role learner-agent) 'learner-agent)
    (setf (grammar learner-agent) (make-agent-cxn-set experiment))))

(defun load-corpus ()
  "A function to load the jsonl file : returns a list of utterance-meaning pairs."
  (let ((file-data '()))
    (with-open-file (stream "/Users/ajouglar/babel/systems/postmodern-parser/data/geography-for-pf.jsonl" :direction :input :external-format :utf-8 :element-type :default)
      (loop for line = (read-line stream nil nil)
            while line
            do (let*
                   ((processed-line (remove-last-character line))
                    (data (com.inuoe.jzon:parse processed-line))
                    (utterance (gethash "utterance" data)))
                 (with-input-from-string (meaning (gethash "meaning" data))
                   (pushend (list `(:utterance ,utterance) `(:meaning ,(read meaning))) file-data)))))
    file-data))

(defmethod initialize-instance :after ((experiment pf-for-sql-experiment) &key)
  "Create the population and load the corpus."
  ;; set the corpus of the experiment
  (setf (corpus experiment) (load-corpus))
  ;; set the population of the experiment
  (make-agents experiment)
  (print (population experiment)))

(defun interact (experiment interaction)
  "The different steps of an interaction between the given agents ('interact' is called by 'run-interaction' in the experimentation-framework package)"
  ;1) initializes instances
  (let* ((tutor-agent (first (population experiment)))
         (learner-agent (second (population experiment)))
         (utterance)
         (feedback))
  ;; 2) The tutor speaks an utterance
    (setf (utterance tutor-agent) (first (corpus experiment)))
    ;; (print (first (corpus experiment)))
    (setf utterance (second (assoc ':utterance (utterance tutor-agent))))
  ;; 3) The learner agent first has an empty cxn set, so it can't understand,
  ;; so the tutor gives feedback and the learner stores a holophrase
    (notify interaction-before-finished utterance (first (cdr (assoc ':meaning (utterance tutor-agent)))))
    (if (= (interaction-number interaction) 1)
      (progn ;; first repair is adding a holophrase for the first utterance
        (setf feedback (first (cdr (assoc ':meaning (utterance tutor-agent)))))
        (learn-holophrase utterance feedback (grammar learner-agent)))
      (add-element (make-html (constructions-list (grammar learner-agent)))))))
