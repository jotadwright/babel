;;;; /experiment.lisp

(in-package :roos)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Robot stuff
(define-configuration-default-value :robot-ip "192.168.1.2")
(define-configuration-default-value :robot-port "1570")
(define-configuration-default-value :english-vocabulary '("left" "right" ;"middle"
                                                        "top" "bottom" ;"center" "up" "down"
                                                        "red" "blue" ;"yellow" "green" "pink" "cyan"
                                                        ; "large" "small"
                                                        ))
(define-configuration-default-value :input-form :text) ; :speech or :text

;; Interacting agents modes
(define-configuration-default-value :determine-interacting-agents-mode :robot-hearer-often) ; :robot-speaker-often
(define-configuration-default-value :robot-hearer-prob 0.7)
(define-configuration-default-value :robot-speaker-prob 0.7)

(define-configuration-default-value :nr-of-topics 1) ;; denotes the nr-of-topics in this interaction
(define-configuration-default-value :context-size 3) ;; denotes the context-size in this interaction

;; Configurations for word/category learning
(define-configuration-default-value :lexical-context-size 3)
(define-configuration-default-value :lexical-nr-of-topics 1)
(define-configuration-default-value :features '(:xpos :ypos :color))
(define-configuration-default-value :feature-bounds '((:xpos (min . 50)
                                                             (max . 650))
                                                      (:ypos (min . 50)
                                                             (max . 450))
                                                      (:color (min . (0 0 0))
                                                              (max . (255 255 255)))
                                                      ;(:area (min . 0)
                                                      ;       (max . 22000))
                                                      ))

;; Configurations for grammar learning
(define-configuration-default-value :min-context-size 6)
(define-configuration-default-value :max-context-size 6)
(define-configuration-default-value :min-nr-of-topics 2)
(define-configuration-default-value :max-nr-of-topics 2)

;; Configurations for alignment
(define-configuration-default-value :alignment-strategy :li)
(define-configuration-default-value :who-aligns :both)
(define-configuration-default-value :li-inc 0.1)
(define-configuration-default-value :li-dec 0.1)
(define-configuration-default-value :li-incf-weight 0.1) ;; rename to li-th-inc
(define-configuration-default-value :li-decf-weight 0.1) ;; rename to li-th-dec
(define-configuration-default-value :alignment-rate 0.3)
(define-configuration-default-value :align-lexicon-when-learning-grammar? t) ; t or nil

;; What game is played?
;; game-stage :lexical     --> learn words and categories
;;            :grammatical --> learn grammar
(define-configuration-default-value :game-stage :lexical) ; :grammatical

;; --------------
;; + Experiment +
;; --------------

(defclass roos-experiment (experiment)
  ()
  (:documentation "Experiment class for robots-origins-of-syntax"))

(defmethod initialize-instance :after ((experiment roos-experiment) &key)
  "Initialize the experiment"
  ;; set the population to a single robot
  (setf (population experiment)
        (list (make-embodied-agent experiment)))
  ;; start the web server that generates random scenes
  (start-scene-server))

(defmethod destroy ((experiment roos-experiment))
  "Some cleanup when manually running an experiment"
  ;; stop the scene server
  (stop-scene-server)
  ;; stop the connection to the robot
  (loop for agent in (population experiment)
        do (disconnect-robot agent)))


;;;; Determine Interacting Agents
(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :random-role-for-single-agent))
                                         &key &allow-other-keys)
  "Assign a random role to the agent"
  (let ((agents (agents experiment)))
    (setf (interacting-agents interaction)           agents
          (discourse-role (first agents))            (random-elt '(speaker hearer))
          (utterance (first agents))                 nil
          (communicated-successfully (first agents)) nil)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :robot-hearer-often))
                                         &key &allow-other-keys)
  "The agent has a higher probability of being hearer"
  (let ((agents (agents experiment))
        (prob (get-configuration experiment :robot-hearer-prob)))
    (setf (interacting-agents interaction) agents)
    (if (> (random 1.0) prob)
      (setf (discourse-role (first agents)) 'speaker)
      (setf (discourse-role (first agents)) 'hearer))
    (setf (utterance (first agents)) nil
          (communicated-successfully (first agents)) nil)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :robot-speaker-often))
                                         &key &allow-other-keys)
  "The agent has a higher probability of being speaker"
  (let ((agents (agents experiment))
        (prob (get-configuration experiment :robot-speaker-prob)))
    (setf (interacting-agents interaction) agents)
    (if (> (random 1.0) prob)
      (setf (discourse-role (first agents)) 'hearer)
      (setf (discourse-role (first agents)) 'speaker))
    (setf (utterance (first agents)) nil
          (communicated-successfully (first agents)) nil)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :always-speaker))
                                         &key &allow-other-keys)
  "The agent is always speaker"
  (setf (interacting-agents interaction) (agents experiment))
  (setf (discourse-role (first (agents experiment))) 'speaker)
  (notify interacting-agents-determined experiment interaction))

(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :always-hearer))
                                         &key &allow-other-keys)
  "The agent is always hearer"
  (setf (interacting-agents interaction) (agents experiment))
  (setf (discourse-role (first (agents experiment))) 'hearer)
  (notify interacting-agents-determined experiment interaction))

;;;; Activate Grammar
(defmethod activate-grammar ((experiment roos-experiment))
  "Set configurations to switch from lexicon learning to grammar learning"
  (let ((cxn-inventory (grammar (first (agents experiment)))))
    (set-configuration experiment
                       :game-stage
                       :grammatical
                       :replace t)
    (set-configuration cxn-inventory
                       :production-goal-tests
                       '(:no-meaning-in-root
                         :single-interpretation-in-world-formulation
                         :not-more-than-two-unit-structures)
                       :replace t)
    (set-configuration cxn-inventory
                       :parse-goal-tests
                       '(:no-strings-in-root
                         :single-interpretation-in-world-comprehension
                         :not-more-than-two-unit-structures)
                       :replace t)
    (set-configuration cxn-inventory
                       :cxn-supplier-mode
                       :cxn-supplier-categorisation-type-hierarchy
                       :replace t)
    (set-configuration cxn-inventory
                       :queue-mode
                       :backtrack-over-grammatical-cxns-only
                       :replace t)
    (set-configuration cxn-inventory
                       :priority-mode
                       :depth-first-with-type-hierachy-weights
                       :replace t)
    (set-configuration cxn-inventory
                       :node-expansion-mode
                       :expand-with-multiple-cxns
                       :replace t)))

;;;; Run Series
(define-event interaction-in-series-finished (interaction-number number) (number-of-interactions number))

(defmethod run-series ((experiment roos-experiment)
                       (number-of-interactions number)
                       &key)
  "Run a series. The robot waits for a head-touch after every interaction"
  (loop for interaction from 1 to number-of-interactions 
        do (progn
             (run-interaction experiment)
             (notify interaction-in-series-finished interaction number-of-interactions)
             (unless (= interaction number-of-interactions)
               (head-touch-middle (first (population experiment))))))
  (notify run-series-finished experiment))

;; ---------------
;; + Scaffolding +
;; ---------------

(defun scaffold-ontology-and-grammar (agent)
  "Scaffold the agent's lexicon"
  (let* ((ontology (make-blackboard))
         (cxn-set (make-agent-cxn-set))
         (left (make-xpos-category 0))
         ;(xmiddle (make-xpos-category 0.5))
         (right (make-xpos-category 1))
         (top (make-ypos-category 0))
         ;(ymiddle (make-ypos-category 0.5))
         (bottom (make-ypos-category 1))
         ;(large (make-area-category 1))
         ;(small (make-area-category 0))
         (red (make-color-category '(1 0 0)))
         ;(green (make-color-category '(0 1 0)))
         (blue (make-color-category '(0 0 1)))
         ;(pink (make-color-category '(1 0 1)))
         ;(cyan (make-color-category '(0 1 1)))
         ;(yellow (make-color-category '(1 1 0)))
         )
    (setf (grammar agent) cxn-set)
    (setf (ontology agent) ontology)
    (set-data ontology :xpos (list left right))
    (set-data ontology :ypos (list top bottom))
    ;(set-data ontology :area (list large small))
    (set-data ontology :color (list red blue))
    (add-lex-cxn agent "left" left)
    ;(add-lex-cxn agent "middle" xmiddle)
    (add-lex-cxn agent "right" right)
    (add-lex-cxn agent "top" top)
    ;(add-lex-cxn agent "center" ymiddle)
    (add-lex-cxn agent "bottom" bottom)
    ;(add-lex-cxn agent "large" large)
    ;(add-lex-cxn agent "small" small)
    (add-lex-cxn agent "red" red)
    (add-lex-cxn agent "blue" blue)
    ;(add-lex-cxn agent "green" green)
    ;(add-lex-cxn agent "pink" pink)
    ;(add-lex-cxn agent "cyan" cyan)
    ;(add-lex-cxn agent "yellow" yellow)
    ))

(defmethod scaffold-lexicon ((experiment roos-experiment))
  "Scaffold the agent's lexicon"
  (let ((agent (first (agents experiment))))
    (scaffold-ontology-and-grammar agent)))