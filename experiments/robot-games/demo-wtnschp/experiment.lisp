;;;; /experiment.lisp

(in-package :demo-wtnschp)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Robot stuff
(define-configuration-default-value :robot-ip "192.168.1.2")
(define-configuration-default-value :robot-port "7850")
(define-configuration-default-value :robot-vocabulary '((:en "green" "yellow" "blue" "red" "grey" "black" "pink")
                                                        (:nl "groen" "geel" "blauw" "rood" "grijs" "zwart" "roze")))
(define-configuration-default-value :input-form :text) ; :speech or :text
(define-configuration-default-value :input-lang :nl) 

;; Interacting agents modes
(define-configuration-default-value :determine-interacting-agents-mode :random-role-for-single-agent) ; :robot-speaker-often
(define-configuration-default-value :robot-hearer-prob 0.7)
(define-configuration-default-value :robot-speaker-prob 0.7)
(define-configuration-default-value :context-size 3)

;; Configurations for alignment
(define-configuration-default-value :alignment-strategy :li)
(define-configuration-default-value :who-aligns :both)
(define-configuration-default-value :li-inc 0.1)
(define-configuration-default-value :li-dec 0.1)
(define-configuration-default-value :alpha 0.3)

;; --------------
;; + Experiment +
;; --------------

(defclass demo-experiment (experiment)
  ()
  (:documentation "Experiment class"))

(defmethod initialize-instance :after ((experiment demo-experiment) &key)
  "Initialize the experiment"
  ;; set the population to a single robot
  (setf (population experiment)
        (list (make-embodied-agent experiment))))

(defmethod destroy ((experiment demo-experiment))
  "Some cleanup when manually running an experiment"
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

;;;; Run Series
(define-event interaction-in-series-finished (interaction-number number) (number-of-interactions number))

(defmethod run-series ((experiment demo-experiment)
                       (number-of-interactions number)
                       &key)
  "Run a series. The robot waits for a head-touch after every interaction"
  (loop for interaction from 1 to number-of-interactions 
        do (progn (run-interaction experiment)
             (notify interaction-in-series-finished interaction number-of-interactions)
             (unless (= interaction number-of-interactions)
               (head-touch-middle (first (population experiment))))))
  (notify run-series-finished experiment))