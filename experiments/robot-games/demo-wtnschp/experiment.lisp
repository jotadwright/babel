;;;; /experiment.lisp

(in-package :demo-wtnschp)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Robot stuff
(define-configuration-default-value :robot-ip "192.168.1.4")
(define-configuration-default-value :robot-port "7850")
(define-configuration-default-value :robot-vocabulary '((:en "green" "yellow" "blue" "red" "grey" "orange")
                                                        (:nl "groen" "geel" "blauw" "rood" "grijs" "oranje")
                                                        (:fr "vert" "jaune" "bleu" "rouge" "gris" "orange")))
(define-configuration-default-value :dutch-nonsense '("tozo" "baga" "lose" "huma" "tado"
                                                      "sela" "muga" "basa" "zabi" "peza"
                                                      "futa" "tili" "rabi" "faru" "moxa"
                                                      "foda" "pawa" "gugo" "liru" "mago"
                                                      "toso" "levi" "dada" "subi" "pira"
                                                      "pulu" "zala" "neto" "paru" "pulu"
                                                      "rika" "raso" "puwa" "savi" "vono"
                                                      "tawa" "gogi" "nipi" "niki" "sero"
                                                      "zago"))
(define-configuration-default-value :input-form :text) ; :speech or :text
(define-configuration-default-value :input-lang :en)
(define-configuration-default-value :printer-name "Canon_SELPHY_CP1300")

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
(define-configuration-default-value :alpha 0.5)

;; --------------
;; + Experiment +
;; --------------

(defclass demo-experiment (experiment)
  ((vision-server :accessor vision-server :initarg :vision-server :documentation "Vision server for analyzing pictures the robot takes"))
  (:documentation "Experiment class"))

;(activate-monitor record-communicative-success)
;(activate-monitor display-communicative-success)

(defmethod initialize-instance :after ((experiment demo-experiment) &key)
  "Initialize the experiment"
  ;; set the population to a single robot
  (setf (population experiment)
        (list (make-embodied-agent experiment)))
  (setf *used-dutch-nonsense-words* nil)
  (case (get-configuration experiment :input-lang)
    (:en (speak (first (population experiment)) "Do you want to play a language game?"))
    (:nl (speak (first (population experiment)) "Wil je een taalspelletje over kleuren spelen?" :speed 75))
    (:fr (speak (first (population experiment)) "Veux-tu jouer a un jeu de langage?"))))

(defmethod destroy ((experiment demo-experiment))
  "Some cleanup when manually running an experiment"
  ;; stop the connection to the robot
  (loop for agent in (population experiment)
        do (disconnect-robot agent)))

(defun grammar->chips (agent)
  (loop for category in (find-data (ontology agent) 'color-categories)
        for cxns = (find-cxn-by-meaning (id category) agent :all)
        for color-category-id = (attr-val (first cxns) :meaning)
        for color-category = (find color-category-id (find-data (ontology agent) 'color-categories) :key #'id)
        for color = (mapcar #'(lambda (x) (float (/ x 255))) (prototype color-category))
        for words = (loop for cxn in cxns
                          collect (list (attr-val cxn :form) (attr-val cxn :score)))
        collect (list color (sort words #'> :key #'second))))

(defmethod print-robot-lexicon ((experiment demo-experiment))
  (let* ((agent (first (population experiment)))
         (chips (grammar->chips agent))
         (args (loop repeat (length chips)
                     for i from 1
                     for kwarg = (make-kw (string-append "chip" (mkstr i)))
                     for val = (nth (- i 1) chips)
                     append (list kwarg val))))
    (apply #'print-lexicon (get-configuration experiment :printer-name) args)))


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
                                         (mode (eql :alternating))
                                         &key &allow-other-keys)
  (let ((agents (agents experiment)))
    (setf (interacting-agents interaction)           agents
          (discourse-role (first agents))            (if (oddp (interaction-number interaction)) 'speaker 'hearer)
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
               (detect-head-touch (first (population experiment)) :middle))))
  (notify run-series-finished experiment))