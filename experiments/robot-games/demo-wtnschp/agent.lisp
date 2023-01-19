;;;; /agent.lisp

(in-package :demo-wtnschp)

;; ---------
;; + Agent +
;; ---------

(defclass demo-agent (object-w-tasks agent)
  ((applied-cxn
    :accessor applied-cxn :initform nil
    :documentation "The applied cxn in the current interaction") ;; this can be removed as well
   (ontology
    :type blackboard :accessor ontology :initform (make-blackboard)
    :documentation "The agent's ontology, storing known categories")
   (grammar
    :type construction-set :accessor grammar :initform (make-agent-cxn-set)
    :documentation "The agent's grammar, storing grammatical constructions"))
  (:documentation "An agent in the robot-origins-of-syntax experiment"))

(defclass embodied-agent (demo-agent nao)
  ()
  (:documentation "A roos-agent embodied in a nao robot"))

(defun make-embodied-agent (experiment)
  "Make an instance of an agent and connect it to the robot"
  (let* ((robot-ip (get-configuration experiment :robot-ip))
         (robot-port (get-configuration experiment :robot-port))    
         (container-name (format nil "nao-~a-~a" robot-ip robot-port))
         (vision-server (vision-server experiment))
         (agent (make-instance 'embodied-agent
                               :experiment experiment
                               :connect-automatically nil)))
    (setf (nao-interface::ip agent) robot-ip)
    (setf (nao-interface::server-port agent) robot-port)
    (setf (nao-interface::container-name agent) container-name)
    (setf (nao-interface::vision-server agent) vision-server)
    (make-new-connection agent :test-connection t)
    agent))

(defmethod reset ((agent embodied-agent))
  "Reset the slots of the agent that change in every interaction"
  (setf (applied-cxn agent) nil)
  (setf (utterance agent) nil)
  (setf (communicated-successfully agent) nil))

(defmethod speaker? ((agent embodied-agent))
  "Check if the agent is the speaker"
  (eql (discourse-role agent) 'speaker))

(defmethod hearer? ((agent embodied-agent))
  "Check if the agent is the hearer"
  (eql (discourse-role agent) 'hearer))

(defun make-agent-cxn-set ()
  "Make the cxn set for the agent of the experiment"
  (let* ((grammar-name (make-const "color-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))))))
    cxn-inventory))

;; ---------------
;; + Formulation +
;; ---------------

;; Lexical
(defclass lexical-speaker-task (task-w-learning)
  ()
  (:documentation "The task the agent needs to execute when being the speaker"))

(defgeneric run-lexical-speaker-task (agent)
  (:documentation "Entry point for when the agent is the speaker"))

(defmethod run-lexical-speaker-task ((agent embodied-agent))
  "Run the speaker task and return the communicative success"
  (let ((task (make-instance 'lexical-speaker-task
                             :owner agent
                             :label 'lexical-speaker-task
                             :processes `(initial-process
                                          observe-scene           ;; observe the scene
                                          choose-topic            ;; choose a topic
                                          conceptualise           ;; conceptualise the topic
                                          produce                 ;; produce a word
                                          visual-input            ;; get the interpreted topic from human
                                          match-visual-input      ;; get the true topic
                                          determine-success       ;; determine success of the interaction
                                          consolidate             ;; update the agent's lexicon and categories
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'conceptualisation-diagnostic)
                                                (make-instance 'production-diagnostic))
                             :repairs (list (make-instance 'new-picture-repair)
                                            (make-instance 'new-category-repair)
                                            (make-instance 'new-word-repair))))
        task-results)
    (setf task-results (object-run-task agent task))
    (find-data (first task-results) 'communicated-successfully)))

;; -----------------
;; + Comprehension +
;; -----------------

;; Lexical
(defclass lexical-hearer-task (task-w-learning)
  ()
  (:documentation "The task the agent needs to execute when being the hearer"))

(defgeneric run-lexical-hearer-task (agent)
  (:documentation "Entry point for when the agent is the hearer"))

(defmethod run-lexical-hearer-task ((agent embodied-agent))
  "Run the hearer task and return the communicative success"
  (let ((task (make-instance 'lexical-hearer-task
                             :owner agent
                             :label 'lexical-hearer-task
                             :processes `(initial-process
                                          observe-scene           ;; observe the scene
                                          speech-input            ;; get speech input
                                          parse                   ;; parse the utterance
                                          interpret               ;; interpret the utterance
                                          visual-input            ;; get the correct topic from human
                                          match-visual-input      ;; match with observed scene
                                          determine-success       ;; determine success of the interaction
                                          hearer-learning         ;; apply learning if necessary
                                          consolidate             ;; update the agent's lexicon and categories
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'conceptualisation-diagnostic))
                             :repairs (list (make-instance 'new-picture-repair)
                                            (make-instance 'new-category-repair))))
        (task-results nil))
    (setf task-results (object-run-task agent task))
    (find-data (first task-results) 'communicated-successfully)))