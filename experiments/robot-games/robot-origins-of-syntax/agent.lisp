;;;; /agent.lisp

(in-package :roos)

;; ---------
;; + Agent +
;; ---------

(defclass roos-agent (object-w-tasks agent)
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

(defclass embodied-agent (roos-agent nao)
  ()
  (:documentation "A roos-agent embodied in a nao robot"))

(defun make-embodied-agent (experiment)
  "Make an instance of an agent and connect it to the robot"
  (let* ((simulation-mode (get-configuration experiment :simulation-mode))
         (robot-ip (get-configuration experiment :robot-ip))
         (robot-port (get-configuration experiment :robot-port))
         (container-name (format nil "nao-~a-~a" robot-ip robot-port))
         (agent (make-instance 'embodied-agent
                               :experiment experiment
                               :connect-automatically nil)))
    (setf (nao-interface::ip agent) robot-ip)
    (setf (nao-interface::server-port agent) robot-port)
    (setf (nao-interface::container-name agent) container-name)
    (unless simulation-mode
      (make-new-connection agent :test-connection t))
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
  (let* ((grammar-name (make-const "robot-grammar"))
         (cxn-inventory
          (eval `(def-fcg-constructions-with-type-hierarchy ,grammar-name
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
                             :processes '(initial-process
                                          observe-scene           ;; observe the scene
                                          sensory-scale           ;; do sensory scaling
                                          choose-topics           ;; choose a topic
                                          compute-all-saliencies  ;; compute saliency for all objects
                                          context-scale           ;; do context scaling
                                          lex-conceptualise       ;; conceptualise the topic
                                          lex-produce             ;; produce a word
                                          visual-input            ;; get the interpreted topic from human
                                          match-visual-input      ;; get the true topic
                                          determine-success       ;; determine success of the interaction
                                          lex-consolidate         ;; update the agent's lexicon and categories
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'conceptualisation-diagnostic)
                                                (make-instance 'production-diagnostic))
                             :repairs (list (make-instance 'new-scene-repair)
                                            (make-instance 'new-category-repair)
                                            (make-instance 'invention-repair))))
        task-results)
    (setf task-results (object-run-task agent task))
    (find-data (first task-results) 'communicated-successfully)))

;; Grammatical
(defclass grammatical-speaker-task (task-w-learning)
  ()
  (:documentation "The task the agents needs to execute when being the speaker"))

(defgeneric run-grammatical-speaker-task (agent)
  (:documentation "Entry point for when the agent is the speaker"))

(defmethod run-grammatical-speaker-task ((agent embodied-agent))
  "Run the speaker task and return communcative success"
  (let ((task (make-instance 'grammatical-speaker-task
                             :owner agent
                             :label 'grammatical-speaker-task
                             :processes '(initial-process         
                                          observe-scene           ;; observe the scene
                                          sensory-scale           ;; do sensory scaling
                                          choose-topics           ;; choose topics
                                          compute-all-saliencies  ;; compute saliency for all objects
                                          context-scale           ;; do context scaling
                                          gr-conceptualise        ;; conceptualise the topics
                                          gr-produce              ;; produce words
                                          visual-input            ;; get the interpreted topics from human
                                          match-visual-input      ;; get the true topic
                                          determine-success       ;; determine success of the interaction
                                          gr-consolidate          ;; update the agent's lexicon, categories and th
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'multiple-hypotheses-formulation-diagnostic))
                             :repairs (list (make-instance 'new-scene-repair)
                                            (make-instance 'add-cxn-or-th-link-formulation))))
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
                             :processes '(initial-process
                                          observe-scene           ;; observe the scene
                                          sensory-scale           ;; do sensory scaling
                                          compute-all-saliencies  ;; compute saliency for all objects
                                          context-scale           ;; do context scaling
                                          speech-input            ;; get speech input
                                          lex-parse               ;; parse the utterance
                                          lex-interpret           ;; interpret the utterance
                                          visual-input            ;; get the correct topic from human
                                          match-visual-input      ;; match with observed scene
                                          determine-success       ;; determine success of the interaction
                                          lex-hearer-learning     ;; apply learning if necessary
                                          lex-consolidate         ;; update the agent's lexicon and categories
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'conceptualisation-diagnostic))
                             :repairs (list (make-instance 'new-scene-repair)
                                            (make-instance 'new-category-repair))))
        (task-results nil))
    (setf task-results (object-run-task agent task))
    (find-data (first task-results) 'communicated-successfully)))

;; Grammatical
(defclass grammatical-hearer-task (task-w-learning)
  ()
  (:documentation "The task the agents needs to execute when being the hearer"))

(defgeneric run-grammatical-hearer-task (agent)
  (:documentation "Entry point for when the agent is the hearer"))

(defmethod run-grammatical-hearer-task ((agent embodied-agent))
  "Run the speaker task and return communcative success"
  (let ((task (make-instance 'grammatical-hearer-task
                             :owner agent
                             :label 'grammatical-hearer-task
                             :processes '(initial-process
                                          observe-scene           ;; observe the scene
                                          sensory-scale           ;; do sensory scaling
                                          compute-all-saliencies  ;; compute saliency for all objects
                                          context-scale           ;; do context scaling
                                          speech-input            ;; get speech input
                                          gr-parse                ;; parse the utterance
                                          gr-interpret            ;; interpret the utterance
                                          visual-input            ;; get the correct topics from human
                                          match-visual-input      ;; match with observed scene
                                          determine-success       ;; determine success of the interaction
                                          gr-hearer-learning      ;; apply learning if necessary
                                          gr-consolidate          ;; update the agent's lexicon, categories and th
                                          )
                             :diagnostics (list (make-instance 'detection-diagnostic)
                                                (make-instance 'multiple-hypotheses-comprehension-diagnostic))
                             :repairs (list (make-instance 'new-scene-repair)
                                            (make-instance 'add-cxn-or-th-link-comprehension))))
        task-results)
    (setf task-results (object-run-task agent task))
    (find-data (first task-results) 'communicated-successfully)))