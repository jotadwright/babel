;;;; experiment.lisp

(in-package :clg)

;; --------------
;; + Experiment +
;; --------------

(defclass clevr-learning-experiment (experiment)
  ((question-data :initarg :question-data :initform nil 
                   :accessor question-data :type list
                   :documentation "A list of samples for the current challenge level")
   (confidence-buffer :initarg :confidence-buffer :initform nil
                      :accessor confidence-buffer :type list
                      :documentation "A buffer to keep track of outcomes of games"))
  (:documentation "The CLEVR learning experiment"))

(defmethod initialize-instance :after ((experiment clevr-learning-experiment) &key)
  ;; set the world of the experiment
  (setf (world experiment)
        (make-instance 'clevr-world
                       :data-sets (get-configuration experiment :clevr-world-data-sets)
                       :load-questions nil))
  ;; set the questions of the experiment
  (load-questions-for-current-challenge-level experiment
                                              (get-configuration experiment :question-sample-mode))
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-clevr-learning-tutor experiment)
              (make-clevr-learning-learner experiment)))
  ;; fill the confidence buffer with zeros
  (setf (confidence-buffer experiment)
        (make-list (get-configuration experiment :evaluation-window-size)
                   :initial-element 0))
  ;; set the log file
  (set-configuration experiment :log-filename
                     (format nil "failed-games-~(~a~)-~a"
                             (id experiment) (make-random-string 5))))

(defmethod tutor ((experiment clevr-learning-experiment))
  (find 'tutor (population experiment) :key #'role))

(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'role))

(defmethod learner ((experiment clevr-learning-experiment))
  (find 'learner (population experiment) :key #'role))

(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'role))

;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------

(defmethod determine-interacting-agents ((experiment clevr-learning-experiment)
                                         interaction (mode (eql :tutor-learner)) &key)
  ;; Tutor is speaker, learner is hearer
  (setf (interacting-agents interaction) (list (tutor experiment)
                                               (learner experiment))
        (discourse-role (tutor experiment)) 'speaker
        (discourse-role (learner experiment)) 'hearer)
  (loop for agent in (list (tutor experiment) (learner experiment))
        do (setf (utterance agent) nil
                 (communicated-successfully agent) nil))
  (notify interacting-agents-determined experiment interaction))

(defmethod determine-interacting-agents ((experiment clevr-learning-experiment)
                                         interaction (mode (eql :learner-always-speaks)) &key)
  ;; Tutor is speaker, learner is hearer
  (setf (interacting-agents interaction) (list (tutor experiment)
                                               (learner experiment))
        (discourse-role (tutor experiment)) 'hearer
        (discourse-role (learner experiment)) 'speaker)
  (loop for agent in (list (tutor experiment) (learner experiment))
        do (setf (utterance agent) nil
                 (communicated-successfully agent) nil))
  (notify interacting-agents-determined experiment interaction))

(defmethod determine-interacting-agents ((experiment clevr-learning-experiment)
                                         interaction (mode (eql :default)) &key)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  (let ((agents (agents experiment)))
    (setf (interacting-agents interaction) (shuffle agents))
    (loop for a in (interacting-agents interaction)
          for d in '(speaker hearer)
          do (setf (discourse-role a) d)
          (setf (utterance a) nil)
          (setf (communicated-successfully a) nil))
    (notify interacting-agents-determined experiment interaction)))
