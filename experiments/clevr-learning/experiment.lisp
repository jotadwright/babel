;;;; experiment.lisp

(in-package :clevr-learning)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(define-configuration-default-value :challenge-files-root
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-learning-data"))
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :challenge-1-files
                                    (make-pathname :directory '(:relative "stage-1")
                                                   :name :wild :type "txt"))
(define-configuration-default-value :challenge-2-files
                                    (make-pathname :directory '(:relative "stage-2")
                                                   :name :wild :type "txt"))
(define-configuration-default-value :challenge-3-files
                                    (make-pathname :directory '(:relative "stage-3")
                                                   :name :wild :type "txt"))
(define-configuration-default-value :questions-per-challenge 1000)
(define-configuration-default-value :clevr-world-data-sets '("val"))

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.2)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)
(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :composer-strategy :store-past-scenes)
(define-configuration-default-value :determine-interacting-agents-mode :tutor-learner)

;; Autotelic principle
(define-configuration-default-value :current-challenge-level 1)
(define-configuration-default-value :max-challenge-level 3)
(define-configuration-default-value :evaluation-window-size 100)
(define-configuration-default-value :confidence-threshold 0.95)

;; Misc
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :hide-type-hierarchy nil)

;; --------------
;; + Experiment +
;; --------------

(defclass clevr-learning-experiment (experiment)
  ((question-files :initarg :question-files :initform nil 
                   :accessor question-files :type list
                   :documentation "A list of filenames for the current challenge level")
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
  (load-questions-for-current-challenge-level experiment)
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-clevr-learning-tutor experiment)
              (make-clevr-learning-learner experiment)))
  ;; fill the confidence buffer with zeros
  (setf (confidence-buffer experiment)
        (make-list (get-configuration experiment :evaluation-window-size)
                   :initial-element 0)))

(define-event challenge-level-questions-loaded (level number))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment))
  ;; Loads N (:questions-per-challenge) random questions
  ;; of the current challenge level
  (let* ((all-challenge-files
          (directory
           (merge-pathnames
            (case (get-configuration experiment :current-challenge-level)
              (1 (get-configuration experiment :challenge-1-files))
              (2 (get-configuration experiment :challenge-2-files))
              (3 (get-configuration experiment :challenge-3-files)))
            (get-configuration experiment :challenge-files-root))))
         (challenge-files
          (let ((n (get-configuration experiment :questions-per-challenge)))
            (if n (random-elts all-challenge-files n)
              all-challenge-files))))
    (setf (question-files experiment) challenge-files)
    (notify challenge-level-questions-loaded
            (get-configuration experiment :current-challenge-level))))

(defmethod tutor ((experiment clevr-learning-experiment))
  (find 'tutor (population experiment) :key #'role))

(defmethod learner ((experiment clevr-learning-experiment))
  (find 'learner (population experiment) :key #'role))

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