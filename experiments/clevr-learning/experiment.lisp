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
(define-configuration-default-value :scenes-per-question 100)
(define-configuration-default-value :question-sample-mode :random) ; random or first
(define-configuration-default-value :clevr-world-data-sets '("val"))

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)
(define-configuration-default-value :initial-th-link-weight 0.1)

(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.2)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)
(define-configuration-default-value :th-link-incf-score 0.1)
(define-configuration-default-value :cxn-forgetting-rate 0.05)
(define-configuration-default-value :cxn-forgetting-threshold 1000)

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
  (load-questions-for-current-challenge-level
   experiment (get-configuration experiment :question-sample-mode))
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-clevr-learning-tutor experiment)
              (make-clevr-learning-learner experiment)))
  ;; fill the confidence buffer with zeros
  (setf (confidence-buffer experiment)
        (make-list (get-configuration experiment :evaluation-window-size)
                   :initial-element 0)))

(define-event challenge-level-questions-loaded (level number))

(defgeneric load-questions-for-current-challenge-level (experiment  mode &optional all-files)
  (:documentation "Load all data for the current challenge level"))

(defmethod load-questions-for-current-challenge-level :around ((experiment clevr-learning-experiment)
                                                               mode &optional all-files)
  (let ((all-challenge-files
          (sort
           (directory
            (merge-pathnames
             (case (get-configuration experiment :current-challenge-level)
               (1 (get-configuration experiment :challenge-1-files))
               (2 (get-configuration experiment :challenge-2-files))
               (3 (get-configuration experiment :challenge-3-files)))
             (get-configuration experiment :challenge-files-root)))
           #'string< :key #'namestring)))
    (format t "~%Loading data...")
    (call-next-method experiment mode all-challenge-files)
    (format t "~%Done!")
    (notify challenge-level-questions-loaded
            (get-configuration experiment :current-challenge-level))))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment)
                                                       (mode (eql :random)) &optional all-files)
  (let* ((number-of-questions
          (get-configuration experiment :questions-per-challenge))
         (scenes-per-questions
          (get-configuration experiment :scenes-per-question))
         (files
          (random-elts all-files number-of-questions))
         (data
          (loop for file in files
                for file-data = (decode-json-as-plist-from-source file)
                for scenes-and-answers = (random-elts (getf file-data :|answers|)
                                                      scenes-per-questions)
                collect (list :|question| (getf file-data :|question|)
                              :|answers| scenes-and-answers))))
    (setf (question-data experiment) data)))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment)
                                                       (mode (eql :first)) &optional all-files)
  (let* ((number-of-questions
          (get-configuration experiment :questions-per-challenge))
         (scenes-per-questions
          (get-configuration experiment :scenes-per-question))
         (files
          (subseq all-files 0 number-of-questions))
         (data
          (loop for file in files
                for file-data = (decode-json-as-plist-from-source file)
                for scenes-and-answers = (random-elts (getf file-data :|answers|)
                                                      scenes-per-questions)
                collect (list :|question| (getf file-data :|question|)
                              :|answers| scenes-and-answers))))
    (setf (question-data experiment) data)))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment)
                                                       (mode (eql :all)) &optional all-files)
  (let* ((scenes-per-questions
          (get-configuration experiment :scenes-per-question))
         (data
          (loop for file in all-files
                for file-data = (decode-json-as-plist-from-source file)
                for scenes-and-answers = (random-elts (getf file-data :|answers|)
                                                      scenes-per-questions)
                collect (list :|question| (getf file-data :|question|)
                              :|answers| scenes-and-answers))))
    (setf (question-data experiment) data)))

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
