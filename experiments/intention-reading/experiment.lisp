;;;; experiment.lisp

(in-package :clevr-learning)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(define-configuration-default-value :challenge-files-root
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-learning-data" "val"))
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :challenge-1-files
                                    (make-pathname :directory '(:relative "stage-1")
                                                   :name :wild :type "lisp"))
(define-configuration-default-value :challenge-2-files
                                    (make-pathname :directory '(:relative "stage-2")
                                                   :name :wild :type "lisp"))
(define-configuration-default-value :challenge-3-files
                                    (make-pathname :directory '(:relative "stage-3")
                                                   :name :wild :type "lisp"))
(define-configuration-default-value :questions-per-challenge 5000)
(define-configuration-default-value :scenes-per-question 20)
(define-configuration-default-value :question-sample-mode :first) ; random or first or all
(define-configuration-default-value :clevr-world-data-sets '("val"))

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)
(define-configuration-default-value :initial-th-link-weight 0.1)

(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.4)
(define-configuration-default-value :cxn-inhibit-score 0.1)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)

(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :determine-interacting-agents-mode :default)
(define-configuration-default-value :tutor-sample-mode :smart) ; :random or :smart
(define-configuration-default-value :learner-cxn-supplier :hashed-and-scored)
(define-configuration-default-value :composer-strategy :store-past-scenes)
(define-configuration-default-value :composer-past-scenes-window 100)
(define-configuration-default-value :remove-cxn-on-lower-bound t)
(define-configuration-default-value :composer-force-shape-category nil)
(define-configuration-default-value :th-link-repair-mode-comprehension :no-path-required)
(define-configuration-default-value :th-link-repair-mode-formulation :path-required)

;; Autotelic principle
(define-configuration-default-value :current-challenge-level 1)
(define-configuration-default-value :max-challenge-level 3)
(define-configuration-default-value :evaluation-window-size 1000)
(define-configuration-default-value :confidence-threshold 1.00)

;; Hybrid or symbolic primitives
(define-configuration-default-value :primitives :symbolic) ; :symbolic or hybrid
(define-configuration-default-value :hybrid-server-address "http://localhost")
(define-configuration-default-value :hybrid-server-port 8888)
(define-configuration-default-value :hybrid-check-past-scenes-errors-allowed 1)

;; Misc
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :hide-type-hierarchy t)

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
                   :initial-element 0))
  ;; set the log file
  (set-configuration experiment :log-filename
                     (format nil "failed-games-~(~a~)-~a"
                             (id experiment) (make-random-string 5))))

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
    (when (null all-challenge-files)
      (warn "~%~%No data found. You probably specified the wrong path...~%~%"))
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
                for file-data = (with-open-file (stream file :direction :input)
                                  (read stream))
                for count-question-p = (find 'count! (second file-data) :key #'first)
                for available-scenes-and-answers
                = (if count-question-p
                    (find-all-if-not #'(lambda (scene-answer-cons)
                                         (= 0 (cdr scene-answer-cons)))
                                     (third file-data))
                    (third file-data))
                for sampled-scenes-and-answers
                = (if (> (length available-scenes-and-answers) scenes-per-questions)
                    (random-elts available-scenes-and-answers scenes-per-questions)
                    available-scenes-and-answers)
                collect (cons (first file-data) sampled-scenes-and-answers))))
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
                for file-data = (with-open-file (stream file :direction :input)
                                  (read stream))
                for count-question-p = (find 'count! (second file-data) :key #'first)
                for available-scenes-and-answers
                = (if count-question-p
                    (find-all-if-not #'(lambda (scene-answer-cons)
                                         (= 0 (cdr scene-answer-cons)))
                                     (third file-data))
                    (third file-data))
                for sampled-scenes-and-answers
                = (if (> (length available-scenes-and-answers) scenes-per-questions)
                    (random-elts available-scenes-and-answers scenes-per-questions)
                    available-scenes-and-answers)
                collect (cons (first file-data) sampled-scenes-and-answers))))
    (setf (question-data experiment) data)))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-learning-experiment)
                                                       (mode (eql :all)) &optional all-files)
  (let* ((scenes-per-questions
          (get-configuration experiment :scenes-per-question))
         (data
          (loop for file in all-files
                for file-data = (with-open-file (stream file :direction :input)
                                  (read stream))
                for count-question-p = (find 'count! (second file-data) :key #'first)
                for available-scenes-and-answers
                = (if count-question-p
                    (find-all-if-not #'(lambda (scene-answer-cons)
                                         (= 0 (cdr scene-answer-cons)))
                                     (third file-data))
                    (third file-data))
                for sampled-scenes-and-answers
                = (if (> (length available-scenes-and-answers) scenes-per-questions)
                    (random-elts available-scenes-and-answers scenes-per-questions)
                    available-scenes-and-answers)
                collect (cons (first file-data) sampled-scenes-and-answers))))
    (setf (question-data experiment) data)))

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
