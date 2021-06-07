(in-package :clevr-grammar-learning)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data
(define-configuration-default-value :challenge-files-root
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :challenge-1-data
                                    (make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "txt"))
(define-configuration-default-value :challenge-2-data
                                    (make-pathname :directory '(:relative "train")
                                                   :name "stage-2" :type "txt"))
(define-configuration-default-value :challenge-3-data
                                    (make-pathname :directory '(:relative "train")
                                                   :name "stage-3" :type "txt"))
(define-configuration-default-value :challenge-1-data-evaluation
                                    (make-pathname :directory '(:relative "val")
                                                   :name "stage-1" :type "txt"))
(define-configuration-default-value :challenge-2-data-evaluation
                                    (make-pathname :directory '(:relative "val")
                                                   :name "stage-2" :type "txt"))
(define-configuration-default-value :challenge-3-data-evaluation
                                    (make-pathname :directory '(:relative "val")
                                                   :name "stage-3" :type "txt"))

(define-configuration-default-value :observation-sample-mode :random) ; random, sequential or evaluation

;; Strategies and scores
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-th-link-weight 0.1)

(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.3)

(define-configuration-default-value :evaluation-grammar nil)
(define-configuration-default-value :alignment-strategy :lateral-inhibition)
(define-configuration-default-value :remove-cxn-on-lower-bound nil)

(define-configuration-default-value :determine-interacting-agents-mode :corpus-learner)
(define-configuration-default-value :learner-cxn-supplier :hashed-and-scored)
(define-configuration-default-value :learner-th-connected-mode :neighbours)

;; Autotelic principle
(define-configuration-default-value :enable-autotelic-levels nil)
(define-configuration-default-value :current-challenge-level 1)
(define-configuration-default-value :max-challenge-level 3)
(define-configuration-default-value :evaluation-window-size 1000)
(define-configuration-default-value :confidence-threshold 1.00)
(define-configuration-default-value :learner-speaks-confidence-threshold 0.5)

;; Misc
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :result-display-interval 100)
(define-configuration-default-value :hide-type-hierarchy nil)

;; --------------
;; + Experiment +
;; --------------

(defclass clevr-grammar-learning-experiment (experiment)
  ((question-data :initarg :question-data :initform nil 
                   :accessor question-data :type list
                   :documentation "A list of samples for the current challenge level")
   (confidence-buffer :initarg :confidence-buffer :initform nil
                      :accessor confidence-buffer :type list
                      :documentation "A buffer to keep track of outcomes of games")
   (success-buffer :initarg :success-buffer :initform nil
                      :accessor success-buffer :type list
                      :documentation "A buffer to keep track of communicative success")
   (consistency-buffer :initarg :consistency-buffer :initform nil
                      :accessor consistency-buffer :type list
                      :documentation "A buffer to keep track of learning consistency, interaction number - grammar size - successful interactions should be 0")
   )
  (:documentation "The CLEVR learning experiment"))

(defmethod initialize-instance :after ((experiment clevr-grammar-learning-experiment) &key)
  ;; set the questions of the experiment
  (load-questions-for-current-challenge-level experiment (get-configuration experiment :observation-sample-mode))
  ;; set the population of the experiment
  (setf (population experiment)
        (list (make-clevr-learning-learner experiment)))
  ;; set configurations for evaluation
  (when (get-configuration experiment :evaluation-grammar)
    (setf (grammar (first (agents experiment))) (get-configuration experiment :evaluation-grammar))
    (set-configuration (grammar (first (agents experiment))) :update-th-links nil)
    (set-configuration (grammar (first (agents experiment))) :use-meta-layer nil)
    (set-configuration (grammar (first (agents experiment))) :consolidate-repairs nil))
  
  ;; fill the confidence buffer with zeros
  (setf (confidence-buffer experiment)
        (make-list (get-configuration experiment :evaluation-window-size)
                   :initial-element 0)))

(define-event challenge-level-questions-loaded (level number))

(defgeneric load-questions-for-current-challenge-level (experiment mode)
  (:documentation "Load all data for the current challenge level"))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-grammar-learning-experiment)
                                                       (mode (eql :random)))
  (format t "~%Loading data...")
  (let* ((challenge-file (merge-pathnames
                          (case (get-configuration experiment :current-challenge-level)
                            (1 (get-configuration experiment :challenge-1-data))
                            (2 (get-configuration experiment :challenge-2-data))
                            (3 (get-configuration experiment :challenge-3-data)))
                          (get-configuration experiment :challenge-files-root))))
    
    (with-open-file (stream challenge-file)
      (let* ((stage-data (shuffle (loop for line = (read-line stream nil)
                                        for data = (when line (cl-json:decode-json-from-string line))
                                        while data
                                        collect (cons (cdr (assoc :question data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal))))))
        (setf (question-data experiment) stage-data)))
    (format t "~%Done!")
    (notify challenge-level-questions-loaded
            (get-configuration experiment :current-challenge-level))))


(defmethod load-questions-for-current-challenge-level ((experiment clevr-grammar-learning-experiment)
                                                       (mode (eql :sequential)))
  (format t "~%Loading data...")
  (let* ((challenge-file (merge-pathnames
                          (case (get-configuration experiment :current-challenge-level)
                            (1 (get-configuration experiment :challenge-1-data))
                            (2 (get-configuration experiment :challenge-2-data))
                            (3 (get-configuration experiment :challenge-3-data)))
                          (get-configuration experiment :challenge-files-root))))
    
    (with-open-file (stream challenge-file)
      (let* ((stage-data (loop for line = (read-line stream nil)
                                        for data = (when line (cl-json:decode-json-from-string line))
                                        while data
                                        collect (cons (cdr (assoc :question data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal)))))
        (setf (question-data experiment) stage-data)))
    (format t "~%Done!")
    (notify challenge-level-questions-loaded
            (get-configuration experiment :current-challenge-level))))

(defmethod load-questions-for-current-challenge-level ((experiment clevr-grammar-learning-experiment)
                                                       (mode (eql :evaluation)))
  (format t "~%Loading evaluation data...")
  (let* ((challenge-file (merge-pathnames
                          (case (get-configuration experiment :current-challenge-level)
                            (1 (get-configuration experiment :challenge-1-data-evaluation))
                            (2 (get-configuration experiment :challenge-2-data-evaluation))
                            (3 (get-configuration experiment :challenge-3-data-evaluation)))
                          (get-configuration experiment :challenge-files-root))))
    
    (with-open-file (stream challenge-file)
      (let* ((stage-data (shuffle (loop for line = (read-line stream nil)
                                        for data = (when line (cl-json:decode-json-from-string line))
                                        while data
                                        collect (cons (cdr (assoc :question data)) (remove-duplicates (read-from-string (cdr (assoc :meaning data))) :test #'equal))))))
        (setf (question-data experiment) stage-data)))
    (format t "~%Done!")
    (notify challenge-level-questions-loaded
            (get-configuration experiment :current-challenge-level))))

(defmethod tutor ((experiment clevr-grammar-learning-experiment))
  (find 'tutor (population experiment) :key #'role))

(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'role))

(defmethod learner ((experiment clevr-grammar-learning-experiment))
  (find 'learner (population experiment) :key #'role))

(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'role))


;; ---------------------------
;; + Interacting Agents Mode +
;; ---------------------------

(defmethod determine-interacting-agents ((experiment clevr-grammar-learning-experiment)
                                         interaction (mode (eql :corpus-learner)) &key)
  ;; Tutor is speaker, learner is hearer
  (setf (interacting-agents interaction) (list (learner experiment))
        (discourse-role (learner experiment)) 'hearer)
  (setf (utterance (learner experiment)) nil
        (communicated-successfully (learner experiment)) nil)
  ;(notify interacting-agents-determined experiment interaction)
  )