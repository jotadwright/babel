(in-package :clevr-learning)

;; ##################
;; + Configurations +
;; ##################

(define-configuration-default-value :dot-interval 10)
(define-configuration-default-value :clevr-data-path *clevr-data-path*)
(define-configuration-default-value :data-sets '("val"))
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)
(define-configuration-default-value :alignment-strategy :lateral-inhibition) 
(define-configuration-default-value :who-aligns? :learner)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.1)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)
(define-configuration-default-value :learning-strategy :keep-samples)
(define-configuration-default-value :sample-window nil)
(define-configuration-default-value :determine-interacting-agents-mode :tutor-learner)
(define-configuration-default-value :learner-speaks-after-interaction 100)
;; How often can the agent try to find the ground truth program for an utterance?
(define-configuration-default-value :max-attempts-per-utterance 50)
;; When the ground truth utterance is found, write to this file
(define-configuration-default-value :output-file
   (babel-pathname :directory '("experiments" "clevr-learning")
                   :name "successful-utterances" :type "txt"))
;; Timeout for the composer to find a solution
(define-configuration-default-value :composer-timeout 900)
;; Learning stage controls where the data will be loaded
;; and which primitives are available
(define-configuration-default-value :learning-stage 1)  ; 1, 2 or 3
(define-configuration-default-value :data-dir
   (merge-pathnames (make-pathname :directory '(:relative "clevr-learning-data"))
                    cl-user:*babel-corpora*))
;; Control whether or not to provide the bind statements
;; to the composer
(define-configuration-default-value :provide-bind-statements nil)

;; ##############
;; + Experiment +
;; ##############

(defclass holophrase-experiment (experiment)
  ((questions :accessor questions :type list :initarg :questions :initform nil
              :documentation "The contents of the questions and answers file"))
  (:documentation "QA Game"))

(defparameter *learning-stage-primitives*
  '((1 count! exist filter get-context query unique)
    (2 count! exist filter get-context query unique
       relate same)
    (3 count! exist filter get-context query unique
       relate same equal? intersect union!
       equal-integer less-than greater-than)))
       

(defun make-learner-primitive-inventory (learning-stage)
  ; copy primitives from *clevr-primitives*
  ; and only keep those occurring in
  ; the available primitives configuration
  (let ((inventory (def-irl-primitives holophrase-primitives
                     :primitive-inventory *holophrase-primitives*))
        (available-primitives
         (rest (assoc learning-stage *learning-stage-primitives*))))
    (loop for p in available-primitives
          do (add-primitive
              (find-primitive p *clevr-primitives*)
              inventory))
    inventory))

(defmethod initialize-instance :after ((experiment holophrase-experiment) &key)
  "Create the world and the population of the experiment"
  ;; set the clevr data path
  (setf clevr-world:*clevr-data-path*
        (get-configuration experiment :clevr-data-path))
  ;; create the clevr world
  (setf (world experiment)
        (make-instance 'clevr-world
                       :data-sets (get-configuration experiment :data-sets)
                       :load-questions nil))
  ;; load the question files, depending on the learning stage
  (let* ((learning-stage (get-configuration experiment :learning-stage))
         (data-dir
          (merge-pathnames
           (make-pathname :directory `(:relative ,(format nil "stage-~a" learning-stage)))
           (get-configuration experiment :data-dir))))                                       
    (setf (questions experiment)
          (shuffle
           (directory
            (merge-pathnames
             (make-pathname :name :wild :type "txt")
             data-dir)))))
  ;; create the agents
  (setf (population experiment)
        (list (make-instance 'holophrase-tutor
                             :role 'tutor
                             :experiment experiment
                             :grammar (let ((clevr-copy (copy-object *CLEVR*)))
                                        (set-configurations
                                         clevr-copy
                                         '((:cxn-supplier-mode . :hashed-simple-queue)
                                           (:priority-mode . :priming))
                                         :replace t)
                                        (set-configurations
                                         (processing-cxn-inventory clevr-copy)
                                         '((:cxn-supplier-mode . :hashed-simple-queue)
                                           (:priority-mode . :priming))
                                         :replace t)
                                        clevr-copy)
                             :ontology (copy-object *clevr-ontology*)
                             :primitives *clevr-primitives*)
              (make-instance 'holophrase-learner
                             :role 'learner
                             :experiment experiment
                             :grammar (make-agent-cxn-set)
                             :ontology (copy-object *clevr-ontology*)
                             :primitives (make-learner-primitive-inventory
                                          (get-configuration experiment :learning-stage)))))
  ;; reset the utterance index and attempts per utterance
  (set-data experiment :current-utterance-index 0)
  (set-data experiment :attempts-per-utterance nil)
  ;; clear the output file
  (with-open-file (stream (get-configuration experiment :output-file)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (declare (ignorable stream)))
  ;; print dots
  (activate-monitor print-a-dot-for-each-interaction))

;; ################################
;; + Determine Interacting Agents +
;; ################################

(defmethod determine-interacting-agents ((experiment holophrase-experiment)
                                         interaction
                                         (mode (eql :default))
                                         &key)
  "Tutor and learner are chosen at random, depending on the configuration
   :learner-speaks-after-interaction"
  (let ((threshold (get-configuration experiment :learner-speaks-after-interaction)))
    (if (> (interaction-number interaction) threshold)
      (determine-interacting-agents experiment interaction nil)
      (determine-interacting-agents experiment interaction :tutor-learner))))

(defmethod determine-interacting-agents ((experiment holophrase-experiment)
                                         interaction
                                         (mode (eql :tutor-learner))
                                         &key)
  "Tutor and learner are always speaker and hearer, respectively"
  (let ((tutor (find 'tutor (population experiment) :key #'role))
        (learner (find 'learner (population experiment) :key #'role)))
    (setf (interacting-agents interaction)
          (list tutor learner)
          (discourse-role tutor) 'speaker
          (discourse-role learner) 'hearer)
    (loop for agent in (list tutor learner)
          do (setf (utterance agent) nil
                   (communicated-successfully agent) nil))
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents ((experiment holophrase-experiment)
                                         interaction
                                         (mode (eql :learner-speaks))
                                         &key)
  "The learner is always the speaker (used for debugging)"
  (let ((tutor (find 'tutor (population experiment) :key #'role))
        (learner (find 'learner (population experiment) :key #'role)))
    (setf (interacting-agents interaction)
          (list tutor learner)
          (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (loop for agent in (list tutor learner)
          do (setf (utterance agent) nil
                   (communicated-successfully agent) nil))
    (notify interacting-agents-determined experiment interaction)))

