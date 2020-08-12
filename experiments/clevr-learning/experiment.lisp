(in-package :clevr-learning)

;; ##################
;; + Configurations +
;; ##################

(define-configuration-default-value :dot-interval 10)
(define-configuration-default-value :clevr-data-path *clevr-data-path*)
(define-configuration-default-value :data-sets '("val"))
(define-configuration-default-value :answer-file
                                    (babel-pathname :directory '("experiments" "clevr-learning")
                                                    :name "CLEVR_val_100" :type "txt"))
(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)
;; Available alignment strategies:
;; :no-alignment
;; :lateral-inhibition
(define-configuration-default-value :alignment-strategy :lateral-inhibition) 
(define-configuration-default-value :who-aligns? :learner)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.1)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)
;; Available learning strategies:
;; :keep-samples (history of scenes)
;; :keep-trash (history of failed programs)
;; :lateral-inhibition
(define-configuration-default-value :learning-strategy :keep-samples)
;; :sample-window is used for both samples and trash
(define-configuration-default-value :sample-window nil)
(define-configuration-default-value :available-primitives
                                    '(count! equal-integer less-than greater-than
                                      equal? exist filter get-context intersect
                                      query relate same union! unique))
(define-configuration-default-value :determine-interacting-agents-mode :tutor-learner)
(define-configuration-default-value :learner-speaks-after-interaction 100)

;; ##############
;; + Experiment +
;; ##############

(defclass holophrase-experiment (experiment)
  ((data :accessor data :type list :initarg :data :initform nil
         :documentation "The contents of the :answer-file"))
  (:documentation "QA Game"))

(defun make-learner-primitive-inventory (available-primitives)
  ; copy primitives from *clevr-primitives*
  ; and only keep those occurring in
  ; the available primitives configuration
  (let ((inventory (def-irl-primitives holophrase-primitives
                     :primitive-inventory *holophrase-primitives*
                     ;:irl-configurations ((:node-tests :no-duplicate-solutions
                     ;                      :no-filter-permutations
                     ;                      :restrict-nr-of-nodes)
                     ;                     (:max-nr-of-nodes . 7500))
                     )))
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
  ;; load the answer file in memory
  (setf (data experiment)
        (with-open-file (in (get-configuration experiment :answer-file)
                            :direction :input)
          (loop for line = (read-line in nil nil)
                while line
                collect (decode-json-from-string line))))
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
                                          (get-configuration experiment :available-primitives)))))
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

