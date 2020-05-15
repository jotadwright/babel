(in-package :clevr-learning)

;; ##################
;; + Configurations +
;; ##################

(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :clevr-data-path *clevr-data-path*)
(define-configuration-default-value :data-sets '("val"))
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
;; :lateral-inhibiyion
(define-configuration-default-value :learning-strategy :keep-samples)
;; :sample-window is used for both samples and trash
(define-configuration-default-value :sample-window nil)
(define-configuration-default-value :available-primitives
                                    '(count! equal-integer less-than greater-than
                                      equal? exist filter get-context intersect
                                      query relate same union! unique))
(define-configuration-default-value :determine-interacting-agents-mode :tutor-learner)
(define-configuration-default-value :learner-speaks-after-interaction 1000)

;; ##############
;; + Experiment +
;; ##############

(defclass holophrase-experiment (experiment)
  ()
  (:documentation "QA Game"))

(defun make-primitive-inventory (available-primitives)
  ; copy primitives from *clevr-primitives*
  (let ((inventory (def-irl-primitives :primitive-inventory *holophrase-primitives*)))
    (loop for p in available-primitives
          do (add-primitive
              (find-primitive p *clevr-primitives*)
              inventory))
    inventory))

(defmethod initialize-instance :after ((experiment holophrase-experiment) &key)
  "Create the world and the population of the experiment"
  (setf clevr-world:*clevr-data-path* (get-configuration experiment :clevr-data-path))
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets (get-configuration experiment :data-sets)
                       :load-questions t))
  (setf (population experiment)
        (list (make-instance 'holophrase-agent :id 'tutor :experiment experiment
                             :grammar *clevr* :ontology (copy-object *clevr-ontology*)
                             :primitives (make-primitive-inventory (get-configuration experiment :available-primitives))
              (make-instance 'holophrase-agent :id 'learner :experiment experiment
                             :grammar (make-agent-cxn-set) :ontology (copy-object *clevr-ontology*)
                             :primitives (make-primitive-inventory (get-configuration experiment :available-primitives)))))
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
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction) (list tutor learner))
    (setf (discourse-role tutor) 'speaker)
    (setf (discourse-role learner) 'hearer)
    (loop for agent in (list tutor learner)
          do (setf (utterance agent) nil)
          do (setf (communicated-successfully agent) nil))
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents ((experiment holophrase-experiment)
                                         interaction
                                         (mode (eql :learner-speaks))
                                         &key)
  "The learner is always the speaker (used for debugging)"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction) (list tutor learner)
          (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (loop for agent in (list tutor learner)
          do (setf (utterance agent) nil
                   (communicated-successfully agent) nil))
    (notify interacting-agents-determined experiment interaction)))

