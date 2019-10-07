(in-package :robot-concept-learning)

;; ------------------
;; + Configurations +
;; ------------------
; :baseline - :cogent - :incremental
(define-configuration-default-value :experiment-type :baseline)

(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :category-representation :prototype)
(define-configuration-default-value :scale-world nil)
(define-configuration-default-value :learning-active t)
(define-configuration-default-value :switch-conditions-after-n-interactions nil)

(define-configuration-default-value :robot-ip "192.158.1.4")
(define-configuration-default-value :robot-port 1570)
(define-configuration-default-value :robot-vocabulary
   '("large" "huge" "small" "tiny"
     "metal" "shiny" "rubber" "matte"
     "cube" "block" "cylinder" "sphere" "ball"
     "left" "right" "front" "back"
     "yellow" "red" "orange" "gray" "blue" "green"))

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Initialize the experiment by creating the population"
  (setf (population experiment)
        (list (make-embodied-agent experiment))))       
        
;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------
(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :tutor-speaks))
                                         &key &allow-other-keys)
  "Determine the interacting agents such that the tutor is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'speaker
          (discourse-role learner) 'hearer)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :learner-speaks))
                                         &key &allow-other-keys)
  "Determine the interacting agents such that the learner is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (notify interacting-agents-determined experiment interaction)))