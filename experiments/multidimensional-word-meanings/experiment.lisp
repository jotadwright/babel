(in-package :mwm)


;;;; Default Configuration Values

(define-configuration-default-value :determine-interacting-agents-mode :tutor-always-speaker)
(define-configuration-default-value :game-mode :tt) ; :tt, :tl or :p
(define-configuration-default-value :population-size 10)
(define-configuration-default-value :min-context-size 2)
(define-configuration-default-value :max-context-size 5)
(define-configuration-default-value :channels
                                    '(:x-pos :y-pos :width :height :wh-ratio :area
                                      :nr-of-sides :nr-of-corners :mean-color :stdev-color))
(define-configuration-default-value :conceptualisation-strategy :nearest-neighbour) ; :nearest-neighbour or :discrimination

;;;; Experiment
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defmethod initialize-population ((experiment mwm-experiment) (mode (eql :tt)))
  (let ((agents (loop repeat 2 collect (make-instance 'mwm-agent :experiment experiment))))
    (mapcar #'build-tutor-ontology-and-lexicon agents)))

(defmethod initialize-population ((experiment mwm-experiment) (mode (eql :tl)))
  (let ((agents (loop repeat 2 collect (make-instance 'mwm-agent :experiment experiment))))
    (build-tutor-ontology-and-lexicon (first agents))
    (setf (id (first agents)) 'tutor
          (id (second agents)) 'learner)
    agents))

(defmethod initialize-population ((experiment mwm-experiment) (mode (eql :p)))
  (let ((population-size (get-configuration experiment :population-size)))
    (loop repeat population-size
          collect (make-instance 'mwm-agent :experiment experiment))))

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  (setf (population experiment)
        (initialize-population experiment (get-configuration experiment :game-mode))))


;;;; Determine interacting agents
(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         (mode (eql :tutor-always-speaker))
                                         &key)
  (assert (eql (get-configuration experiment :game-mode) :tl))
  (setf (interacting-agents interaction) (population experiment))
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (discourse-role tutor) 'speaker
          (discourse-role learner) 'hearer))
  (loop for a in (interacting-agents interaction)
        do (setf (utterance a) nil
                 (communicated-successfully a) nil))
  (notify interacting-agents-determined experiment interaction))