(in-package :mwm)

;; ------------------
;; + Configurations +
;; ------------------
(define-configuration-default-value :dot-interval 100)

;; when :data-source is :clevr, load the clevr dataset(s) specified
;: using :data-sets. Otherwise, load the continuous clevr data
;; specified using :data-path
(define-configuration-default-value :data-source :clevr)
(define-configuration-default-value :data-sets (list "val"))
(define-configuration-default-value :data-path
   "/Users/jensnevens/SVN/Babel2-corpora/CLEVR/CLEVR-v1.0/scenes/cval/")

(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :category-representation :prototype)
(define-configuration-default-value :noise-amount nil) 
(define-configuration-default-value :noise-prob nil) 
(define-configuration-default-value :scale-world t)
(define-configuration-default-value :max-tutor-utterance-length 1) 
(define-configuration-default-value :lexical-variation nil)
(define-configuration-default-value :tutor-lexicon :continuous)
(define-configuration-default-value :game-mode :tutor-learner)
(define-configuration-default-value :tutor-re-entrance nil)
(define-configuration-default-value :export-lexicon-interval 500)

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defparameter *world* nil)

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Create the population and load the scenes from file"
  (activate-monitor print-a-dot-for-each-interaction)
  (setf (population experiment)
        (case (get-configuration experiment :game-mode)
          (:tutor-learner (list (make-tutor-agent experiment)
                                (make-learner-agent experiment)))
          (:tutor-tutor (list (make-tutor-agent experiment)
                              (make-tutor-agent experiment)))))
  (unless *world*
    (setf *world*
          (make-instance 'clevr-world :data-sets
                         (get-configuration experiment :data-sets))))
  (setf (world experiment) *world*))

(defmethod learner ((experiment mwm-experiment))
  (find 'learner (population experiment) :key #'id))
(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'id))

(defmethod tutor ((experiment mwm-experiment))
  (find 'tutor (population experiment) :key #'id))
(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'id))

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------
(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :tutor-speaks))
                                         &key &allow-other-keys)
  "The tutor is always the speaker"
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
  "The learner is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (notify interacting-agents-determined experiment interaction)))