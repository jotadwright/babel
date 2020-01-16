(in-package :mwm)

;; ------------------
;; + Configurations +
;; ------------------
(define-configuration-default-value :dot-interval 100)
; :baseline - :cogent - :incremental
(define-configuration-default-value :experiment-type :baseline)
; :simulated - :extracted
(define-configuration-default-value :data-type :simulated)
; data-sets for clevr-world class
(define-configuration-default-value :data-sets (list "val"))
; data-path for additional data
(define-configuration-default-value :data-path
   (merge-pathnames (make-pathname :directory '(:relative "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                    cl-user:*babel-corpora*))

(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :category-representation :prototype)
(define-configuration-default-value :scale-world t)
(define-configuration-default-value :max-tutor-utterance-length 1)
(define-configuration-default-value :lexical-variation nil)
(define-configuration-default-value :export-lexicon-interval 500)
(define-configuration-default-value :learning-active t)
(define-configuration-default-value :switch-conditions-after-n-interactions nil)
(define-configuration-default-value :max-z-score 2)
(define-configuration-default-value :extracted-colour-space :hsv) ; :hsv :rgb :lab
(define-configuration-default-value :training-period 1000)

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defparameter *baseline-clevr-data-path* *clevr-data-path*)
(defparameter *cogent-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT"))
                   cl-user:*babel-corpora*))
(defparameter *incremental-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-incremental"))
                   cl-user:*babel-corpora*))

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Create the population and load the scenes from file"
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor export-lexicon-evolution)
  ;; set the population
  (setf (population experiment)
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  ;; set the clevr-data-path
  (setf *clevr-data-path*
        (case (get-configuration experiment :experiment-type)
          (:baseline *baseline-clevr-data-path*)
          (:cogent *cogent-clevr-data-path*)
          (:incremental *incremental-clevr-data-path*)))
  ;; set the world
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets (get-configuration experiment :data-sets)))
  ;; store the data-sets and data-path in the blackboard
  (set-data experiment :data-sets (get-configuration experiment :data-sets))
  (set-data experiment :data-path (get-configuration experiment :data-path))
  ;; set the max z-score (too cumbersome with configuration)
  (setf *max-z-score* (get-configuration experiment :max-z-score)))

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

(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :learner-speaks-after-training-period))
                                         &key &allow-other-keys)
  "The learner can speak after a certaint training period"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (if (> (interaction-number interaction) (get-configuration experiment :training-period))
      (let ((roles (shuffle '(speaker hearer))))
        (setf (discourse-role tutor) (first roles)
              (discourse-role learner) (second roles)))
      (setf (discourse-role tutor) 'speaker
            (discourse-role learner) 'hearer))
    (notify interacting-agents-determined experiment interaction)))