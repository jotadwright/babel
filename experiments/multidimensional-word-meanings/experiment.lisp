(in-package :mwm)

;; ------------------
;; + Configurations +
;; ------------------
(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :contexts (list
                                               (merge-pathnames
                                                (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                                               :name "CLEVR_val_full_per_line" :type "json")
                                                cl-user:*babel-corpora*)
                                               (merge-pathnames
                                                (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes")
                                                               :name "CLEVR_train_full_per_line" :type "json")
                                                cl-user:*babel-corpora*)))
(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks) ; :tutor-speaks :learner-speaks :default
(define-configuration-default-value :attributes '(x-pos y-pos
                                                  area wh-ratio
                                                  R G B roughness
                                                  nr-of-sides nr-of-corners))
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :alpha 0.05)
(define-configuration-default-value :max-tutor-utterance-length 1) ; nil or int
(define-configuration-default-value :shift-prototype :always) ; :on-success :on-failure :always
(define-configuration-default-value :update-certainty t) ; t or nil
(define-configuration-default-value :remove-on-lower-bound t) ; t or nil
(define-configuration-default-value :category-representation :min-max) ; :min-max :prototype :prototype-min-max
(define-configuration-default-value :feature-selection :all) ; :all or :sampling
(define-configuration-default-value :noise-amount nil) ; nil or float in [0,1]
(define-configuration-default-value :noise-prob nil) ; nil or float in [0,1]
(define-configuration-default-value :scale-world nil)  ; t or nil

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
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  (warn "Loading large data files into memory. This could take a few seconds...")
  (unless *world*
    (setf *world*
          (loop for file in (get-configuration experiment :contexts)
                for scenes = (read-contexts-from-file file)
                append scenes)))
  (setf (world experiment) *world*)
  (warn "Done loading!"))

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