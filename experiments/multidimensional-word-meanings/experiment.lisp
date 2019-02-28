(in-package :mwm)

;; ------------------
;; + Configurations +
;; ------------------
(define-configuration-default-value :contexts (merge-pathnames
                                               (make-pathname :directory '(:relative "CLEVR" "CLEVR-learning-data")
                                                              :name "CLEVR_scenes_num_objects_4_per_line" :type "json")
                                               cl-user:*babel-corpora*))
(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :attributes '(x-pos y-pos area width height wh-ratio
                                                  mean-rgb rgb-variance nr-of-sides nr-of-corners))
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf 0.2)
(define-configuration-default-value :alpha 0.05)

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Create the population and load the scenes from file"
  (setf (population experiment)
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  (setf (world experiment)
        (read-contexts-from-file (get-configuration experiment :contexts))))

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
  "The tutor is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (notify interacting-agents-determined experiment interaction)))