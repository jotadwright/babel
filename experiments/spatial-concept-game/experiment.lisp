(in-package :spatial-concepts)

;; ------------------
;; + Configurations +
;; ------------------

;; :baseline - :cogent - :incremental - :compositional
(define-configuration-default-value :experiment-type :baseline)

;; :simulated - :extracted
(define-configuration-default-value :world-type :simulated)

;; :A or :B
(define-configuration-default-value :cogent-stage :A)

;; 1, 2, 3, 4 or 5
(define-configuration-default-value :incremental-stage 1)

(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :lexical-variation nil)
(define-configuration-default-value :export-lexicon-interval 500)
(define-configuration-default-value :switch-conditions-after-n-interactions nil)
(define-configuration-default-value :alignment-filter :all) ; :none - :at-least-one - :all

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))



;; path to each variant of the CLEVR dataset
;; every variant should have a subdirectory 'scenes'
(defparameter *baseline-clevr-data-path*
  *clevr-data-path*)

(defparameter *cogent-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT"))
                   cl-user:*babel-corpora*))

(defparameter *incremental-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "Frontiers-data" "CLEVR-incremental"))
                   cl-user:*babel-corpora*))



(defun initial-data-set (experiment)
  (case (get-configuration experiment :experiment-type)
    (:baseline "val")
    (:cogent "valA")
    (:incremental "phase_1")
    (:compositional "val")))


(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Create the population and load the scenes from file"
  (activate-monitor print-a-dot-for-each-interaction)
  ;; set the population
  (setf (population experiment)
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  ;; set the clevr-data-path
  ;; this will be used to load the 'clevr-world below
  (setf *clevr-data-path*
        (case (get-configuration experiment :experiment-type)
          (:baseline *baseline-clevr-data-path*)
          (:cogent *cogent-clevr-data-path*)
          (:incremental *incremental-clevr-data-path*)
          (:compositional *baseline-clevr-data-path*)))
  ;; set the world (it uses the global variable *clevr-data-path*)
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets (list (initial-data-set experiment))))
  ;; store the data-sets and data-path in the blackboard
  (set-data experiment :ns-vqa-data-path
            (case (get-configuration experiment :experiment-type)
              (:baseline
               (merge-pathnames
                (make-pathname
                 :directory `(:relative "Frontiers-data" "CLEVR"
                              ,(initial-data-set experiment)))
                cl-user:*babel-corpora*))
              (:cogent
               (merge-pathnames
                (make-pathname
                 :directory `(:relative "Frontiers-data" "CoGenT"
                              ,(initial-data-set experiment)))
                cl-user:*babel-corpora*))
              (:incremental
               (merge-pathnames
                (make-pathname
                 :directory `(:relative "Frontiers-data" "incremental"
                              ,(initial-data-set experiment)))
                cl-user:*babel-corpora*))
              (:compositional
               (merge-pathnames
                (make-pathname
                 :directory `(:relative "Frontiers-data" "CLEVR"
                              ,(initial-data-set experiment)))
                cl-user:*babel-corpora*)))))


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
