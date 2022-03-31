(in-package :spatial-concepts)

;; ------------------
;; + Configurations +
;; ------------------

;; :simulated - :extracted
(define-configuration-default-value :world-type :simulated)

(define-configuration-default-value :dot-interval 100)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :lexical-variation nil)
(define-configuration-default-value :export-lexicon-interval 500)
(define-configuration-default-value :switch-conditions-after-n-interactions nil)
(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :alignment-filter :all) ; :none - :at-least-one - :all

;; --------------
;; + Experiment +
;; --------------
(defclass spatial-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defmethod initialize-instance :after ((experiment spatial-experiment) &key)
  "Create the population and load the scenes from file"
  (activate-monitor print-a-dot-for-each-interaction)
  ;; set the population
  (setf (population experiment)
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  ;; reset *clevr-data-path* to root directory of clevr data
  ;; this will be used to load the 'clevr-world below
  (reset-clevr-data-path)
  ;; set the world (it uses the global variable *clevr-data-path*)
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets (list "val")))
  ;; store the data-sets and data-path in the blackboard
  (set-data experiment :ns-vqa-data-path (merge-pathnames (make-pathname
                                                           :directory `(:relative  "CLEVR-v1.0" "scenes" "val")) cl-user:*babel-corpora*)))


(defmethod learner ((experiment spatial-experiment))
  (find 'learner (population experiment) :key #'id))

(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'id))

(defmethod tutor ((experiment spatial-experiment))
  (find 'tutor (population experiment) :key #'id))

(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'id))               
        

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------

(defmethod determine-interacting-agents ((experiment spatial-experiment)
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


(defmethod determine-interacting-agents ((experiment spatial-experiment)
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

