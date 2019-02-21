(in-package :clevr-learning)

(defclass vqa-experiment (experiment)
  ()
  (:documentation "QA Game"))

(define-configuration-default-value :dot-interval 100)

(define-configuration-default-value :context-size 4)
(define-configuration-default-value :contexts-file
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "CLEVR" "CLEVR-learning-data")
                                                    :name "CLEVR_scenes_num_objects_X_per_line" :type "json")
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :questions-file
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "CLEVR" "CLEVR-learning-data" "base-zero-hop-questions")
                                                    :name "CLEVR_questions_num_objects_X_per_line_enhanced" :type "json")
                                     cl-user:*babel-corpora*))
(define-configuration-default-value :images-dir
                                    (merge-pathnames
                                     (make-pathname :directory '(:relative "CLEVR" "CLEVR-learning-data" "images" "num_objects_X"))
                                     cl-user:*babel-corpora*))

(define-configuration-default-value :initial-cxn-score 0.5)
(define-configuration-default-value :initial-chunk-score 0.5)

;; Available alignment strategies:
;; :no-alignment, :no-competitors-punished,
;; :no-form-competitors-punished,
;; :no-meaning-competitors-punished,
;; :lateral-inhibition
(define-configuration-default-value :alignment-strategy :no-alignment) 
(define-configuration-default-value :who-aligns? :learner)
(define-configuration-default-value :cxn-incf-score 0.1)
(define-configuration-default-value :cxn-decf-score 0.1)
(define-configuration-default-value :chunk-incf-score 0.1)
(define-configuration-default-value :chunk-decf-score 0.1)

;; Available learning strategies:
;; :keep-samples (history of scenes)
;; :keep-trash (history of failed programs)
(define-configuration-default-value :learning-strategy :keep-samples)
;; :sample-window is used for both samples and trash
(define-configuration-default-value :sample-window nil)

(define-configuration-default-value :available-primitives
                                    '(count! equal-integer less-than greater-than
                                      equal? exist filter get-context intersect
                                      query relate same union! unique))
(define-configuration-default-value :determine-interacting-agents-mode :tutor-learner)
(define-configuration-default-value :learner-speaks-after-interaction 1000)


(defun set-file-paths (experiment)
  "Set the paths for contexts-file, questions-file and images-dir depending
   on the context size of the experiment"
  (let ((context-size (get-configuration experiment :context-size)))
    (set-configuration experiment :contexts-file
                       (parse-namestring (string-replace (namestring (get-configuration experiment :contexts-file))
                                                         "X" (mkstr context-size)))
                       :replace t)
    (set-configuration experiment :questions-file
                       (parse-namestring (string-replace (namestring (get-configuration experiment :questions-file))
                                                         "X" (mkstr context-size)))
                       :replace t)
    (set-configuration experiment :images-dir
                       (parse-namestring (string-replace (namestring (get-configuration experiment :images-dir))
                                                         "X" (mkstr context-size)))
                       :replace t)))

(defmethod initialize-instance :after ((experiment vqa-experiment) &key)
  "Create the world and the population of the experiment"
  (set-file-paths experiment)
  (setf (world experiment)
        (make-vqa-world (get-configuration experiment :contexts-file)
                        (get-configuration experiment :questions-file)))
  (setf (population experiment)
        (list (make-instance 'vqa-agent :id 'tutor :experiment experiment
                             :grammar *clevr* :ontology *clevr-ontology*
                             :primitives (get-configuration experiment :available-primitives))
              (make-instance 'vqa-agent :id 'learner :experiment experiment
                             :grammar (make-agent-cxn-set) :ontology (copy-object *clevr-ontology*)
                             :primitives (get-configuration experiment :available-primitives))))
  (activate-monitor print-a-dot-for-each-interaction))

(defmethod determine-interacting-agents ((experiment vqa-experiment)
                                         interaction
                                         (mode (eql :default))
                                         &key)
  "Tutor and learner are chosen at random, depending on the configuration
   :learner-speaks-after-interaction"
  (let ((threshold (get-configuration experiment :learner-speaks-after-interaction)))
    (if (> (interaction-number interaction) threshold)
      (determine-interacting-agents experiment interaction nil)
      (determine-interacting-agents experiment interaction :tutor-learner))))

(defmethod determine-interacting-agents ((experiment vqa-experiment)
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

(defmethod determine-interacting-agents ((experiment vqa-experiment)
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

