(in-package :clevr-learning)

(defun initialize-agent (agent &key context question)
  "Prepare the agent for the interaction"
  (setf (question-object agent) question
        (utterance agent) (question question)
        (applied-cxn agent) nil
        (applied-chunk agent) nil
        (alternative-cxn agent) nil
        (alternative-chunk agent) nil
        (found-answer agent) nil
        (communicated-successfully agent) t)
  ;; set the current context in the ontology
  (set-data (ontology agent) 'clevr-context context)
  ;; set the ontology in the blackboard of the grammar (used for goal test)
  ;(set-data (blackboard (grammar agent)) 'ontology (ontology agent))
  ;; give the speaker the correct answer
  (when (speakerp agent)
    (setf (found-answer agent)
          (answer->category (ontology agent)
                            (answer question)))))

(define-event context-determined (image-path pathname))
(define-event question-determined (question-object clevr-question))

(defmethod interact :before ((experiment holophrase-experiment) interaction &key)
  "Choose the context and question (utterance) for the current interaction."
  (multiple-value-bind (context question-set) (random-scene (world experiment))
    (let ((question (random-elt (questions question-set))))
      (notify context-determined (image context))
      (notify question-determined question)
      (loop for agent in (interacting-agents experiment)
            do (initialize-agent agent :context context :question question)))))

;; tutor = speaker
;; learner = hearer
(defmethod learner-hears ((experiment holophrase-experiment) interaction)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    (if (and (parse-question hearer)
             (interpret hearer))
      (unless (determine-success speaker hearer)
        (adopt hearer (found-answer speaker))
        (setf (communicated-successfully speaker) nil
              (communicated-successfully hearer) nil))
      (progn (adopt hearer (found-answer speaker))
        (setf (communicated-successfully speaker) nil
              (communicated-successfully hearer) nil)))))

;; tutor = hearer
;; learner = speaker
(defmethod learner-speaks ((experiment holophrase-experiment) interaction)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction)))
    (if (and (conceptualise speaker)
             (produce-question speaker))
      (progn (setf (utterance hearer) (utterance speaker))
        (unless (tutor-validates-success hearer speaker)
          (speaker-learning speaker hearer)
          (setf (communicated-successfully speaker) nil
                (communicated-successfully hearer) nil)))
      (setf (communicated-successfully speaker) nil
            (communicated-successfully hearer) nil))))
      
;;;; Interact
(defmethod interact ((experiment holophrase-experiment) interaction &key)
  "Interaction script"
  (let ((speaker (speaker interaction)))
    (if (tutorp speaker)
      (learner-hears experiment interaction)
      (learner-speaks experiment interaction))))

(defun store-sample (agent)
  (let* ((context (find-data (ontology agent) 'clevr-context))
         (context-id (index context))
         (answer (answer->category (ontology agent) (answer (question-object agent))))
         (sample (list context-id (utterance agent) answer))
         (sample-window (get-configuration agent :sample-window)))
    (unless (find sample (samples agent)
                  :test #'(lambda (sample-1 sample-2)
                            (and (= (first sample-1) (first sample-2))
                                 (string= (second sample-1) (second sample-2)))))
      (if (and sample-window
               (length>= (samples agent) sample-window))
        (setf (samples agent)
              (cons sample (butlast (samples agent))))
        (push sample (samples agent))))))

(defmethod interact :after ((experiment holophrase-experiment) interaction &key)
  "Consolidation after the interaction"
  ;; consolidation on the hearer side based on success
  (loop for agent in (interacting-agents experiment)
        do (align-agent agent (get-configuration experiment :alignment-strategy)))
  ;; store sample when strategy is active
  (when (eql (get-configuration experiment :learning-strategy) :keep-samples)
    (let ((learner (find 'learner (interacting-agents experiment) :key #'id)))
      (when (hearerp learner)
        (store-sample learner)))))

