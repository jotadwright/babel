(in-package :mwm)

;;;;
;;;; Setup interaction
;;;;

(defun clear-agent (agent)
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil))

(defun closest-object (mwm-topic symbolic-clevr-context)
  "Given a topic from the 'extracted' world, find
   the closest object in the symbolic world using
   the xy-coordinates."
  (let ((topic-x (get-attr-val mwm-topic 'xpos))
        (topic-y (get-attr-val mwm-topic 'ypos)))
    (the-smallest #'(lambda (object)
                      (abs (euclidean (list topic-x topic-y)
                                      (list (x-pos object)
                                            (y-pos object)))))
                  (objects symbolic-clevr-context))))

(defgeneric before-interaction (experiment)
  (:documentation "Initialize the interaction"))

(define-event context-determined (experiment mwm-experiment))
(define-event topic-determined (experiment mwm-experiment))

(defmethod before-interaction ((experiment mwm-experiment))
  ;; 1. load a random scene
  ;; 2. pick a random topic
  ;; step 2 can be retried if conceptualisation fails
  ;; but need to keep track of the objects that have
  ;; been tried (in some blackboard probably)
  (loop for agent in (interacting-agents experiment)
        do (clear-agent agent))
  (sample-scene experiment)
  (sample-topic experiment))


(defun sample-scene (experiment)
  (let* ((world-type (get-configuration experiment :world-type))
         (symbolic-clevr-context (random-scene (world experiment)))
         (mwm-context
          (case world-type
            (:simulated
             (clevr->simulated symbolic-clevr-context))
            (:extracted
             (clevr->extracted symbolic-clevr-context
                               :directory (find-data experiment :ns-vqa-data-path))))))
    (if (length> (objects mwm-context) 1)
      (progn
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'context mwm-context)
              do (set-data agent 'tutor-context symbolic-clevr-context))
        (notify context-determined experiment))
      (sample-scene experiment))))


(defun sample-topic (experiment)
  (let* ((interaction (current-interaction experiment)) 
         (world-type (get-configuration experiment :world-type))
         (agent (first (population experiment)))
         (symbolic-clevr-context (find-data agent 'tutor-context))
         (mwm-context (find-data agent 'context))
         (tried-topics (tried-topics interaction (name symbolic-clevr-context)))
         (available-topics (set-difference (objects mwm-context) tried-topics)))
    (if (null available-topics)
      (progn
        (sample-scene experiment)
        (sample-topic experiment))
      (let* ((mwm-topic (random-elt available-topics))
             (tutor-topic
              (case world-type
                (:simulated (find (id mwm-topic) (objects symbolic-clevr-context) :key #'id))
                (:extracted (closest-object mwm-topic symbolic-clevr-context))))
             (scene-topic-cons (cons (name symbolic-clevr-context) mwm-topic)))
        (notify topic-determined experiment)
        (push-data interaction 'attempted-topics scene-topic-cons)
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'topic mwm-topic)
              do (set-data agent 'tutor-topic tutor-topic))))))


(defun tried-topics (interaction scene-name)
  (let ((attempts (find-data interaction 'attempted-topics)))
    (mapcar #'cdr (find-all scene-name attempts :key #'car :test #'string=))))


;;;;
;;;; Run interaction
;;;;
(defgeneric do-interaction (experiment)
  (:documentation "Run the interaction script"))

(defmethod do-interaction ((experiment mwm-experiment))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (conceptualise speaker)
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

;;;;
;;;; Alignment
;;;;
(defgeneric after-interaction (experiment)
  (:documentation "Finalize the interaction"))

(defmethod after-interaction ((experiment mwm-experiment))
  ;; only in condition B of the cogent experiment, there is no alignment
  (unless (and (eql (get-configuration experiment :experiment-type) :cogent)
               (eql (get-configuration experiment :cogent-stage) :B))
    (let ((tutor (tutor experiment))
          (learner (learner experiment)))
      (if (speakerp tutor)
        ;; alignment when tutor is speaker
        ;; the tutor reveals the topic and
        ;; the learner aligns its concept to it
        (when (find-data tutor 'tutor-conceptualisation)
          (alignment learner (get-data learner 'topic)
                     (find-data learner 'applied-concept)))
        ;; alignment when learner is speaker
        (cond
         ;; success? do alignment
         ((and (communicated-successfully tutor)
               (communicated-successfully learner))
          (alignment learner (get-data learner 'topic)
                     (find-data learner 'applied-concept)))
         ;; learner could not conceptualise? do nothing
         ((null (find-data learner 'applied-concept))
          nil)
         ;; learner could conceptualise, but interpretation failed?
         ;; do nothing
         ((null (find-data tutor 'interpreted-topic))
          nil)
         ;; learner could conceptualise and interpretation succeeded
         ;; but incorrectly? do nothing
         (t nil))))))


;;;;
;;;; Experimental conditions
;;;;
(defgeneric switch-condition-p (experiment experiment-type)
  (:documentation "Predicate that checks whether the experiment should
   advance to the next condition"))

(defmethod switch-condition-p ((experiment mwm-experiment) experiment-type)
  "Switch when N interactions have been played."
  (let ((switch-condition-interval
         (get-configuration experiment :switch-conditions-after-n-interactions))
        (current-interaction-number
         (interaction-number (current-interaction experiment))))
    (= (mod current-interaction-number switch-condition-interval) 0)))

(defmethod switch-condition-p ((experiment mwm-experiment)
                               (experiment-type (eql :baseline)))
  "Never switch conditions"
  nil)

(defmethod switch-condition-p ((experiment mwm-experiment)
                               (experiment-type (eql :cogent)))
  "Switch when N interactions have been played
   and the experiment is still in stage A."
  (and (call-next-method)
       (eql (get-configuration experiment :cogent-stage) :A)))

(defmethod switch-condition-p ((experiment mwm-experiment)
                               (experiment-type (eql :incremental)))
  "Switch when N interactions have been played
   and the experiment has not reached stage 5."
  (and (call-next-method)
       (< (get-configuration experiment :incremental-stage) 5)))

(defmethod switch-condition-p ((experiment mwm-experiment)
                               (experiment-type (eql :compositional)))
  "Never switch conditions"
  nil)


(defgeneric setup-next-condition (experiment experiment-type)
  (:documentation "Setup the next experimental condition."))

(defmethod setup-next-condition ((experiment mwm-experiment)
                                 (experiment-type (eql :cogent)))
  ;; message
  (format t "~%~%SWITCHING FROM CONDITION A TO CONDITION B. SWITCHED OFF LEARNING~%~%")
  ;; set the config
  (set-configuration experiment :cogent-stage :B :replace t)
  ;; load the scenes
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets '("valB")))

  ;; to do: maybe summarize the concepts here, using the concept history??
  
  ;; load the extracted scenes
  (when (eql (get-configuration experiment :world-type) :extracted)
    (let ((data-path (namestring (find-data experiment :ns-vqa-data-path))))
      (set-data experiment :ns-vqa-data-path
                (parse-namestring
                 (cl-ppcre:regex-replace-all "valA" data-path "valB"))))))

(defmethod setup-next-condition ((experiment mwm-experiment)
                                 (experiment-type (eql :incremental)))
  (let* ((current-stage (get-configuration experiment :incremental-stage))
         (next-stage (1+ current-stage))
         (next-stage-name (format nil "phase_~a" next-stage)))
    ;; message
    (format t "~%~%SWITCHING FROM CONDITION ~a TO CONDITION ~a~%~%"
            current-stage next-stage)
    ;; export the lexicon before each condition switch
    (lexicon->pdf (learner experiment) :serie (series-number experiment))
    ;; set the config
    (set-configuration experiment :incremental-stage next-stage :replace t)
    ;; reload the world with a different dataset
    (setf (world experiment)
          (make-instance 'clevr-world :data-sets (list next-stage-name)))
    ;; when the data-type is :extracted
    ;; also changed the data-path
    (when (eql (get-configuration experiment :world-type) :extracted)
      (let ((data-path (namestring (find-data experiment :ns-vqa-data-path))))
        (set-data experiment :ns-vqa-data-path
                  (parse-namestring
                   (cl-ppcre:regex-replace-all (format nil "/phase_~a" current-stage)
                                               data-path
                                               (format nil "/phase_~a" next-stage))))))))
    
  



  
           
;;;;
;;;; interact
;;;;
(defmethod interact ((experiment mwm-experiment) interaction &key)
  (let ((experiment-type (get-configuration experiment :experiment-type)))
    (when (switch-condition-p experiment experiment-type)
      (setup-next-condition experiment experiment-type)))
  ;; regular interaction
  (before-interaction experiment)
  (do-interaction experiment)
  (after-interaction experiment))

