(in-package :spatial-concepts)

;;;;
;;;; Setup interaction
;;;;

(defun clear-agent (agent)
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil))

(defun closest-object (spatial-topic symbolic-clevr-context)
  "Given a topic from the 'extracted' world, find
   the closest object in the symbolic world using
   the xy-coordinates."
  (let ((topic-x (get-attr-val spatial-topic 'xpos))
        (topic-y (get-attr-val spatial-topic 'ypos)))
    (the-smallest #'(lambda (object)
                      (abs (euclidean (list topic-x topic-y)
                                      (list (x-pos object)
                                            (y-pos object)))))
                  (objects symbolic-clevr-context))))

(defgeneric before-interaction (experiment)
  (:documentation "Initialize the interaction"))

(define-event context-determined (experiment spatial-experiment))
(define-event topic-determined (experiment spatial-experiment))

(defmethod before-interaction ((experiment spatial-experiment))
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
         (spatial-context
          (case world-type
            (:simulated
             (clevr->simulated symbolic-clevr-context))
            (:extracted
             (clevr->extracted symbolic-clevr-context
                               :directory (find-data experiment :ns-vqa-data-path))))))
    (if (length> (objects spatial-context) 1)
      (progn
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'context spatial-context)
              do (set-data agent 'tutor-context symbolic-clevr-context))
        (notify context-determined experiment))
      (sample-scene experiment))))


(defun sample-topic (experiment)
  (let* ((interaction (current-interaction experiment)) 
         (world-type (get-configuration experiment :world-type))
         (agent (first (population experiment)))
         (symbolic-clevr-context (find-data agent 'tutor-context))
         (spatial-context (find-data agent 'context))
         (tried-topics (tried-topics interaction (name symbolic-clevr-context)))
         (available-topics (set-difference (objects spatial-context) tried-topics)))
    (if (null available-topics)
      (progn
        (sample-scene experiment)
        (sample-topic experiment))
      (let* ((spatial-topic (random-elt available-topics))
             (tutor-topic
              (case world-type
                (:simulated (find (id spatial-topic) (objects symbolic-clevr-context) :key #'id))
                (:extracted (closest-object spatial-topic symbolic-clevr-context))))
             (scene-topic-cons (cons (name symbolic-clevr-context) spatial-topic)))
        (notify topic-determined experiment)
        (push-data interaction 'attempted-topics scene-topic-cons)
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'topic spatial-topic)
              do (set-data agent 'tutor-topic tutor-topic))))))


(defun tried-topics (interaction scene-name)
  (let ((attempts (find-data interaction 'attempted-topics)))
    (mapcar #'cdr (find-all scene-name attempts :key #'car :test #'string=))))


;;;;
;;;; Run interaction
;;;;
(defgeneric do-interaction (experiment)
  (:documentation "Run the interaction script"))

(defmethod do-interaction ((experiment spatial-experiment))
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
    (when (and (utterance speaker)(pointed-object speaker))
      (progn
        (setf (utterance hearer) (utterance speaker))
        (setf (pointed-object hearer) (pointed-object speaker)))
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

(defmethod after-interaction ((experiment spatial-experiment))
    (let ((tutor (tutor experiment))
          (learner (learner experiment)))
        (when (find-data tutor 'tutor-conceptualisation)
        (alignment learner (get-data learner 'topic)
                   (find-data learner 'applied-concept)))))
               
;;;;
;;;; interact
;;;;
(defmethod interact ((experiment spatial-experiment) interaction &key)
  ;; regular interaction
  (before-interaction experiment)
  (do-interaction experiment)
  (after-interaction experiment))

