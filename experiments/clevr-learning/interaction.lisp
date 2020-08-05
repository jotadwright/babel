(in-package :clevr-learning)

;; ####################
;; + Initialize Agent +
;; ####################

(defgeneric initialize-agent (agent scene question answer)
  (:documentation "Prepare the agent for the next interaction"))

(defmethod initialize-agent ((agent holophrase-tutor)
                             scene question answer)
  (setf (clevr-question agent) nil
        (utterance agent) question
        (communicated-successfully agent) t
        (ground-truth-answer agent) (answer->category (ontology agent) answer))
  ;; set the current context in the ontology
  (set-data (ontology agent) 'clevr-context scene))

(defmethod initialize-agent ((agent holophrase-learner)
                             scene question answer)
  (setf (clevr-question agent) nil
        (utterance agent) question
        (communicated-successfully agent) t
        (applied-cxn agent) nil
        (applied-chunk agent) nil
        (applicable-chunks agent) nil
        (computed-answer agent) nil)
  ;; set the current context in the ontology
  (set-data (ontology agent) 'clevr-context scene)
  ;; set the ground-truth answer when speaker
  (when (speakerp agent)
    (setf (ground-truth-answer agent)
          (answer->category (ontology agent) answer))))
          

;; ###############
;; + Interaction +
;; ###############

(defparameter *primitive-mapping*
  '(("scene" . get-context) ("filter" . filter)
    ("query" . query) ("count" . count!)
    ("relate" . relate) ("same" . same)
    ("union" . union!) ("intersect" . intersect)
    ("unique" . unique) ("exist" . exist)
    ("equal" . equal?) ("less" . less-than)
    ("greater" . greater-than)
    ("equal_integer" . equal-integer))
  "Maps the CLEVR primitive names to the IRL primitive names")

(defun all-primitives-available-p (experiment meaning)
  (let* ((available-primitives (get-configuration experiment :available-primitives))
         (necessary-primitives (remove 'bind (remove-duplicates (mapcar #'first meaning)))))
    (loop for p in necessary-primitives
          always (find p available-primitives))))
    

(define-event context-determined (image-path pathname))
(define-event question-determined (question string) (answer string))

(defmethod interact :before ((experiment holophrase-experiment) interaction &key)
  "Choose the context and question (utterance) for the current interaction.
   Always check if all primitives are available. If not, retry."
  ;; examples: third, sixth, eighth, ninth, first, second, fourth, fifth, tenth 
  (loop for line = (random-elt (subseq (data experiment) 0 10))
        until (all-primitives-available-p
               experiment (read-from-string
                           (rest (assoc :meaning line))))
        finally (let* ((question (rest (assoc :question line)))
                       (answers (rest (assoc :answers line)))
                       (scene-name/answer (random-elt answers))
                       (scene (find-scene-by-name (rest (assoc :scene scene-name/answer))
                                                  (world experiment)))
                       (answer (rest (assoc :answer scene-name/answer))))
                  (notify context-determined (image scene))
                  (notify question-determined question answer)
                  (loop for agent in (interacting-agents experiment)
                        do (initialize-agent agent scene question answer)))))

(defmethod interact ((experiment holophrase-experiment) interaction &key)
  "Interaction script depends on who is the speaker
   and who is the listener"
  (holophrase-interaction experiment interaction
                          (speaker interaction)
                          (hearer interaction)))
      
(defgeneric holophrase-interaction (experiment interaction speaker hearer)
  (:documentation "Run the interaction, depending on speaker and hearer"))

(defmethod holophrase-interaction ((experiment holophrase-experiment) interaction
                                   (speaker holophrase-tutor)
                                   (hearer holophrase-learner))
  (if (and (parse-question hearer)
           (interpret hearer))
    (unless (determine-success speaker hearer)
      (adopt hearer (ground-truth-answer speaker))
      (setf (communicated-successfully speaker) nil
            (communicated-successfully hearer) nil))
    (progn (adopt hearer (ground-truth-answer speaker))
      (setf (communicated-successfully speaker) nil
            (communicated-successfully hearer) nil))))

(defmethod holophrase-interaction ((experiment holophrase-experiment) interaction
                                   (speaker holophrase-learner)
                                   (hearer holophrase-tutor))
  (if (and (conceptualise speaker)
           (produce-question speaker))
    (progn (setf (utterance hearer) (utterance speaker))
      (tutor-interprets hearer)
      (unless (determine-success speaker hearer)
        (setf (communicated-successfully speaker) nil
              (communicated-successfully hearer) nil)))
    (setf (communicated-successfully speaker) nil
          (communicated-successfully hearer) nil)))

(defmethod interact :after ((experiment holophrase-experiment) interaction &key)
  "Consolidation after the interaction"
  ;; consolidation on the hearer side based on success
  (loop for agent in (interacting-agents experiment)
        do (align-agent agent (get-configuration experiment :alignment-strategy)))
  ;; store sample when strategy is active
  (when (eql (get-configuration experiment :learning-strategy) :keep-samples)
    (let ((learner (find 'learner (interacting-agents experiment) :key #'role))
          (tutor (find 'tutor (interacting-agents experiment) :key #'role)))
      (store-sample learner (ground-truth-answer tutor)))))



