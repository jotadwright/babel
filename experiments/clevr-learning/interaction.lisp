(in-package :clevr-learning)

;; ####################
;; + Initialize Agent +
;; ####################

(defgeneric initialize-agent (agent context clevr-question)
  (:documentation "Prepare the agent for the next interaction"))

(defmethod initialize-agent ((agent holophrase-tutor)
                             context clevr-question)
  (setf (clevr-question agent) clevr-question
        (utterance agent) (question clevr-question)
        (communicated-successfully agent) t
        (ground-truth-answer agent)
        (answer->category (ontology agent)
                          (answer clevr-question)))
  ;; set the current context in the ontology
  (set-data (ontology agent) 'clevr-context context))

(defmethod initialize-agent ((agent holophrase-learner)
                             context clevr-question)
  (setf (clevr-question agent) clevr-question
        (utterance agent) (question clevr-question)
        (communicated-successfully agent) t
        (applied-cxn agent) nil
        (applied-chunk agent) nil
        (applicable-chunks agent) nil
        (computed-answer agent) nil)
  ;; set the current context in the ontology
  (set-data (ontology agent) 'clevr-context context)
  ;; set the ground-truth answer when speaker
  (when (speakerp agent)
    (setf (ground-truth-answer agent)
          (answer->category (ontology agent)
                            (answer clevr-question)))))
          

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

(defun all-primitives-available-p (experiment clevr-question)
  (let* ((available-primitives (get-configuration experiment :available-primitives))
         (necessary-primitives (mapcar #'function-name
                                       (nodes (program clevr-question))))
         (processed-primitives
          (loop for primitive in necessary-primitives
                if (eql primitive 'equal_integer)
                collect (downcase (mkstr primitive))
                else
                collect (downcase (first (split (mkstr primitive) #\_)))))
         (found-primitives
          (remove-duplicates 
           (loop for primitive in processed-primitives
                 collect (rest (assoc primitive
                                      *primitive-mapping*
                                      :test #'string=))))))
    (loop for primitive in found-primitives
          always (find primitive available-primitives))))
    

(define-event context-determined (image-path pathname))
(define-event question-determined (clevr-question clevr-question))

(defmethod interact :before ((experiment holophrase-experiment) interaction &key)
  "Choose the context and question (utterance) for the current interaction.
   Always check if all primitives are available. If not, retry."
  (loop for (context question-set) = (multiple-value-list
                                      (random-scene (world experiment)))
        for clevr-question = (random-elt (questions question-set))
        until (all-primitives-available-p experiment clevr-question)
        finally (progn (notify context-determined (image context))
                  (notify question-determined clevr-question)
                  (loop for agent in (interacting-agents experiment)
                        do (initialize-agent agent context clevr-question)))))

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
    (let ((learner (find 'learner (interacting-agents experiment) :key #'role)))
      (when (hearerp learner)
        (store-sample learner)))))



