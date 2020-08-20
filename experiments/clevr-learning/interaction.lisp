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
  ;; set the composer-chunks in the ontology
  (unless (find-data (ontology agent) 'composer-chunks)
    (set-data (ontology agent) 'composer-chunks
              (mapcar #'(lambda (p) (create-chunk-from-primitive
                                     p :primitive-inventory (primitives agent)))                       
                      (primitives-list (primitives agent)))))
  ;; set the ground-truth answer when speaker
  (when (speakerp agent)
    (setf (ground-truth-answer agent)
          (answer->category (ontology agent) answer))))
          

;; ###############
;; + Interaction +
;; ###############

(define-event context-determined (image-path pathname))
(define-event question-determined (question string) (answer string))


(defun read-clevr-data-file (filename)
  (with-open-file (stream filename :direction :input)
    (decode-json-from-string
     (read-line stream))))

(defun all-primitives-available-p (experiment meaning)
  (let* ((available-primitives (get-configuration experiment :available-primitives))
         (necessary-primitives (remove 'bind (remove-duplicates (mapcar #'first meaning))))
         (all-available-p
          (loop for p in necessary-primitives
                always (find p available-primitives))))
    (unless all-available-p
      (set-data experiment :current-utterance-index
                (1+ (get-data experiment :current-utterance-index))))
    all-available-p))

(defun attempts-remaining-p (experiment)
  (let* ((current-utterance-index (get-data experiment :current-utterance-index))
         (attempts-per-utterance (get-data experiment :attempts-per-utterance))
         (max-attempts (get-configuration experiment :max-attempts-per-utterance))
         (pair (assoc current-utterance-index attempts-per-utterance)))
    (if (and pair (> (rest pair) max-attempts))
      (progn (set-data experiment :current-utterance-index
                       (1+ current-utterance-index))
        nil)
      t)))

(defun timeout-reached-p (experiment)
  (let ((learner (find 'learner (population experiment) :key #'role)))
    (if (find-data learner :timeout)
      (progn (set-data experiment :current-utterance-index
                       (1+ (get-data experiment :current-utterance-index)))
        (set-data learner :timeout nil)
        t)
      nil)))

(defun increment-utterance-attempt (experiment)
  (let ((current-utterance-index (get-data experiment :current-utterance-index))
        (attempts-per-utterance (get-data experiment :attempts-per-utterance)))
    (if (assoc current-utterance-index attempts-per-utterance)
      (incf (rest (assoc current-utterance-index attempts-per-utterance)))
      (set-data experiment :attempts-per-utterance
                (cons (cons current-utterance-index 1) attempts-per-utterance)))))

(defmethod interact :before ((experiment holophrase-experiment) interaction &key)
  "Choose the context and question (utterance) for the current interaction.
   Always check if all primitives are available. If not, retry."
  (loop for filename = (nth (get-data experiment :current-utterance-index)
                            (questions experiment))
        for line = (read-clevr-data-file filename)
        for continuep = (and (all-primitives-available-p
                              experiment (read-from-string
                                          (rest (assoc :meaning line))))
                             (attempts-remaining-p experiment)
                             (not (timeout-reached-p experiment)))
        until continuep
        finally (let* ((question (rest (assoc :question line)))
                       (answers (rest (assoc :answers line)))
                       (meaning (read-from-string (rest (assoc :meaning line))))
                       (bind-statements (find-all 'bind meaning :key #'car))
                       (scene-name/answer (random-elt answers))
                       (scene (find-scene-by-name (rest (assoc :scene scene-name/answer))
                                                  (world experiment)))
                       (answer (rest (assoc :answer scene-name/answer))))
                  (notify context-determined (image scene))
                  (notify question-determined question answer)
                  (increment-utterance-attempt experiment)
                  (loop for agent in (interacting-agents experiment)
                        do (set-data agent :ground-truth-meaning meaning)
                        do (set-data agent :bind-statements bind-statements)
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
    (if (determine-success speaker hearer)
      ;; when the game was a success, check if the ground truth program was reached
      ;; if it was, store the utterance and the program
      (when (check-ground-truth-program hearer)
        (with-open-file (stream (get-configuration experiment :output-file)
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (write-line (format nil "~a,~a"
                              (utterance hearer)
                              (downcase (mkstr (irl-program (applied-chunk hearer)))))
                      stream))
        (set-data experiment :current-utterance-index
                  (1+ (get-data experiment :current-utterance-index))))
      (progn (adopt hearer (ground-truth-answer speaker))
        (setf (communicated-successfully speaker) nil
              (communicated-successfully hearer) nil)))
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



