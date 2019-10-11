(in-package :robot-concept-learning)

(defun clear-agent (agent)
  "Clear the slots of the agent for the next interaction."
  (setf (applied-cxn agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

;;;; before-interaction
(defgeneric before-interaction (experiment)
  (:documentation "Initialize the interaction, e.g. set the context of the agents"))

(define-event context-determined (experiment mwm-experiment) (image string))

(defmethod before-interaction ((experiment mwm-experiment))
  (let ((agent (first (population experiment))))
    ;; the agent observes the world and stores it
    (multiple-value-bind (context image) (observe-and-process-world agent)
      (setf (context agent) context)
      (notify context-determined experiment image))))

;;;; do-interaction
(defgeneric do-interaction (experiment)
  (:documentation "Run the interaction script"))

(defmethod do-interaction ((experiment mwm-experiment))
  (let ((agent (first (population experiment))))
    ;; the agent waits for an utterance from the human
    ;; the agent tries to parse and interpret the utterance
    (setf (utterance agent)
          (receive-utterance agent))
    (parse-word agent)
    (interpret agent)))

;;;; after-interaction
(defgeneric after-interaction (experiment)
  (:documentation "Perform tasks after the interaction, such as alignment"))

(defmethod after-interaction ((experiment mwm-experiment))
  ;; the agent asks feedback after the interaction,
  ;; determines the success and updates its lexicon
  (let* ((agent (first (population experiment)))
         (feedback (receive-feedback agent))
         (correct-topic (closest-object agent (first (objects feedback)))))
    (when (eql (topic agent) correct-topic)
      (setf (communicated-successfully agent) t))
    (when (get-configuration experiment :learning-active)
      (align-agent agent correct-topic))))

;;;; Interact
(defmethod interact ((experiment mwm-experiment)
                     interaction &key)
  (when (or (eql (get-configuration experiment :experiment-type) :cogent)
            (eql (get-configuration experiment :experiment-type) :incremental))
    (maybe-switch-conditions experiment))
  ;; regular interaction
  (before-interaction experiment)
  (do-interaction experiment)
  (after-interaction experiment))

(defun maybe-switch-conditions (experiment)
  (let ((switch-condition-interval (get-configuration experiment :switch-conditions-after-n-interactions))
        (current-interaction-number (interaction-number (current-interaction experiment))))
    (when (= (mod current-interaction-number switch-condition-interval) 0)
      (case (get-configuration experiment :experiment-type)
        ;; COGENT
        (:cogent
         ;; only do something when learning is active
         ;; otherwise, conditions have already been switched
         (when (get-configuration experiment :learning-active)
           ;; turn off learning
           (set-configuration experiment :learning-active nil :replace t)
           ;; reload the world with a different dataset
           (setf (world experiment)
                 (make-instance 'clevr-world :data-sets '("valB")))
           ;; when the data-type is :extracted
           ;; also change the :data-path
           (when (eql (get-configuration experiment :data-type) :extracted)
             (set-data experiment :data-path
                       (parse-namestring
                        (cl-ppcre:regex-replace-all "valA" (namestring (find-data experiment :data-path)) "valB"))))
           (format t "~%~%SWITCHING FROM CONDITION A TO CONDITION B. SWITCHED OFF LEARNING~%~%")))
        ;; INCREMENTAL
        (:incremental
         ;; get the current condition (1, 2 or 3)
         (let* ((current-condition-char (uiop:last-char
                                         (first (get-data experiment :data-sets))))
                (current-condition-nr (parse-integer (mkstr current-condition-char))))
           ;; only do someting when condition 3 has not yet been reached
           (unless (= current-condition-nr 5)
             (let* ((next-condition-nr (1+ current-condition-nr))
                    (next-condition-char (coerce (mkstr next-condition-nr) 'character))
                    (next-condition (mkstr "phase_" next-condition-nr)))
               ;; export the lexicon before each condition switch
               ;(lexicon->pdf (find 'learner (population experiment) :key #'id))
               ;; reload the world with a different dataset
               (setf (world experiment)
                     (make-instance 'clevr-world :data-sets (list next-condition)))
               ;; set the :data-sets configuration
               (set-data experiment :data-sets (list next-condition))
               ;; when the data-type is :extracted
               ;; also changed the data-path
               (when (eql (get-configuration experiment :data-type) :extracted)
                 (set-data experiment :data-path
                           (parse-namestring
                            (cl-ppcre:regex-replace-all (format nil "/phase_~a" current-condition-char)
                                                        (namestring (get-data experiment :data-path))
                                                        (format nil "/phase_~a" next-condition-char)))))
               ;; print a message
               (format t "~%~%SWITCHING FROM CONDITION ~a TO CONDITION ~a~%~%"
                       current-condition-nr next-condition-nr)))))))))
