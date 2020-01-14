(in-package :mwm)

(defun clear-agent (agent)
  "Clear the slots of the agent for the next interaction."
  (setf (applied-cxns agent) nil
        (discriminative-set agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

;;;; before-interaction
(defgeneric before-interaction (experiment)
  (:documentation "Initialize the interaction"))

(define-event context-determined (experiment mwm-experiment))

(defmethod before-interaction ((experiment mwm-experiment))
  (let* ((data-type (get-configuration experiment :data-type))
         (symbolic-clevr-context (random-scene (world experiment)))
         (simulated-clevr-context
          (clevr->simulated symbolic-clevr-context :scale (get-configuration experiment :scale-world)))
         (extracted-clevr-context
          (when (eql data-type :extracted)
            (clevr->extracted symbolic-clevr-context
                              :directory (find-data experiment :data-path)
                              :scale (get-configuration experiment :scale-world)
                              :colour (get-configuration experiment :extracted-colour-space)))))
    (loop for agent in (interacting-agents experiment)
          do (setf (context agent)
                   (if (learnerp agent)
                     (if (eql data-type :simulated)
                       simulated-clevr-context
                       (if (> (length (objects extracted-clevr-context)) 1)
                         extracted-clevr-context
                         nil))
                     symbolic-clevr-context))
          do (setf (symbolic-context agent) symbolic-clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent)
                     (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (unless (context (hearer experiment))
      (before-interaction experiment))
    (notify context-determined experiment)))


;;;; do-interaction
(defgeneric do-interaction (experiment)
  (:documentation "Run the appropriate interaction script"))

(defun conceptualise-until-success (agent)
  (loop while t
        for success = (conceptualise agent (id agent))
        if success
        return success
        else
        do (before-interaction (experiment agent))))

(defmethod do-interaction ((experiment mwm-experiment))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (case (id speaker)
      (tutor (conceptualise-until-success speaker))
      (learner (conceptualise speaker (id speaker))))
    (produce-word speaker (id speaker))
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer (id hearer))
                 (interpret hearer (id hearer))
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

;;;; after-interaction
(defgeneric after-interaction (experiment)
  (:documentation "Finalize the interaction"))

(defmethod after-interaction ((experiment mwm-experiment))
  (when (get-configuration experiment :learning-active)
    (case (id (speaker experiment))
      (tutor (let ((tutor (speaker experiment))
                   (learner (hearer experiment)))
               (when (discriminative-set tutor)
                 (let ((topic (if (eql (get-configuration experiment :data-type) :simulated)
                                (find (id (topic tutor)) (objects (context learner)) :key #'id)
                                (closest-to-topic tutor (context learner)))))
                   (align-agent learner topic)))))
      (learner nil))))
                 
;; how to align when the learner was speaker?
;; let the tutor produce for the topic and align using that word-object pair?
;; this could be a completely different utterance than the one produced by the learner
;; it could even be null...

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
               (lexicon->pdf (find 'learner (population experiment) :key #'id)
                             :name (format nil "incremental-phase-~a" current-condition-nr))
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
