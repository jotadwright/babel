(in-package :mwm)

(defun clear-agent (agent)
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil)
  (setf (utterance agent) nil
        (communicated-successfully agent) nil))

(defun closest (topic clevr-context)
  (let ((topic-x (get-attr-val topic 'xpos))
        (topic-y (get-attr-val topic 'ypos)))
    (the-smallest #'(lambda (object)
                      (abs (euclidean (list topic-x topic-y)
                                      (list (x-pos object)
                                            (y-pos object)))))
                  (objects clevr-context))))

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
                              :colour (get-configuration experiment :extracted-colour-space))))
         (mwm-context (if (eql data-type :simulated)
                        simulated-clevr-context
                        (when (length> (objects extracted-clevr-context) 1)
                          extracted-clevr-context)))
         (topic (when mwm-context (random-elt (objects mwm-context)))))
    ;; context = the learner's context
    ;; topic = the topic in the learner's context
    ;; clevr-context = the tutor's context
    ;; clevr-topic = the topic in the tutor's context
    (loop for agent in (interacting-agents experiment)
          do (clear-agent agent)
          do (set-data agent 'context mwm-context)
          do (set-data agent 'clevr-context symbolic-clevr-context)
          do (set-data agent 'topic topic)
          do (set-data agent 'clevr-topic
                       (when topic
                         (if (eql data-type :simulated)
                           (find (id topic) (objects symbolic-clevr-context) :key #'id)
                           (closest topic symbolic-clevr-context)))))
    (unless (find-data (hearer experiment) 'context)
      (before-interaction experiment))
    (notify context-determined experiment)))


;;;; do-interaction
(defgeneric do-interaction (experiment)
  (:documentation "Run the appropriate interaction script"))

(defmethod conceptualise-until-success ((agent mwm-agent) (role (eql 'tutor)))
  (loop while t
        for success = (conceptualise agent (id agent))
        if success
        return success
        else
        do (before-interaction (experiment agent))))

(defmethod conceptualise-until-success ((agent mwm-agent) (role (eql 'learner)))
  "In some cases, the tutor cannot even discriminate the topic.
   If this is the case, the learner should not even try"
  (let ((tutor (find 'tutor (population (experiment agent)) :key #'id)))
    (loop while t
          for possible-to-conceptualise-symbolically
          = (with-disabled-monitors (conceptualise tutor (id tutor)))
          if possible-to-conceptualise-symbolically
          do (progn (conceptualise agent (id agent))
               (return))
          else
          do (before-interaction (experiment agent)))))

(defmethod do-interaction ((experiment mwm-experiment))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (conceptualise-until-success speaker (id speaker))
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
      ;; alignment when tutor is speaker
      (tutor (let ((tutor (speaker experiment))
                   (learner (hearer experiment)))
               (when (find-data tutor 'clevr-conceptualisation)
                 (align-agent learner (get-data learner 'topic)))))
      ;; alignment when learner is speaker
      ;; reasons for failure:
      ;; - the learner could not conceptualise --> do nothing
      ;; - the tutor's interpretation failed 
      ;;   because the learner said too much/too little/the wrong things
      ;;   --> how to align?
      ;; - the tutor's and learner's topics are not equal --> do alignment
      ;; or there was success --> do alignment
      (learner (let ((tutor (hearer experiment))
                     (learner (speaker experiment)))
                 (when (and (find-data learner 'applied-cxns)
                            (find-data tutor 'interpreted-topic))
                   (align-agent learner (get-data learner 'topic))))))))

                

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
