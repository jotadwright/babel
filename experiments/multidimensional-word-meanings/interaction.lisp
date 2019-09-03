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
  (let* ((data-source (get-configuration experiment :data-source))
         (symbolic-clevr-context (random-scene (world experiment)))
         (simulated-clevr-context
          (clevr->simulated symbolic-clevr-context :scale (get-configuration experiment :scale-world)))
         (extracted-clevr-context
          (when (eql data-source :extracted)
            (clevr->extracted symbolic-clevr-context
                              :directory (find-data experiment :data-path)
                              :scale (get-configuration experiment :scale-world)))))
    (loop for agent in (interacting-agents experiment)
          do (setf (context agent)
                   (if (learnerp agent)
                     (if (eql data-source :clevr)
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
                 (let ((topic (if (eql (get-configuration experiment :data-source) :clevr)
                                (find (id (topic tutor)) (objects (context learner)) :key #'id)
                                (closest-to-topic tutor (context learner)))))
                   (align-agent learner topic)))))
      (learner nil))))
                 
;; how to align when the learner was speaker?
;; let the tutor produce for the topic and align using that word-object pair?
;; this could be a completely different utterance than the one produced by the learner
;; it could even be null...

;;;; Interact
(defmethod interact ((experiment mwm-experiment)
                     interaction &key)
  ;; for the cogent test:
  (let ((test-after-n-interactions (get-configuration experiment :test-after-n-interactions)))
    (when (and test-after-n-interactions (= (interaction-number interaction) test-after-n-interactions))
      (set-configuration experiment :learning-active nil :replace t)
      ;; reload the world with a different dataset
      (setf (world experiment)
            (make-instance 'clevr-world :data-sets '("valB")))
      (when (eql (get-configuration experiment :data-source) :extracted)
        ;; set the data path
        (set-data experiment :data-path
                  (parse-namestring (replace-char (namestring (find-data experiment :data-path)) #\A #\B))))
                  ;(make-pathname :directory
                  ;               (append (butlast (pathname-directory (find-data experiment :data-path)))
                  ;                       (list "valB-extracted")))))
      (format t "~%~%SWITCHING FROM CONDITION A TO CONDITION B. SWITCHED OFF LEARNING~%~%")))
  ;; regular interaction
  (before-interaction experiment)
  (do-interaction experiment)
  (after-interaction experiment))
