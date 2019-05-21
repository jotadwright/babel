(in-package :mwm)

(defun clear-agent (agent)
  (setf (applied-cxns agent) nil
        (discriminative-set agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

;;;; before-interaction
(defgeneric before-interaction (experiment game-mode agents-mode tutor-mode)
  (:documentation "Initialize the interaction, depending on the configuration settings"))

(define-event context-determined (experiment mwm-experiment))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (agents-mode (eql :tutor-speaks))
                               (tutor-mode (eql :symbolic)))
  (let* ((clevr-context (random-elt (world experiment)))
         (mwm-context (clevr->mwm clevr-context
                                  :noise-amount (get-configuration experiment :noise-amount)
                                  :scale (get-configuration experiment :scale-world)
                                  :noise-prob (get-configuration experiment :noise-prob))))
    (notify context-determined experiment)
    (loop for agent in (interacting-agents experiment)
          do (setf (context agent)
                   (if (learnerp agent) mwm-context clevr-context))
          do (setf (clevr-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent) (random-elt (objects (context agent)))))
          do (clear-agent agent))))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (agents-mode (eql :tutor-speaks))
                               (tutor-mode (eql :continuous)))
  (let* ((clevr-context (random-elt (world experiment)))
         (mwm-context (clevr->mwm clevr-context
                                  :scale (get-configuration experiment :scale-world))))
    (if (and (get-configuration experiment :noise-amount)
             (get-configuration experiment :noise-prob))
      (loop for agent in (interacting-agents experiment)
            for context = (copy-object mwm-context)
            do (add-noise context
                          (get-configuration experiment :noise-prob)
                          (get-configuration experiment :noise-amount))
            do (setf (context agent) context))
      (loop for agent in (interacting-agents experiment)
            do (setf (context agent) mwm-context)))
    (loop for agent in (interacting-agents experiment)
          do (setf (clevr-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent) (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (notify context-determined experiment)))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-tutor))
                               agents-mode
                               (tutor-mode (eql :continuous)))
  (let* ((clevr-context (random-elt (world experiment)))
         (mwm-context (clevr->mwm clevr-context
                                  :scale (get-configuration experiment :scale-world))))
    (if (and (get-configuration experiment :noise-amount)
             (get-configuration experiment :noise-prob))
      (loop for agent in (interacting-agents experiment)
            for context = (add-noise (copy-object mwm-context)
                                     (get-configuration experiment :noise-prob)
                                     (get-configuration experiment :noise-amount))
            do (setf (context agent) context))
      (loop for agent in (interacting-agents experiment)
            do (setf (context agent) mwm-context)))
    (loop for agent in (interacting-agents experiment)
          do (setf (clevr-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent)
                     (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (notify context-determined experiment)))


;;;; do-interaction
(defgeneric do-interaction (experiment game-mode agents-mode tutor-mode)
  (:documentation "Run the appropriate interaction script, depending on the
   configuration settings"))

(defmethod do-interaction ((experiment mwm-experiment)
                           (game-mode (eql :tutor-learner))
                           (agents-mode (eql :tutor-speaks))
                           (tutor-mode (eql :symbolic)))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (loop while t
          for success = (conceptualise speaker)
          if success
          return success
          else
          do (before-interaction experiment game-mode agents-mode tutor-mode))
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

(defun conceptualise-until-success (agent game-mode agents-mode tutor-mode)
  (loop while t
        for mwm-context = (context agent)
        for clevr-context = (clevr-context agent)
        for concept-success
        = (and (setf (context agent) clevr-context)
               (with-disabled-monitors
                 (conceptualise-symbolic agent))
               (setf (context agent) mwm-context)
               (conceptualise agent))
        if concept-success
        do (progn (setf (discriminative-set agent) nil)
             (return concept-success))
        else
        do (before-interaction (experiment agent) game-mode
                               agents-mode tutor-mode)))

(defun conceptualise-with-re-entrance (agent game-mode agents-mode tutor-mode)
  (loop while t
        for mwm-context = (context agent)
        for clevr-context = (clevr-context agent)
        for concept-success
        = (and (setf (context agent) clevr-context)
               (with-disabled-monitors
                 (conceptualise-symbolic agent))
               (setf (context agent) mwm-context)
               (conceptualise agent)
               (re-entrance agent))
        if concept-success
        do (progn (setf (discriminative-set agent) nil)
             (return concept-success))
        else
        do (before-interaction (experiment agent) game-mode
                               agents-mode tutor-mode)))
                                

(defmethod do-interaction ((experiment mwm-experiment)
                           (game-mode (eql :tutor-learner))
                           (agents-mode (eql :tutor-speaks))
                           (tutor-mode (eql :continuous)))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  ;; In some scenes, the tutor is unable to discriminate a
  ;; topic using a single word on the symbolic level,
  ;; but is able to do so using the continuous categories
  ;; by stretching some concept. The learner gets very
  ;; confused by this, so we remove these scenes...
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (if (get-configuration experiment :tutor-re-entrance)
      (conceptualise-with-re-entrance speaker game-mode agents-mode tutor-mode)
      (conceptualise-until-success speaker game-mode agents-mode tutor-mode))
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

(defmethod do-interaction ((experiment mwm-experiment)
                           (game-mode (eql :tutor-tutor))
                           agents-mode
                           (tutor-mode (eql :continuous)))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (if (get-configuration experiment :tutor-re-entrance)
      (conceptualise-with-re-entrance speaker game-mode agents-mode tutor-mode)
      (conceptualise-until-success speaker game-mode agents-mode tutor-mode))
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))
    


;;;; after-interaction
(defgeneric after-interaction (experiment game-mode agents-mode tutor-mode)
  (:documentation "Finalize the interaction, depending on the configuration settings"))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (agents-mode (eql :tutor-speaks))
                               (tutor-mode (eql :symbolic)))
  (let* ((tutor (find 'tutor (interacting-agents experiment) :key #'id))
         (learner (find 'learner (interacting-agents experiment) :key #'id))
         (topic (find (id (topic tutor)) (objects (context learner)) :key #'id)))
    (when (discriminative-set tutor)
      (align-agent learner topic))))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (agents-mode (eql :tutor-speaks))
                               (tutor-mode (eql :continuous)))
  (let* ((tutor (find 'tutor (interacting-agents experiment) :key #'id))
         (learner (find 'learner (interacting-agents experiment) :key #'id))
         (topic (find (id (topic tutor)) (objects (context learner)) :key #'id)))
    (when (applied-cxns tutor)
      (align-agent learner topic))))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-tutor))
                               agents-mode
                               tutor-mode)
  ;; do nothing
  nil)


;;;; Interact
(defmethod interact ((experiment mwm-experiment)
                     interaction &key)
  (before-interaction experiment
                      (get-configuration experiment :game-mode)
                      (get-configuration experiment :determine-interacting-agents-mode)
                      (get-configuration experiment :tutor-lexicon))
  (do-interaction experiment
                  (get-configuration experiment :game-mode)
                  (get-configuration experiment :determine-interacting-agents-mode)
                  (get-configuration experiment :tutor-lexicon))
  (after-interaction experiment
                     (get-configuration experiment :game-mode)
                     (get-configuration experiment :determine-interacting-agents-mode)
                     (get-configuration experiment :tutor-lexicon)))
