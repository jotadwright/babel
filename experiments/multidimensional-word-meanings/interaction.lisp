(in-package :mwm)

(defun clear-agent (agent)
  (setf (applied-cxns agent) nil
        (discriminative-set agent) nil
        (utterance agent) nil
        (communicated-successfully agent) nil
        (parsed-meaning agent) nil))

;;;; before-interaction
(defgeneric before-interaction (experiment game-mode tutor-mode)
  (:documentation "Initialize the interaction, depending on the configuration settings"))

(define-event context-determined (experiment mwm-experiment))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (tutor-mode (eql :symbolic)))
  (let* ((data-source (get-configuration experiment :data-source))
         (clevr-context (random-scene (world experiment)))
         (mwm-context (clevr->mwm clevr-context
                                  :scale (get-configuration experiment :scale-world)))
         (continuous-context
          (when (eql data-source :continuous-clevr)
            (clevr->continuous clevr-context
                               :directory (get-configuration experiment :data-path)))))
    (loop for agent in (interacting-agents experiment)
          do (setf (context agent)
                   (if (learnerp agent)
                     (if (eql data-source :clevr)
                       mwm-context
                       (if (> (length (objects continuous-context)) 1)
                         continuous-context nil))
                     clevr-context))
          do (setf (symbolic-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent)
                     (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (unless (context (hearer experiment))
      (before-interaction experiment game-mode tutor-mode))
    (notify context-determined experiment)))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (tutor-mode (eql :continuous)))
  (let* ((data-source (get-configuration experiment :data-source))
         (clevr-context (random-scene (world experiment)))
         (mwm-context (clevr->mwm clevr-context :scale (get-configuration experiment :scale-world)))
         (continuous-context
          (when (eql data-source :continuous-clevr)
            (clevr->continuous clevr-context
                               :directory (get-configuration experiment :data-path)))))
    ;; if using clevr and noise, add noise
    (if (and (get-configuration experiment :noise-amount)
             (get-configuration experiment :noise-prob)
             (eql data-source :clevr))
      (loop for agent in (interacting-agents experiment)
            for context = (copy-object mwm-context)
            do (add-noise context
                          (get-configuration experiment :noise-prob)
                          (get-configuration experiment :noise-amount))
            do (setf (context agent) context))
      ;; else if using clevr, give both agents the same scene
      ;; otherwise, give tutor the clevr-scene and learner
      ;; the continuous scene
      ;; some continuous scenes have only one object. Skip these...
      (if (eql data-source :clevr)
        (loop for agent in (interacting-agents experiment)
              do (setf (context agent) mwm-context))
        (if (> (length (objects continuous-context)) 1)
          (setf (context (tutor experiment)) mwm-context
                (context (learner experiment)) continuous-context)
          (before-interaction experiment game-mode tutor-mode))))
    (loop for agent in (interacting-agents experiment)
          do (setf (symbolic-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent)
                     (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (notify context-determined experiment)))

(defmethod before-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-tutor))
                               (tutor-mode (eql :continuous)))
  (let* ((clevr-context (random-scene (world experiment)))
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
          do (setf (symbolic-context agent) clevr-context)
          do (setf (topic agent)
                   (when (speakerp agent)
                     (random-elt (objects (context agent)))))
          do (clear-agent agent))
    (notify context-determined experiment)))


;;;; do-interaction
(defgeneric do-interaction (experiment game-mode tutor-mode)
  (:documentation "Run the appropriate interaction script, depending on the
   configuration settings"))

(defmethod do-interaction ((experiment mwm-experiment)
                           (game-mode (eql :tutor-learner))
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
          do (before-interaction experiment game-mode tutor-mode))
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))

(defun conceptualise-until-success (agent game-mode tutor-mode)
  "The tutor will always first try to conceptualise on the
   symbolic level. If this already fails, the scene is skipped
   and another one is loaded. If it succeeds, the tutor
   conceptualises using its continuous-valued lexion."
  (loop while t
        for mwm-context = (context agent)
        for clevr-context = (symbolic-context agent)
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
        do (before-interaction (experiment agent) game-mode tutor-mode)))

(defun conceptualise-with-re-entrance (agent game-mode tutor-mode)
  "The tutor first tries to conceptualise on the symbolic level,
   next on the continuous level and finally the tutor also does
   re-entrance."
  (loop while t
        for mwm-context = (context agent)
        for clevr-context = (symbolic-context agent)
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
        do (before-interaction (experiment agent) game-mode tutor-mode)))
                                

(defmethod do-interaction ((experiment mwm-experiment)
                           (game-mode (eql :tutor-learner))
                           (tutor-mode (eql :continuous)))
  "The tutor conceptualises the topic and produces
   one or multiple words. The hearer tries to parse
   and interpret the utterance. If both succeed and
   the interpretation is correct, the interaction is
   a success. Adoption is handled together with
   alignment."
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (if (get-configuration experiment :tutor-re-entrance)
      (conceptualise-with-re-entrance speaker game-mode tutor-mode)
      (conceptualise-until-success speaker game-mode tutor-mode))
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
                           (tutor-mode (eql :continuous)))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
    (if (get-configuration experiment :tutor-re-entrance)
      (conceptualise-with-re-entrance speaker game-mode tutor-mode)
      (conceptualise-until-success speaker game-mode tutor-mode))
    (produce-word speaker)
    (when (utterance speaker)
      (setf (utterance hearer) (utterance speaker))
      (when (and (parse-word hearer)
                 (interpret hearer)
                 (determine-success speaker hearer))
        (setf (communicated-successfully speaker) t
              (communicated-successfully hearer) t)))))
    


;;;; after-interaction
(defgeneric after-interaction (experiment game-mode tutor-mode)
  (:documentation "Finalize the interaction, depending on the configuration settings"))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (tutor-mode (eql :symbolic)))
  (let* ((tutor (find 'tutor (interacting-agents experiment) :key #'id))
         (learner (find 'learner (interacting-agents experiment) :key #'id))
         (topic (if (eql (get-configuration experiment :data-source) :clevr)
                  (find (id (topic tutor)) (objects (context learner)) :key #'id)
                  (closest-to-topic tutor (context learner)))))
    (when (discriminative-set tutor)
      (align-agent learner topic))))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-learner))
                               (tutor-mode (eql :continuous)))
  (let* ((tutor (tutor experiment))
         (learner (learner experiment))
         (topic (if (eql (get-configuration experiment :data-source) :clevr)
                  (find (id (topic tutor)) (objects (context learner)) :key #'id)
                  (closest-to-topic tutor (context learner)))))
    (when (applied-cxns tutor)
      (align-agent learner topic))))

(defmethod after-interaction ((experiment mwm-experiment)
                               (game-mode (eql :tutor-tutor))
                               tutor-mode)
  ;; do nothing
  nil)


;;;; Interact
(defmethod interact ((experiment mwm-experiment)
                     interaction &key)
  (let ((game-mode (get-configuration experiment :game-mode))
        (tutor-mode (get-configuration experiment :tutor-lexicon)))
    (before-interaction experiment game-mode tutor-mode)
    (do-interaction experiment game-mode tutor-mode)
    (after-interaction experiment game-mode tutor-mode)))
