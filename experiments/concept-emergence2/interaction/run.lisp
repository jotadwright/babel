(in-package :cle)

;; --------------------
;; + Interaction flow +
;; --------------------

(defmethod interact ((experiment cle-experiment) interaction &key scene topic agents)
  (when (switch-condition-p experiment (get-configuration experiment :switch-condition))
    (setup-next-condition experiment))
  (before-interaction experiment :scene scene :topic topic :agents agents)
  (do-interaction experiment)
  (after-interaction experiment))

(defmethod run-interaction ((experiment cle-experiment)
                            &key (scene nil) (topic nil) (agents nil)
                            &allow-other-keys)
  "runs an interaction by increasing the interaction number"
  (let ((interaction (make-instance 'interaction
                                    :experiment experiment
                                    :interaction-number (if (interactions experiment)
                                                          (+ 1 (interaction-number
                                                                (car (interactions experiment))))
                                                          1))))
    ;; during training: store experiment every x interactions
    (when (and (equalp (get-configuration experiment :dataset-split) "train")
               (not (zerop (interaction-number interaction)))
               (get-configuration experiment :nr-of-interactions) ;; if set, then continue
               (zerop (mod (interaction-number interaction) (/ (get-configuration experiment :nr-of-interactions) 4))))
      (store-experiment experiment))

    ;; 1. set new interaction as interaction
    (setf (interactions experiment) (list interaction))
    ;; 2. run interaction script
    (interact experiment interaction :scene scene :topic topic :agents agents)
    ;; 3. determine outcome
    (setf (communicated-successfully interaction)
          (loop for agent in (interacting-agents interaction)
                always (communicated-successfully agent)))
    ;; 4. notify
    (if (get-configuration experiment :record-every-x-interactions)
      ;; different from :log-every-x-interactions!
      ;; log -> log information to standard output (for development)
      ;; output -> output results to disk
      (when 
          (or
           ;; If we set a configuration called record-every-x-interactions,
           ;; notify will only be fired every x interactions AND notify it the very first interaction
           (= (mod (interaction-number interaction) (get-configuration experiment :record-every-x-interactions)) 0) 
           (= (interaction-number interaction) 1))
          
        (notify interaction-finished experiment interaction (interaction-number interaction)))
      ;;; if you do not set such a configuration, notify will always be notified
      (notify interaction-finished experiment interaction (interaction-number interaction)))
    (values interaction experiment)))
