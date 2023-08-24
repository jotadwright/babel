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
