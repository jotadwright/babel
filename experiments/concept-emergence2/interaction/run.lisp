(in-package :cle)

;; --------------------
;; + Interaction flow +
;; --------------------

(defmethod interact ((experiment cle-experiment) interaction &key scene topic agents)
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


    ;; check if we need to switch the dataset split
    (switch-split experiment interaction)
    ;; TODO do not condition during validation
    (when (switch-condition-p experiment (get-configuration experiment :switch-condition))
      (setup-next-condition experiment))
    
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
          
        (notify-interaction-by-split experiment interaction (interaction-number interaction)))
      ;;; if you do not set such a configuration, notify will always be notified
      (notify-interaction-by-split experiment interaction (interaction-number interaction)))
    (values interaction experiment)))

(defun switch-split (experiment interaction &key (switch-to-val-x-times 8) (run-val-for-percent-of-total-interactions 1/64))
  "Switches the dataset between the training and test split"
  (let ((current-interaction-number (interaction-number interaction)))
    ;; if training, switch to validation 8 times (every 1/8 of the interactions)
    (when (and (not (eq current-interaction-number 1))
               (string= (get-configuration experiment :dataset-split) "train")
               (eq (mod current-interaction-number (/ (get-configuration experiment :nr-of-interactions) switch-to-val-x-times)) 0))
      ;; LOG TO OUTPUT BROWSER
      (format t "~% ~a -> Switching from ~a to val" current-interaction-number  (get-configuration experiment :dataset-split))

      
      (set-configuration experiment :saved-interaction-number current-interaction-number)
      (setf (interaction-number interaction) 1)
      (set-configuration experiment :dataset-split "val")
      (set-configuration experiment :align nil)
      (initialise-world experiment)
      (return-from switch-split))
    ;; run validation each time for 1/64th of the interactions, then switch back to training
    (when (and (not (eq current-interaction-number 1))
               (string= (get-configuration experiment :dataset-split) "val")
               (eq (mod current-interaction-number (floor (* (get-configuration experiment :nr-of-interactions) run-val-for-percent-of-total-interactions))) 0))
      (format t "~% ~a -> Switching from ~a to train" current-interaction-number (get-configuration experiment :dataset-split))
      (set-configuration experiment :dataset-split "train")
      (set-configuration experiment :align t)
      ;; reset the interaction number to before the switch
      (setf (interaction-number interaction) (get-configuration experiment :saved-interaction-number))
      (initialise-world experiment))))

(defun notify-interaction-by-split (experiment interaction interaction-number)
  "Notify the correct interaction-finsihed"
  (let ((dataset-split (get-configuration experiment :dataset-split)))
    (cond ((string= dataset-split "train")
           (notify interaction-finished-train experiment interaction interaction-number))
          ((string= dataset-split "val")
           (notify interaction-finished-val experiment interaction interaction-number))
          ((string= dataset-split "test")
           (notify interaction-finished-test experiment interaction interaction-number)))))