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

    ;; 1. set new interaction as interaction
    (setf (interactions experiment) (list interaction))

    ;; 2. check if we need to switch the dataset split
    (switch-split experiment interaction)
    ;; Only switch conditions if training or testing
    (when (and (or (string= (get-configuration experiment :dataset-split) "train")
                   (string= (get-configuration experiment :dataset-split) "test"))
               (switch-condition-p experiment (get-configuration experiment :switch-condition)))
      (setup-next-condition experiment))

    ;; 3. run interaction script
    (interact experiment interaction :scene scene :topic topic :agents agents)
    ;; 4. determine outcome
    (setf (communicated-successfully interaction)
          (loop for agent in (interacting-agents interaction)
                always (communicated-successfully agent)))
    ;; 5. notify
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

(defun switch-split (experiment interaction &key (switch-to-val-x-times 8) (run-val-for-percent-of-total-interactions 1/100))
  "Switches the dataset between the training and test split.
  
   The experiment will run :nr-of-interactions.
    - `switch-to-val-x-times` determines how often the dataset will be switched from train to val (based on :nr-of-interactions)
    - `run-val-for-percent-of-total-interactions` determines how long the validation will run for (based on :nr-of-interactions)"
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
    ;; run validation each time for 1/xth of the interactions, then switch back to training
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
  "Notify the correct interaction-finished."
  (let* ((dataset-split (get-configuration experiment :dataset-split))
         (standard-measures (mapcar #'(lambda (monitor) (intern (string-upcase monitor)))
                                    (get-configuration experiment :split-independent-monitors)))
         (measures (mapcar #'(lambda (monitor) (intern (string-upcase (format nil "~a-~a" monitor dataset-split))))
                           (get-configuration experiment :split-dependent-monitors))))
    (and (not monitors::*monitor-notifications-disabled*)
         (loop for monitor-id in (monitors::active-monitors (monitors::get-event 'interaction-finished))
               for monitor = (monitors::get-monitor monitor-id)
               if (or (find monitor-id measures) (find monitor-id standard-measures))
                 do (monitors::handle-interaction-finished-event monitor
                                                                 monitor-id
                                                                 'interaction-finished
                                                                 experiment
                                                                 interaction
                                                                 (interaction-number interaction))))))
