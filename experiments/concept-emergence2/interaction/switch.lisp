(in-package :cle)

;; --------------------------------
;; + Whether to switch conditions +
;; --------------------------------
(defgeneric switch-condition-p (experiment switch-condition)
  (:documentation "Predicate that checks whether the experiment should advance to the next condition"))

(defmethod switch-condition-p ((experiment cle-experiment)
                               (switch-condition (eql :none)))
  "Never switch conditions"
  nil)

(defmethod switch-condition-p ((experiment cle-experiment)
                               (switch-condition (eql :after-n-interactions)))
  "Switch when N interactions have been played."
  (let ((switch-condition-interval (get-configuration experiment :switch-conditions-after-n-interactions))
        (current-interaction-number (interaction-number (current-interaction experiment))))
    (and (= (mod current-interaction-number switch-condition-interval) 0)
         (stages-left-p experiment))))

(defun stages-left-p (experiment)
  (nth (get-configuration experiment :current-stage) (get-configuration experiment :stage-parameters)))

;; --------------------
;; + General switcher +
;; --------------------
(defmethod setup-next-condition ((experiment cle-experiment))
  (let* ((current-stage (get-configuration experiment :current-stage))
         (next-stage (1+ current-stage))
         (params (nth current-stage (get-configuration experiment :stage-parameters))))
    ;; PART 1: logging the previous stage
    ;; message
    (format t "~%~%SWITCHING FROM CONDITION ~a TO CONDITION ~a~%~%" current-stage next-stage)
    ;; store history
    (store-experiment experiment)
    ;; set the new stage
    (set-configuration experiment :current-stage next-stage)

    ;; PART 2: the possible switches
    ;; check if channels need to be disabled
    (switch-disable-channels experiment params)
    ;; check if half of the population gets some channels disabled
    (switch-disable-channels-half experiment params)
    ;; check if need to add agents
    (switch-add-agents experiment params)
    ;; check if need to change alignment
    (switch-alignment experiment params)
    ;; check if dataset changes
    (switch-dataset experiment params)))

;; -----------------
;; + The switchers +
;; -----------------
(defmethod switch-disable-channels ((experiment cle-experiment)
                                    (params list))
  "Disables a specified list of sensors (or a random amount of sensors) for the entire population."
  (when (assoc :switch-disable-channels params)
    (let* ((change (assqv :switch-disable-channels params)) ;; change is either a list or a number
           (channels (if (numberp change)
                       ;; disable n sensors
                       (random-elts (get-configuration experiment :available-channels) change)
                       ;; disable the given list of features
                       change)))
      (loop for agent in (agents experiment)
            do (loop for channel in channels
                     do (switch-channel-availability agent channel))))))

(defmethod switch-disable-channels-half ((experiment cle-experiment)
                                         (params list))
  "Disables a specified list of sensors (or a random amount of sensors) for the half of the population."
  (when (assoc :switch-disable-channels-half params)
    (let* ((change (assqv :switch-disable-channels-half params)) ;; change is either a list or a number
           (channels (if (numberp change)
                       ;; disable n sensors
                       (random-elts (get-configuration experiment :available-channels) change)
                       ;; disable the given list of features
                       change))
           (population-size (length (agents experiment)))
           (population-half (first-n (floor (/ population-size 2)) (agents experiment))))
      (loop for agent in population-half
            do (loop for channel in channels
                     do (switch-channel-availability agent channel))))))

(defmethod switch-dataset ((experiment cle-experiment)
                           (params list))
  "Switches the dataset."
  (when (assoc :switch-dataset params)
    (set-configuration experiment :dataset (assqv :switch-dataset params))
    (set-configuration experiment :dataset-split (assqv :switch-dataset-split params))
    (set-configuration experiment :data-fname (assqv :switch-data-fname params))
    (set-configuration experiment :scene-sampling (assqv :switch-scene-sampling params))
    (set-configuration experiment :topic-sampling (assqv :switch-topic-sampling params))
    (set-configuration experiment :available-channels (assqv :switch-available-channels params))
    (initialise-world experiment)))

(defmethod switch-add-agents ((experiment cle-experiment)
                              (params list))
  "Adds brand new agents to the population."
  (when (assoc :switch-add-agents params)
    (let* ((amount-of-agents (assqv :switch-add-agents params))
           (disabled-channels-list (determine-disable-channels experiment
                                                               amount-of-agents
                                                               (get-configuration experiment :disable-channels)))
           (new-agents  (loop for i from 0 to (- amount-of-agents 1)
                              for disabled-channels = (nth i disabled-channels-list)
                              collect (initialise-agent experiment disabled-channels))))
      (set-configuration experiment :population-size (+ (get-configuration experiment :population-size)
                                                        amount-of-agents))
      (setf (agents experiment) (append (agents experiment) new-agents)))))
            

(defmethod switch-alignment ((experiment cle-experiment)
                             (params list))
  "Switches whether agents perform alignment or not."
  (when (assoc :switch-alignment params)
    (set-configuration experiment :align (not (get-configuration experiment :align)))))
