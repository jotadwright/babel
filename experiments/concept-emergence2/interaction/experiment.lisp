(in-package :cle)

;; --------------
;; + Experiment +
;; --------------

(defclass cle-experiment (experiment)
  ()
  (:documentation "The experiment class."))

(defmethod initialize-instance :after ((experiment cle-experiment) &key)
  "Create the population and load the scenes from file."
  ;; 0. read data
  (let* ((dataset (get-configuration experiment :dataset))
         (dataset-split (get-configuration experiment :dataset-split))
         (fname (get-configuration experiment :data-fname))
         (available-channels (get-configuration experiment :available-channels))
         (fpath (mkstr (make-pathname :directory `(:relative ,dataset)
                                      :name fname))))
    (when fname
      ;; load the scene ids
      (set-configuration experiment :scene-ids (read-scene-ids fpath))
      ;; set the current scene to the first
      (set-configuration experiment :current-scene-idx 0))
    ;; initialise the population
    (let* ((disabled-channels-list (determine-disable-channels experiment (get-configuration experiment :disable-channels))))
      (setf (agents experiment)
            (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                  for disabled-channels = (nth i disabled-channels-list)
                  for sensor-noise = (determine-noise-in-sensor experiment
                                                                disabled-channels
                                                                (get-configuration experiment :sensor-noise))
                  for observation-noise = (determine-noise-in-observation experiment
                                                                          disabled-channels
                                                                          (get-configuration experiment :sensor-noise))
                  collect (make-instance 'cle-agent
                                         :experiment experiment
                                         :disabled-channels disabled-channels
                                         :noise-in-each-sensor sensor-noise
                                         :noise-in-each-observation observation-noise))))
    ;; create a world object to load scenes into
    (setf (world experiment) (make-instance 'dataset-world
                                            :dataset dataset
                                            :dataset-split dataset-split
                                            :available-channels available-channels))))
