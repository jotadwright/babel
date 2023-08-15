(in-package :cle)

;; --------------
;; + Experiment +
;; --------------

(defclass cle-experiment (experiment)
  ()
  (:documentation "The experiment class."))

(defmethod initialize-instance :after ((experiment cle-experiment) &key)
  "Create the population and load the scenes from file."
  (set-configuration experiment :current-stage 0)
  (initialise-population experiment)
  (initialise-world experiment))

(defun initialise-world (experiment)
  "Initialise the world of the experiment by loading the given dataset."
  (let* ((dataset (get-configuration experiment :dataset))
         (dataset-split (get-configuration experiment :dataset-split))
         (scene-sampling (get-configuration experiment :scene-sampling))
         (available-channels (get-configuration experiment :available-channels)))
    (when (eql scene-sampling :deterministic)
      ;; load the scene ids
      (set-configuration experiment
                         :scene-ids (read-scene-ids
                                     (mkstr (make-pathname :directory
                                                           `(:relative ,dataset)
                                                           :name
                                                           (get-configuration experiment :data-fname)))))
      ;; set the current scene to the first
      (set-configuration experiment :current-scene-idx 0))
    ;; create a world object to load scenes into
    (setf (world experiment) (make-instance 'dataset-world
                                            :dataset dataset
                                            :dataset-split dataset-split
                                            :available-channels available-channels))))

(defun initialise-agent (experiment disabled-channels)
  "Creates and initialises an agent with sensors and calibrations for these sensors."
  (let* ((sensor-noise (determine-noise-in-sensor experiment
                                                  disabled-channels
                                                  (get-configuration experiment :sensor-noise)))
         (observation-noise (determine-noise-in-observation experiment
                                                            disabled-channels
                                                            (get-configuration experiment :sensor-noise)))
         (disabled-channels-hash (list-to-hash-table disabled-channels))
         (new-agent (make-instance 'cle-agent
                                   :experiment experiment
                                   :disabled-channels disabled-channels-hash
                                   :noise-in-each-sensor sensor-noise
                                   :noise-in-each-observation observation-noise)))
    new-agent))

(defun initialise-population (experiment)
  "Creates and initialises a population of agents."
  (let* ((disabled-channels-list (determine-disable-channels experiment
                                                             (get-configuration experiment :population-size)
                                                             (get-configuration experiment :disable-channels))))
    (setf (agents experiment)
          (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                for disabled-channels = (nth i disabled-channels-list)
                collect (initialise-agent experiment disabled-channels)))))
