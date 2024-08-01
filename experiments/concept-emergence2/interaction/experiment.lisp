(in-package :cle)

;; --------------
;; + Experiment +
;; --------------

(defclass cle-experiment (experiment)
  ()
  (:documentation "The experiment class."))

(defmethod initialize-instance :after ((experiment cle-experiment) &key)
  "Create the population and load the scenes from file."
  (set-seed (get-configuration experiment :seed))
  (set-configuration experiment :current-stage 0)
  ;; world needs to be setup first
  (initialise-world experiment)
  ;; then setup population
  (initialise-population experiment))

(defun initialise-world (experiment)
  "Initialise the world of the experiment by loading the given dataset."
  (setf (world experiment) (make-instance 'world
                                          :dataset-name (get-configuration experiment :dataset)
                                          :dataset-split (get-configuration experiment :dataset-split)
                                          :feature-set (get-configuration experiment :feature-set)
                                          :scene-sampling (get-configuration experiment :scene-sampling)
                                          :data-fname (get-configuration experiment :data-fname))))

(defun initialise-agent (experiment disabled-channels)
  "Creates and initialises an agent with sensors and calibrations for these sensors."
  (let* (;; get all channels except the disabled ones
         (channels (set-difference (get-feature-set (world experiment)) disabled-channels))
         ;; determine the noise in the sensors and observations
         (sensor-noise (determine-noise-in-sensor experiment
                                                  channels
                                                  (get-configuration experiment :sensor-noise)))
         (observation-noise (determine-noise-in-observation experiment
                                                            channels
                                                            (get-configuration experiment :observation-noise)))
         (new-agent (make-instance 'cle-agent
                                   :experiment experiment
                                   :disabled-channels (list-to-hash-table disabled-channels)
                                   :noise-in-each-sensor sensor-noise
                                   :noise-in-each-observation observation-noise
                                   :usage-table (create-usage-table (get-configuration experiment :usage-table-window)))))
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
