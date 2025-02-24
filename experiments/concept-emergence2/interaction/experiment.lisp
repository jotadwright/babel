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
  (let ((world (case (get-configuration experiment :dataset-loader)
                 (:precomputed (make-instance 'precomputed-world
                                              :experiment experiment))
                 (:runtime (make-instance 'runtime-world
                                          :experiment experiment)))))
    (setf (world experiment) world)))

(defun initialise-population (experiment)
  "Creates and initialises a population of agents."
  (let* ((views-list (determine-views experiment (get-configuration experiment :dataset-view)))
         (disabled-channels-list (determine-disable-channels experiment
                                                             views-list
                                                             (get-configuration experiment :population-size)
                                                             (get-configuration experiment :disable-channels))))
    (setf (agents experiment)
          (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                for views = (nth i views-list)
                for disabled-channels = (nth i disabled-channels-list)
                collect (initialise-agent experiment views disabled-channels)))))

(defun initialise-agent (experiment views disabled-channels)
  "Creates and initialises an agent with sensors and calibrations for these sensors."
  ;; Not supported: can't disable features if you have multiple views
  (assert (not (and disabled-channels (> (length views) 1))))
  
  (let* (;; get all channels except the disabled ones
         (channels (set-difference (get-feature-set (world experiment) (first views))
                                   disabled-channels))
         ;; determine the noise in the sensors and observations
         (sensor-noise (determine-noise-in-sensor experiment
                                                  channels
                                                  (get-configuration experiment :sensor-noise)))
         (observation-noise (determine-noise-in-observation experiment
                                                            channels
                                                            (get-configuration experiment :observation-noise)))
         (new-agent (make-instance 'cle-agent
                                   :experiment experiment
                                   :views views
                                   :lexicon (make-instance 'lexicon :configuration (configuration experiment))
                                   :disabled-channels (list->hash-table disabled-channels)
                                   :noise-in-each-sensor sensor-noise
                                   :noise-in-each-observation observation-noise
                                   :usage-table (create-usage-table (get-configuration experiment :usage-table-window)))))
    new-agent))
