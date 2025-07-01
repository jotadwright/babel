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
         (disabled-features-list (determine-disable-features experiment
                                                             views-list
                                                             (get-configuration experiment :population-size)
                                                             (get-configuration experiment :disable-features))))
    (setf (agents experiment)
          (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                for views = (nth i views-list)
                for disabled-features = (nth i disabled-features-list)
                collect (initialise-agent experiment views disabled-features)))
    
    (initialise-social-network experiment)

    ;; initialise neighbour q-values
    (loop for agent in (agents experiment)
          for other-agents = (social-network::social-network agent)
          do (loop for other in other-agents
                   do (partner-selection::initialise-neighbours-q-values agent other)))))

(defun initialise-agent (experiment views disabled-features)
  "Creates and initialises an agent with sensors and calibrations for these sensors."
  ;; Not supported: can't disable features if you have multiple views
  (assert (not (and disabled-features (> (length views) 1))))
  
  (let* (;; get all features except the disabled ones
         (features (set-difference (get-feature-set (world experiment) (first views))
                                   disabled-features))
         ;; determine the noise in the sensors and observations
         (sensor-noise (determine-noise-in-sensor experiment
                                                  features
                                                  (get-configuration experiment :sensor-noise)))
         (observation-noise (determine-noise-in-observation experiment
                                                            features
                                                            (get-configuration experiment :observation-noise)))
         (new-agent (make-instance 'cle-agent
                                   :experiment experiment
                                   :views views
                                   :lexicon (make-instance 'lexicon :configuration (configuration experiment))
                                   :disabled-features (list->hash-table disabled-features)
                                   :noise-in-each-sensor sensor-noise
                                   :noise-in-each-observation observation-noise
                                   :usage-tracker (create-usage-tracker (get-configuration experiment :usage-tracker-window)))))
    new-agent))

(defmethod initialise-social-network (experiment)
  (let ((topology (get-configuration experiment :network-topology)))
    (case topology
      ;; fully-connected
      (:fully-connected (social-network::create-fully-connected-network (agents experiment)))
      ;; regular
      (:regular (social-network::create-regular-population-network (agents experiment) (get-configuration experiment :local-connectivity)))
      ;; small-world
      (:small-world (progn
                      (social-network::create-regular-population-network (agents experiment) (get-configuration experiment :local-connectivity))
                      (social-network::rewire-regular-network (agents experiment) (get-configuration experiment :rewiring-probability))))
      (error "Invalid or missing network topology: ~S. Expected one of: :fully-connected, :regular, :small-world."
             topology))))

