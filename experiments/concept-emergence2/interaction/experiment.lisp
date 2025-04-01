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

(defmethod initialise-world ((experiment cle-experiment))
  "Initialise the world of the experiment by loading the given dataset."
  (let ((world (case (get-configuration experiment :dataset-loader)
                 (:naming-game (make-instance 'naming-game-world
                                              :experiment experiment))
                 (:precomputed (make-instance 'precomputed-world
                                              :experiment experiment))
                 (:runtime (make-instance 'runtime-world
                                          :experiment experiment)))))
    (setf (world experiment) world)))

(defmethod initialise-population ((experiment cle-experiment))
  "Creates and initialises a population of agents."
  (let* ((views-list (determine-views experiment (get-configuration experiment :dataset-view))))
    ;; create initial agents (with possibly disabled sensors)
    (setf (agents experiment)
          (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                for views = (nth i views-list)
                collect (initialise-agent experiment views)))

    ;; initialise the social network
    (initialise-social-network experiment)
    
    ;; initialise neighbor q-values
    (loop for agent in (agents experiment)
          for other-agents = (social-network agent)
          do (loop for other in other-agents
                   do (initialise-neighbor-q-values agent other)))))

(defmethod initialise-agent ((experiment cle-experiment) views)
  "Creates and initialises an agent with sensors and calibrations for these sensors."
  (let* (;; get all channels except the disabled ones
         (new-agent (make-instance 'cle-agent
                                   :experiment experiment
                                   :views views
                                   :lexicon (make-instance 'lexicon :configuration (configuration experiment))
                                   :usage-table (create-usage-table (get-configuration experiment :usage-table-window)))))
    new-agent))
