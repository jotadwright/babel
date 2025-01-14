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
  (initialise-population experiment)
  
  (initialise-clusters experiment))

(defun initialise-world (experiment)
  "Initialise the world of the experiment by loading the given dataset."
  (let ((world (case (get-configuration experiment :dataset-loader)
                 (:precomputed (make-instance 'precomputed-world
                                              :experiment experiment))
                 (:runtime (make-instance 'runtime-world
                                          :experiment experiment)))))
    (setf (world experiment) world)))

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

(defun initialise-population (experiment)
  "Creates and initialises a population of agents."
  (let* ((views-list (determine-views experiment (get-configuration experiment :dataset-view)))
         (disabled-channels-list (determine-disable-channels experiment
                                                             (get-configuration experiment :population-size)
                                                             (get-configuration experiment :disable-channels))))
    (setf (agents experiment)
          (loop for i from 0 to (- (get-configuration experiment :population-size) 1)
                for views = (nth i views-list)
                for disabled-channels = (nth i disabled-channels-list)
                collect (initialise-agent experiment views disabled-channels)))))

(defun initialise-clusters (experiment)
  (loop for idx from 0
        for agent in (agents experiment)
        for agent-id = (format nil "agent-~a" idx)
        for dataset-name = (first (get-configuration experiment :dataset))
        for dataset-split = (get-configuration experiment :dataset-split)
        for n-clusters = (get-configuration experiment :n-clusters)
        for fpath = (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2"
                                                                           "split-by-scenes"
                                                                           ,dataset-name 
                                                                           "clusters" 
                                                                           ,(format nil "~a-~a"
                                                                                    dataset-name
                                                                                    n-clusters)
                                                                           ,dataset-split
                                                                           )
                                                     :name agent-id
                                                     :type "json")
                                     cl-user:*babel-corpora*)
        for agent-data = (car (load-json fpath))
        for concepts = (loop for unique-id from 0
                             for cluster in (alexandria::hash-table-values agent-data)
                             for new-cxn = (make-cxn-json agent unique-id cluster)
                             do (setf (gethash (form new-cxn) (get-inventory (lexicon agent) :unassigned)) new-cxn))))

;; ------------------------------------------------
;; + Sets up an experiment with pretrained agents +
;; ------------------------------------------------

(defun load-json (file-path)
  "Load and parse a JSON file line by line using JZON.
Takes FILE-PATH as the path to the JSON file."
  (with-open-file (stream file-path :direction :input :external-format :utf-8 :element-type :default)
    (loop for line = (read-line stream nil nil)
          while line
          collect (let* ((data (jzon::parse line)))
                    data))))

(defun make-distribution-json (agent mean st-dev nr-of-samples)
  "Create a gaussian distribution that will be updated using welford's online algorithm."
  (let* ((history (list (list
                         0
                         mean
                         mean
                         st-dev)))
         (M2 (* (expt st-dev 2) nr-of-samples)))
    (make-instance 'gaussian-welford
                   :mean mean
                   :st-dev st-dev
                   :nr-of-samples nr-of-samples
                   :history history 
                   :M2 M2)))

(defun make-concept-json (agent cluster-data)
  "Creates a concept (based on combinations of distributions) for the given object."
  (let ((prototypes (loop for channel being the hash-keys of (gethash "concept" cluster-data)
                            using (hash-value distribution-data)
                          ;; create a distribution for each channel
                          for distribution = (make-distribution-json agent
                                                                    (gethash "mean" distribution-data)
                                                                    (gethash "st-dev" distribution-data)
                                                                    (gethash "samples" cluster-data))
                          ;; create a prototype for each channel
                          for new-prototype = (make-instance 'prototype
                                                             :channel (intern (upcase channel) :keyword)
                                                             :weight 100 ;; TODO: IMPORTANT HYPERPARAMETER
                                                             :weight-mode (get-configuration (experiment agent) :weight-update-strategy)
                                                             :distribution distribution)
                          ;; only create prototypes for enabled sensors
                          if (and (not (gethash channel (disabled-channels agent)))
                                  cluster-data)
                            collect new-prototype)))
    ;; create the concept
    (make-instance 'concept-distribution :prototypes (list->hash-table prototypes :key #'channel))))

(defun make-cxn-json (agent unique-id cluster-data)
  "Creates a new construction."
  (make-instance 'cxn
                 :form unique-id
                 :meaning (make-concept-json agent cluster-data)
                 :score (get-configuration (experiment agent) :initial-cxn-entrenchement)
                 :history (list (cons 0 0))))