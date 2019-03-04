(ql:quickload :grounded-colour-naming-game-experiment)
(in-package :gcng)

;; ---------------------------------
;; + Running series of experiments +
;; + with different configurations +
;; ---------------------------------

(defun run-experiments (strategies
                         &key
                         (number-of-interactions 5)
                         (number-of-series 1)
                         (monitors
                          (list "export-communicative-success"
                                "export-lexicon-size"
                                "export-ontology-size"
                                "export-avg-forms-per-meaning"
                                "export-avg-meanings-per-form"))
                         (population-size 10)
                         (export-lexicon-interval 10))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
    :experiment-class 'grounded-color-naming-game-experiment 
    :number-of-interactions number-of-interactions
    :number-of-series number-of-series
    :monitors monitors
    :shared-configuration `((:population-size . ,population-size)
                            (:export-lexicon-interval . ,export-lexicon-interval))
    :configurations strategies
    :output-dir (babel-pathname :directory '("experiments" "robot-games" "robot-colour-naming-game" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names y-axis y1-max y2-max xlabel y1-label y2-label)
  ;; This function allows you to plot one or more measures for a single experiment
  ;; e.g. communicative success and lexicon size
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for measure-name in measure-names
          collect `("experiments" "robot-games" "robot-colour-naming-game" "raw-data" ,experiment-name ,measure-name))
    :average-windows 1000
    :plot-directory `("experiments" "robot-games" "robot-colour-naming-game" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :use-y-axis y-axis
    :y1-min 0
    :y1-max y1-max
    :y2-min 0
    :y2-max y2-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name (y-min 0) (y-max 1) xlabel y1-label y2-label)
  ;; This function allows you to compare a given measure accross different
  ;; experiments, e.g. comparing lexicon size
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot
    :raw-file-paths
    (loop for experiment-name in experiment-names
          collect `("tutorial" "language-games" "grounded-color-naming-game" "raw-data" ,experiment-name ,measure-name))
    :average-windows 1000
    :captions experiment-names
    :plot-directory '("tutorial" "language-games" "grounded-color-naming-game" "graphs")
    :error-bars '(:stdev)
    :error-bar-modes '(:lines)
    :y1-min y-min
    :y1-max y-max
    :x-label (if xlabel xlabel "Number of Games")
    :y1-label (when y1-label y1-label)
    :y2-label (when y2-label y2-label))
  (format t "~%Graphs have been created"))

;; -------
;; + RUN +
;; -------

(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl-in-web-browser)
  (activate-monitor trace-interaction-in-web-interface))

;; Running a single experiment
(defparameter *experiment* (make-instance 'grounded-color-naming-game-experiment))

(set-configuration *experiment* :export-lexicon-interval 1 :replace t)

(run-interaction *experiment*)

(run-series *experiment* 2)  ;; note that the robot connection is closed after a series!

(destroy *experiment*)

;; Run a batch of experiments
(run-experiments '(
                   (baseline ((:population-size . 5)
                              (:export-lexicon-interval . 10)
                              (:silent . t)))
                   )
                 :number-of-interactions 500
                 :number-of-series 1) 

(create-graph-for-single-strategy
 :experiment-name "baseline"
 :measure-names '("communicative-success"
                  "lexicon-size"
                  "ontology-size"
                  "forms-per-meaning"
                  "meanings-per-form")
 :y-axis '(1 2 2 2 2)
 :y1-max 1)


;; For setting up the robot
(setf *robot* (make-robot :type 'nao :ip "192.168.1.4" :port "7850"))

(stand *robot*)
(crouch *robot*)
(sit *robot*)

(take-picture *robot*)
(observe-scene *robot* :open t)

(point *robot* :right)

(look-up *robot* 12)
(robot-interface::look-down *robot* 10)
(robot-interface::look-right *robot* 15)


(disconnect-robot *robot*)
; (setf nao-interface::*nao-servers* nil)

;; For setting up the scene server
(start-scene-server)

(generate-new-scene 3)

(stop-scene-server)