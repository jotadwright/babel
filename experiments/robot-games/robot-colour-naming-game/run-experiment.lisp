;;;; This is the main file for running the experiment

;;;; To evaluate an expression in Emacs, place your cursor
;;;; at the closing parenthesis of the expression and do
;;;; C-x C-f.

;;;; Load the system
(ql:quickload :grounded-colour-naming-game-experiment)

;;;; Go into the package
(in-package :gcng)

;;;; Activate web monitors
(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl-in-web-browser)
  (activate-monitor trace-interaction-in-web-interface))

;;;; Open your browser at localhost:8000

;;;; Create a configuration object, holding the parameters of the experiment
(defparameter *configuration*
  (make-configuration
   :entries '((:min-context-size . 2)
              (:max-context-size . 5)
              (:population-size . 10)
              (:simulation-mode . t)
              (:trace-every-nth-interaction . 1))))

;;;; Create an instance of the experiment
(defparameter *experiment*
  (make-instance 'grounded-color-naming-game-experiment :configuration *configuration*))

;;;; Run a single interaction of the experiment
(run-interaction *experiment*)

;;;; Run a series of experiments
(run-series *experiment* 20)

;;;; Destroy the experiment (disconnects robots)
(destroy *experiment*)

;;;; Run the experiment for S amount of series,
;;;; with each serie containing N interactions.
;;;; At the same time, data will be collected and
;;;; exported to a subfolder of the experiment.
(run-experiments '(
                   (baseline ((:min-context-size . 2)
                              (:max-context-size . 5)
                              (:population-size . 10)
                              (:simulation-mode . t)
                              (:trace-every-nth-interaction . 1)))
                   )
                 :number-of-interactions 1000
                 :number-of-series 5) 

;;;; Create a graph from the exported data
;;;; Make sure that the experiment-name matches
;;;; what is specified in run-experiments.
;;;; This will create and open a PDF file.
(create-graph-for-single-strategy
 :experiment-name "baseline"
 :measure-names '("communicative-success"
                  "lexicon-size"
                  "ontology-size"
                  "forms-per-meaning"
                  "meanings-per-form")
 :y-axis '(1 2 2 2 2)
 :y1-max 1)






;;;; Functions for setting up the Nao robot
;;;; (not available unless you have the nao-interface package)
(setf *robot* (make-robot :type 'nao :ip "192.168.1.4" :port "7850"))

(stand *robot*)
(crouch *robot*)
(sit *robot*)

(take-picture *robot*)
(observe-scene *robot* :open t)

(point *robot* :right)

(look-down *robot* 10)
(look-right *robot* 15)

(disconnect-robot *robot*)




;;;; Functions for setting up the scene server
;;;; (not available unless yoou have the robot-scene-generator package)
(start-scene-server) ; look at localhost:8000/svg

(generate-new-scene 3)

(stop-scene-server)