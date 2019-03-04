;;;; This is the main file for running the experiment

;;;; To evaluate an expression in Emacs, first place your cursor
;;;; at the closing parenthesis of the expression. Next, do
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

;;;; Create an instance of the experiment
(defparameter *experiment* (make-instance 'grounded-color-naming-game-experiment))

;;;; Run a single interaction of the experiment
(run-interaction *experiment*)

;;;; Run a series of experiments
(run-series *experiment* 2)  ;; note that the robot connection is closed after a series!

;;;; Destroy the experiment (disconnects robots)
(destroy *experiment*)

;;;; Run a batch of experiments and export data
(run-experiments '(
                   (baseline ((:population-size . 5)
                              (:export-lexicon-interval . 10)
                              (:silent . t)))
                   )
                 :number-of-interactions 500
                 :number-of-series 1) 

;;;; Create a graph from the exported data
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
; (setf nao-interface::*nao-servers* nil)




;; Functions for setting up the scene server
(start-scene-server) ; look at localhost:8000/svg

(generate-new-scene 3)

(stop-scene-server)