;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;; Script for running a DEMO ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clg)
(in-package :clg)

(defparameter *configuration* (make-configuration
                               :entries '((:determine-interacting-agents-mode . :tutor-learner)
                                          (:question-sample-mode . :all)
                                          ;(:questions-per-challenge . 1000)
                                          (:scenes-per-question . 50)
                                          (:confidence-threshold . 1.1)
                                          (:tutor-sample-mode . :smart) ;; or :random
                                          (:cxn-decf-score . 0.4)
                                          (:cxn-inhibit-score . 0.1)
                                          (:primitives . :symbolic)
                                          (:learner-cxn-supplier . :hashed-and-scored)
                                          (:alignment-strategy . :lateral-inhibition)
                                          (:hide-type-hierarchy . nil)
                                          (:remove-cxn-on-lower-bound . t)
                                          (:composer-strategy . :store-past-scenes)
                                          (:th-link-repair-mode-comprehension . :no-path-required)
                                          (:th-link-repair-mode-formulation . :path-required))))

(progn
  (format t "~% Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)
  ;; instantiate an experiment
  (defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*)))


;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics)

  (run-series *experiment* 2500))


;; Option 2: run experiment with real-time tracing in the web-interface
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  ;(activate-monitor trace-tasks-and-processes)

  (run-interaction *experiment*)
  )

