
(ql:quickload :robot-concept-learning)
(in-package :robot-concept-learning)

(activate-monitor trace-interaction-in-web-interface)
;(deactivate-monitor trace-interaction-in-web-interface)

(activate-monitor robot-monitor)
;(deactivate-monitor robot-monitor)

;; --------------------
;; + Run interactions +
;; --------------------

(defparameter *experiment*
  (make-instance 'mwm-experiment))

(run-interaction *experiment*)

(run-series *experiment* 10)

(display-lexicon (first (population *experiment*)))
(disconnect-robot-agent *experiment*)

;; ------------------------
;; + Setting up the robot +
;; ------------------------

(defparameter *robot*
  (make-robot :type 'nao :ip "192.168.1.4"
              :server-port "1570"))

(go-to-posture *robot* :crouch)
(look-direction *robot* :down 1.0)

(take-picture *robot* :open t)
(observe-world *robot* :open t)

(disconnect-robot *robot*)

;(setf nao-interface::*nao-servers* nil)

