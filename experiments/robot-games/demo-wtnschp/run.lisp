;;;; /run.lisp

; (ql:quickload :demo-wtnschp)

(in-package :demo-wtnschp)

(progn
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor trace-tasks-and-processes)
  ;(activate-monitor record-communicative-success)
  ;(activate-monitor display-communicative-success)
  ;(activate-monitor record-lexicon-size)
  ;(activate-monitor display-lexicon-size)
  )

(deactivate-all-monitors)

(defparameter *experiment* (make-instance 'demo-experiment))

(set-configuration *experiment* :input-form :text)
(set-configuration *experiment* :input-form :speech)

(set-configuration *experiment* :determine-interacting-agents-mode :robot-speaker-often)
(set-configuration *experiment* :determine-interacting-agents-mode :always-hearer)
(set-configuration *experiment* :determine-interacting-agents-mode :always-speaker)
(set-configuration *experiment* :determine-interacting-agents-mode :robot-hearer-often)
(set-configuration *experiment* :determine-interacting-agents-mode :random-role-for-single-agent)
(set-configuration *experiment* :determine-interacting-agents-mode :alternating)

(run-interaction *experiment*)

(run-series *experiment* 5)

(print-robot-lexicon *experiment*)

(destroy *experiment*)


;; For setting up the robot
(setf *robot* (make-robot :type 'nao :ip "192.168.1.4" :server-port "7850"))

(hear *robot* '("red" "green" "blue" "yellow"))

(go-to-posture *robot* :stand)
(go-to-posture *robot* :crouch)
(go-to-posture *robot* :sit)

(take-picture *robot*)
(observe-world *robot* :open t)

(look-direction *robot* :up 12)
(look-direction *robot* :down 12)

(disconnect-robot *robot*)
; (setf nao-interface::*nao-servers* nil)