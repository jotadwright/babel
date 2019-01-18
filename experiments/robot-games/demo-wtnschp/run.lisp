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

(defvar *exp* (make-instance 'demo-experiment))
(setf *exp* (make-instance 'demo-experiment))

(set-configuration *exp* :input-form :text)
(set-configuration *exp* :input-form :speech)

(set-configuration *exp* :determine-interacting-agents-mode :robot-speaker-often)
(set-configuration *exp* :determine-interacting-agents-mode :always-hearer)
(set-configuration *exp* :determine-interacting-agents-mode :always-speaker)
(set-configuration *exp* :determine-interacting-agents-mode :robot-hearer-often)
(set-configuration *exp* :determine-interacting-agents-mode :random-role-for-single-agent)
(set-configuration *exp* :determine-interacting-agents-mode :alternating)

(run-interaction *exp*)

(run-series *exp* 5)

(print-robot-lexicon *exp*)

(destroy *exp*)


;; For setting up the robot
(setf *robot* (make-robot :ip "192.168.1.4" :server-port "7850"))

(stand *robot*)
(crouch *robot*)
(sit *robot*)

(take-picture *robot*)
(observe-scene *robot* :open t)

(look-up *robot* 12)
(look-down *robot* 12)

(disconnect-robot *robot*)
; (setf nao-interface::*nao-servers* nil)