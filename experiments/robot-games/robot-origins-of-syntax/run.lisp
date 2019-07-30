;;;; /run.lisp

; (ql:quickload :robot-origins-of-syntax)

(in-package :roos)

(progn
  (activate-monitor trace-interaction-in-web-interface)
  (activate-monitor trace-tasks-and-processes)
  ;(activate-monitor trace-fcg)
  ;(activate-monitor trace-fcg-search-process)
  )

(progn
  (activate-monitor record-communicative-success)
  ;(activate-monitor display-communicative-success)
  (activate-monitor record-ontology-size)
  ;(activate-monitor display-ontology-size)
  (activate-monitor record-lexicon-size)
  ;(activate-monitor display-lexicon-size)
  (activate-monitor display-metrics)
  )

(deactivate-all-monitors)

(defvar *exp* (make-instance 'roos-experiment))
(setf *exp* (make-instance 'roos-experiment))

;; input as :text or :speech
(set-configuration *exp* :input-form :text)
;; (set-configuration *exp* :input-form :speech)

(run-interaction *exp*)

(run-series *exp* 10)

(scaffold-lexicon *exp*)
(activate-grammar *exp*)

(set-configuration *exp* :determine-interacting-agents-mode :robot-speaker-often)
(set-configuration *exp* :determine-interacting-agents-mode :always-hearer)
(set-configuration *exp* :determine-interacting-agents-mode :robot-hearer-often)
(set-configuration *exp* :determine-interacting-agents-mode :random-role-for-single-agent)

(destroy *exp*)



;; For showing the grammar
(defvar *grammar* (grammar (first (agents *exp*))))
(add-element (make-html *grammar*))
(mapcar #'(lambda (agent)
            (add-element (make-html (get-type-hierarchy (grammar agent))
                                    :weights? t
                                    :colored-edges-0-1 t
                                    :render-program "circo")))
        (population *exp*))

;; For showing the lexicon
(defvar *agent* (first (population *exp*)))
(show-lexicon *agent*)


;; For setting up the robot
(setf *robot* (make-robot :ip "192.168.1.2" :server-port "1570"))
(crouch *robot*)
(sit *robot*)
(take-picture *robot*)
(look-up-down *robot* 12)
(disconnect-robot *robot*)
; (setf nao-interface::*nao-servers* nil)