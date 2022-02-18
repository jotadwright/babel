(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; FCG MONITORS
;(activate-monitor trace-fcg)
;; IRL MONITORS
;(activate-monitor trace-irl)
;; CLEVR EVALUATION MONITOR
;(activate-monitor trace-clevr-evaluation)



(evaluate-clevr-accuracy "val" :nr-of-scenes 1)