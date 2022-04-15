(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; FCG MONITORS
;(activate-monitor trace-fcg)
;; IRL MONITORS
;(activate-monitor trace-irl-verbose)
;; CLEVR EVALUATION MONITOR
;(activate-monitor trace-clevr-evaluation)

(deactivate-all-monitors)


(evaluate-clevr-accuracy "val" :nr-of-scenes 1 :nr-of-questions 1)

(understand-utterance-in-scene
 "is there a metallic object left of the gray object that is behind the large cylinder that is in front of the green matte object?"
 "CLEVR_val_000004" "val")