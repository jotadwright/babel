(ql:quickload :clevr-evaluation)
(in-package :clevr-evaluation)

;; FCG MONITORS
;(activate-monitor trace-fcg)
;; IRL MONITORS
;(activate-monitor trace-irl)
;; CLEVR EVALUATION MONITOR
;(activate-monitor trace-clevr-evaluation)



(evaluate-clevr-accuracy "val")

(understand-utterance-in-scene
 "is there a metallic object left of the gray object that is behind the large cylinder that is in front of the green matte object?"
 "CLEVR_val_000004" "val")