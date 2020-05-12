

;; (ql:quickload :propbank-english)
(in-package :propbank-english)

(activate-monitor trace-fcg)

(setf nlp-tools::*penelope-host* "http://localhost:5000")

(comprehend-and-extract-frames "Paul thinks that Katrien is right.")



;; (comprehend-and-extract-frames "Katrien felt that Paul was right.")

;; (comprehend-and-extract-frames "Remi expected that Katrien thinks that Paul was right.")


;; (comprehend-and-extract-frames "Katrien thinks that Paul was right.")
;; (comprehend-and-extract-frames "The apple was eaten by the man.")
;; (comprehend-and-extract-frames "The man was eating an apple.")


