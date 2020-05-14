

;; (ql:quickload :propbank-english)
(in-package :propbank-english)

(activate-monitor trace-fcg)

(setf nlp-tools::*penelope-host* "http://localhost:5000")



;;; examples by Paul and Katrien

(comprehend-and-extract-frames "Paul thinks that Katrien is right.")
;; (comprehend-and-extract-frames "Katrien felt that Paul was right.")
;; (comprehend-and-extract-frames "Remi expected that Katrien thinks that Paul was right.")
;; (comprehend-and-extract-frames "Katrien thinks that Paul was right.")
;; (comprehend-and-extract-frames "The apple was eaten by the man.")
;; (comprehend-and-extract-frames "The man was eating an apple.")



;;; propbank-frames examples for roleset believe.01

;; (comprehend-and-extract-frames "She could hardly believe her eyes.")
;; (comprehend-and-extract-frames "I believe in the system.")
;; (comprehend-and-extract-frames "You believe that Seymour cray can do it again")
;; (comprehend-and-extract-frames "The declaration by Economy Minister Nestor Rapanelli is believed to be the first time such an action has been called for.")
;; (comprehend-and-extract-frames "For that matter , the Chinese display a willingness to believe in the auspiciousness of just about anything!")


;;; Sandbox with variations and additional examples

;; (comprehend-and-extract-frames "Winston could hardly believe that Seymour Cray can do it again.")
;; (comprehend-and-extract-frames "You believe that Seymour Cray can do it again.")
;; (comprehend-and-extract-frames "You could not believe that Seymour Cray can do it again.")

;; (comprehend-and-extract-frames "You strongly believe your eyes.")
;; (comprehend-and-extract-frames "You believe your eyes.")
;; (comprehend-and-extract-frames "Winston could hardly  believe his eyes.")
;; (comprehend-and-extract-frames "He strongly believes in the system.")
;; (comprehend-and-extract-frames "I fully believe in the system.")
;; (comprehend-and-extract-frames "You believe that Seymour Cray can do it again")
;; (comprehend-and-extract-frames "This is fully  believed to be the first time such an action has been called for.")


;; (comprehend-and-extract-frames "For that matter, the Chinese display a willingness to believe in the auspiciousness of just about anything!")
