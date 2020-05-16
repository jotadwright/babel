
;; (ql:quickload :propbank-english)
(in-package :propbank-english)

(activate-monitor trace-fcg)

(setf nlp-tools::*penelope-host* "http://localhost:5000")



;;; examples by Paul and Katrien

;;(comprehend-and-extract-frames "Paul thinks that Katrien is right.")
;; (comprehend-and-extract-frames "Katrien felt that Paul was right.")
;; (comprehend-and-extract-frames "Remi expected that Katrien thinks that Paul was right.")
;; (comprehend-and-extract-frames "Katrien thinks that Paul was right.")
;; (comprehend-and-extract-frames "The apple was eaten by the man.")
;; (comprehend-and-extract-frames "The man was eating an apple.")



;;; propbank-frames examples for roleset believe.01

;; (comprehend-and-extract-frames "Cathryn Rice could hardly believe her eyes.")
;; (comprehend-and-extract-frames "I believe in the system.")
;; (comprehend-and-extract-frames "You believe that Seymour Cray can do it again")
;; (comprehend-and-extract-frames "The declaration by Economy Minister Nestor Rapanelli is believed to be the first time such an action has been called for.")
;; (comprehend-and-extract-frames "For that matter, the Chinese display a willingness to believe in the auspiciousness of just about anything!")


;;; Additional examples for roleset believe.01

;; (comprehend-and-extract-frames "She believes his answer.")
;; (comprehend-and-extract-frames "The minster believes his answer.")

;; (comprehend-and-extract-frames "The minister could fully believe in the system.")
;; (comprehend-and-extract-frames "Winston barely believes in himself.")

;; (comprehend-and-extract-frames "You could believe that Seymour cray could do it again.")
;; (comprehend-and-extract-frames "The man could believe that Seymour cray could do it again.")
;; (comprehend-and-extract-frames "Winston could hardly believe that Seymour cray would say something like that.")

;; (comprehend-and-extract-frames "He is believed to be the first to discover a vaccine.")

;; (comprehend-and-extract-frames "The dog has a tendency to believe in everything.")
;; (comprehend-and-extract-frames "The Chinese have a strong tendency to believe he is right.")

;; (comprehend-and-extract-frames "It is believed that he lives.")
;; (comprehend-and-extract-frames "You could believe that he lives.")

;; (comprehend-and-extract-frames "This is believed")
;; (comprehend-and-extract-frames "He is believed.")

;; (comprehend-and-extract-frames "The story is believed by them")
;; (comprehend-and-extract-frames "It is believed by them")

;; (comprehend-and-extract-frames "He is believed by them.")
;; (comprehend-and-extract-frames "The large dogs are believed by them.")



;;



