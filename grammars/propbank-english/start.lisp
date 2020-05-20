
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

;;; constructed test examples for roleset believe.01

;; (comprehend-and-extract-frames "She believes that he is right.")
;; (comprehend-and-extract-frames "Winston believes that he is right.")
;; (comprehend-and-extract-frames "The dog believes that he is right.")
;; (comprehend-and-extract-frames "Dogs believe that he is right.") 
;; (comprehend-and-extract-frames "She believes he is right.")
;; (comprehend-and-extract-frames "She does not believe that he is right.")
;; (comprehend-and-extract-frames "She strongly believes that he is right.")

;; (comprehend-and-extract-frames "She could believe that he is right.")
;; (comprehend-and-extract-frames "Winston could believe that he is right.")
;; (comprehend-and-extract-frames "The dog could believe that he is right.")
;; (comprehend-and-extract-frames "Dogs could believe that he is right.") 
;; (comprehend-and-extract-frames "She could believe he is right.")
;; (comprehend-and-extract-frames "She could barely believe that he is right.")
;; (comprehend-and-extract-frames "She could not believe that he is right.")

;; (comprehend-and-extract-frames "She believes in it.")
;; (comprehend-and-extract-frames "Winston believes in it.")
;; (comprehend-and-extract-frames "The dog believes in it.")
;; (comprehend-and-extract-frames "Dogs believe in it.")
;; (comprehend-and-extract-frames "She does not believe in it.")
;; (comprehend-and-extract-frames "She strongly believes in it.")

;; (comprehend-and-extract-frames "She could believe in it.")
;; (comprehend-and-extract-frames "Winston could believe in it.")
;; (comprehend-and-extract-frames "The dog could believe in it.")
;; (comprehend-and-extract-frames "Dogs could believe in it.")
;; (comprehend-and-extract-frames "She could not believe in it.")
;; (comprehend-and-extract-frames "She could strongly believe in it.")

;; (comprehend-and-extract-frames "She could believe the article.")
;; (comprehend-and-extract-frames "Winston could believe the article.")
;; (comprehend-and-extract-frames "The dog could believe the article.")
;; (comprehend-and-extract-frames "Dogs believe the article.") 
;; (comprehend-and-extract-frames "She could not believe the article.")
;; (comprehend-and-extract-frames "She could strongly believe the article.")

;; (comprehend-and-extract-frames "She believes the article.")
;; (comprehend-and-extract-frames "Winston believes the article.")
;; (comprehend-and-extract-frames "The dog believes the article.")
;; (comprehend-and-extract-frames "Dogs believe the article.")
;; (comprehend-and-extract-frames "She does not believe the article.")
;; (comprehend-and-extract-frames "She strongly believes the article.")

;; (comprehend-and-extract-frames "She could believe him.")

;; (comprehend-and-extract-frames "She is believed to be right.")
;; (comprehend-and-extract-frames "Winston is believed to be right.")
;; (comprehend-and-extract-frames "The dog is believed to be right.")
;; (comprehend-and-extract-frames "Dogs are believed to be right.") 
;; (comprehend-and-extract-frames "This is believed to be right.")
;; (comprehend-and-extract-frames "The article is not believed to be right.")
;; (comprehend-and-extract-frames "The article could not be believed to be right") ;?check

;; (comprehend-and-extract-frames "The Chinese display a willingness to believe in the auspiciousness of just about anything!")
;; (comprehend-and-extract-frames "The man has a strong tendency to believe it.")
;; (comprehend-and-extract-frames "Winston has a strong tendency to believe the story.")
;; (comprehend-and-extract-frames "The dog has a strong tendency to believe the story.")
;; (comprehend-and-extract-frames "Politicians have a tendency to believe the story.")

;; (comprehend-and-extract-frames "She shows a tendency to believe that he is right.")
;; (comprehend-and-extract-frames "She has shown a tendency to believe that he is right.")

;; (comprehend-and-extract-frames "She is willing to believe in him.")
;; (comprehend-and-extract-frames "Winston is willing to believe in him.")
;; (comprehend-and-extract-frames "The dog is willing to believe in him.")
;; (comprehend-and-extract-frames "Dogs are willing to believe in him.")
;; (comprehend-and-extract-frames "She should be willing to believe in him.") ;check 
;; (comprehend-and-extract-frames "She should not be willing to believe in him.") ;check 

;; (comprehend-and-extract-frames "She is willing to believe that he is right.")
;; (comprehend-and-extract-frames "Winston is willing to believe that he is right.")
;; (comprehend-and-extract-frames "The dog is willing to believe that he is right.")
;; (comprehend-and-extract-frames "Dogs are willing to believe that he is right.")
;; (comprehend-and-extract-frames "She should be willing to believe that he is right."); check
;; (comprehend-and-extract-frames "She should not be willing to believe that he is right.") ;check

;; (comprehend-and-extract-frames "She is willing to believe him.") 
;; (comprehend-and-extract-frames "Winston is willing to believe it.") 
;; (comprehend-and-extract-frames "The dog is willing to believe him.")
;; (comprehend-and-extract-frames "Dogs are willing to believe him.")
;; (comprehend-and-extract-frames "She should be willing to believe him.") ;check
;; (comprehend-and-extract-frames "She should not be willing to believe him.") ;check

;; (comprehend-and-extract-frames "She is willing to believe that he is right.")
;; (comprehend-and-extract-frames "Winston is willing to believe that he is right.")
;; (comprehend-and-extract-frames "The dog is willing to believe that he is right.")
;; (comprehend-and-extract-frames "Dogs are willing to believe that he is right.")
;; (comprehend-and-extract-frames "She should be willing to believe that he is right."); check
;; (comprehend-and-extract-frames "She should not be willing to believe that he is right.") ;check

;; (comprehend-and-extract-frames "She intends to believe that he is right.")
;; (comprehend-and-extract-frames "Winston intends to believe that he is right.")
;; (comprehend-and-extract-frames "The dog intends to believe that he is right.")
;; (comprehend-and-extract-frames "Dogs intend to believe that he is right.")
;; (comprehend-and-extract-frames "She tends to not believe that he is right.")

;; (comprehend-and-extract-frames "She intends to believe in the truth.")
;; (comprehend-and-extract-frames "Winston intends to believe in the truth.")
;; (comprehend-and-extract-frames "The dog intends to believe in the truth.")
;; (comprehend-and-extract-frames "Dogs intend to believe in the truth.")
;; (comprehend-and-extract-frames "She tends to not believe in the truth.")

;; (comprehend-and-extract-frames "She tends to believe him.")

;; (comprehend-and-extract-frames "It is believed that he is right.")
;; (comprehend-and-extract-frames "It is believed to be true.")
;; (comprehend-and-extract-frames "It should not be believed that he is right.") 

;; (comprehend-and-extract-frames "She is believed.")
;; (comprehend-and-extract-frames "Winston is believed.")
;; (comprehend-and-extract-frames "The dog is believed.")
;; (comprehend-and-extract-frames "Dogs are believed.") 
;; (comprehend-and-extract-frames "She is not believed.")
;; (comprehend-and-extract-frames "She is strongly believed.")
;; (comprehend-and-extract-frames "She should really be believed.") 
;; (comprehend-and-extract-frames "She should not be believed.") 

;; (comprehend-and-extract-frames "She is believed by them.")
;; (comprehend-and-extract-frames "Winston is believed by them.")
;; (comprehend-and-extract-frames "The dog is believed by them.")
;; (comprehend-and-extract-frames "Dogs are believed by them.") ;check
;; (comprehend-and-extract-frames "She is not believed by them.")
;; (comprehend-and-extract-frames "She is barely believed by them.")
;; (comprehend-and-extract-frames "She should be believed by them.")
;; (comprehend-and-extract-frames "She should not readily be believed by them.") 

;; (comprehend-and-extract-frames "She does not say it unless she honestly believes it.")



;; tests for questions

;; (comprehend-and-extract-frames "You believe it.")
;; (comprehend-and-extract-frames "Would you believe it?")



;;; Constructed examples for arg-m NER tests

;; (comprehend-and-extract-frames "At 5 o'clock, I still believe that Trump is right.")
;; (comprehend-and-extract-frames "In 2020 Trump's claims cannot be believed.")
;; (comprehend-and-extract-frames "In America, many people still believe that Trump is right.")


;;; Constructed examples for generalization tests

;; (comprehend-and-extract-frames "He will not do it unless she believes the story.")




