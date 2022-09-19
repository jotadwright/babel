(in-package :fcg)

;; (activate-monitor trace-fcg)

(def-fcg-constructions sql-query-grammar
  
  (def-fcg-cxn car-cxn
               ((?car-word
                 (args (?x))
                 (sem-cat (sem-class object))
                 (syn-cat (lex-class noun)))
                <-
                (?car-word
                 (HASH meaning ((?x cars)))                     
                 --
                 (HASH form ((string ?the-word  "cars"))))))
  
  (def-fcg-cxn hybrid
               ((?hybrid-word
                 (args (?x))
                 (sem-cat (sem-class property))
                 (syn-cat (lex-class adjective)))
                <-
                (?hybrid-word
                 (HASH meaning ((?x hybride = "oui")))                     
                 --
                 (HASH form ((string ?hybrid-word  "hybrid"))))))

  (def-fcg-cxn which-x-are-y-cxn
               ((?clause-unit
                 (args (?x ?y))
                 (sem-cat (sem-class query))
                 (syn-cat (lex-class clause))
                 (subunits (?which-unit ?x-unit ?are-word ?y-unit)))
                <-
                (?which-unit
                 (HASH meaning ((SELECT * FROM ?x WHERE ?y)))                     
                 --
                 (HASH form ((string ?which-unit  "which"))))
                (?x-unit
                 (args (?x))
                 (sem-cat (sem-class object))
                 --
                 (syn-cat (lex-class noun)))
                (?are-word
                 --
                 (HASH form ((string ?are-word  "are"))))
                (?y-unit
                 (args (?y))
                 (sem-cat (sem-class property))
                 --
                 (syn-cat (lex-class adjective)))))

  (def-fcg-cxn how-many-x-are-y-cxn
               ((?clause-unit
                 (args (?x ?y))
                 (sem-cat (sem-class query))
                 (syn-cat (lex-class clause))
                 (subunits (?how-unit ?many-unit ?x-unit ?are-word ?y-unit)))
                <-
                (?how-unit
                 (HASH meaning ((SELECT COUNT(*) FROM ?x WHERE ?y)))                     
                 --
                 (HASH form ((string ?how-unit "how"))))
                (?many-unit
                 --
                 (HASH form ((string ?many-unit  "many"))))
                (?x-unit
                 (args (?x))
                 (sem-cat (sem-class object))
                 --
                 (syn-cat (lex-class noun)))
                (?are-word
                 --
                 (HASH form ((string ?are-word  "are"))))
                (?y-unit
                 (args (?y))
                 (sem-cat (sem-class property))
                 --
                 (syn-cat (lex-class adjective)))))

  )

;; (comprehend "which cars are hybrid")
;; (comprehend "how many cars are hybrid")
