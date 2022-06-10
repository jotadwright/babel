(ql:quickload :fcg)

(in-package :fcg)

(activate-monitor trace-fcg)

(configure-grammar *fcg-constructions*)

(def-fcg-constructions german-case-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set)
                  (case sequence))
  :fcg-configurations ((:max-nr-of-nodes . 40000)
                       (:hide-features footprints sem-cat form boundaries)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network :connected-structure)
                       ;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :cxn-sets) ;; returns all cxns at once
                       (:node-tests :mal-cxn-applied :restrict-search-depth :restrict-nr-of-nodes :check-duplicate)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-sets) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       ;; cxn sets
                       (:parse-order cxn  mal-cxn)
                       (:production-order cxn mal-cxn )
                       ;; goal tests
                       (:production-goal-tests
                        :no-applicable-cxns :connected-structure
                        :no-meaning-in-root)))


(defmethod cip-node-test ((node cip-node) (mode (eql :mal-cxn-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'mal-cxn)
    (and (push 'mal-cxn-applied (statuses node))
         t)
      t
      ))


#|(defmethod cip-node-test ((node cip-node) (mode (eql :dev-rule-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'dev-rule)
    (and (push 'deviation-from-input (statuses node))
         t)
      t
      ))|#



(pushnew '((mal-cxn-applied) .  "#eb4034;") *status-colors*)


(def-fcg-cxn der-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nm ?nm - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (?gen - ?gf - ?gp)    ;genitive feminine
                               (?df - ?df - -)      ;sing, masc, fem, neut, plural
                               (?s ?nm ?f - ?gp))))   ;sing, masc, fem, neut, plural

               --
               (HASH form ((string ?the-word "der")))))
             :disable-automatic-footprints t)

(def-fcg-cxn dem-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)      ;sing, masc, fem, neut, plural
                               (+ ?dm - ?dn -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "dem")))))
             :disable-automatic-footprints t)


(def-fcg-cxn die-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nom - ?nf - ?np)    ;nom, acc, gen, dat  (nom masculine)
                               (?acc - ?af - ?ap)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (- - - - -)
                               (?s - ?f - ?p))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "die")))))
             :disable-automatic-footprints t)


(def-fcg-cxn den-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((- - - - -)        
                               (?am ?am - - -)        
                               (- - - - -)          
                               (?dp - - - ?dp)
                               (?am ?am - - ?dp))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "den")))))
             :disable-automatic-footprints t)


(def-fcg-cxn das-cxn
             ((?the-word
               (footprints (article))) 
             <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nn - - ?nn -)    ;nom, acc, gen, dat  (nom masculine)
                               (?an - - ?an -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (- - - - -)
                               (+ - - + -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "das")))))
             :disable-automatic-footprints t)


(def-fcg-cxn Polizist-cxn
             ((?policeman-word                        
               (referent ?p)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?policeman-word                            
               (HASH meaning ((policeman ?p)))                    
               --
               (HASH form ((string ?policeman-word  "Polizist"))))))


(def-fcg-cxn Polizist-cxn
             ((?policeman-word                        
               (referent ?p)
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?am - ?af - -)      
                               (- - - - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate))
              (error-cat (error incorrect-determiner)
                         (reason polizist-is-a-masculine-noun-needs-a-masculine-determiner-not-feminine)))
              <-
              (?policeman-word                            
               (HASH meaning ((policeman ?p)))                    
               --
               (HASH form ((string ?policeman-word  "Polizist")))))
             :cxn-set mal-cxn)
             

(def-fcg-cxn Mann-cxn
             ((?man-word                        
               (referent ?m)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?man-word                            
               (HASH meaning ((man ?m)))                    
               --
               (HASH form ((string ?man-word  "Mann"))))))

(def-fcg-cxn Tiger-cxn
             ((?tiger-word                        
               (referent ?t)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?tiger-word                            
               (HASH meaning ((tiger ?t)))                    
               --
               (HASH form ((string ?tiger-word  "Tiger"))))))


(def-fcg-cxn Kellner-cxn
             ((?waiter-word                        
               (referent ?k)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?waiter-word                            
               (HASH meaning ((waiter ?k)))                    
               --
               (HASH form ((string ?waiter-word  "Kellner"))))))

(def-fcg-cxn Becher-cxn
             ((?baker-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate))
               (error-cat (error incorrect-noun-choice)
                          (reason noun-does-not-match-stimulus-or-German-word)))
              <-
              (?baker-word                            
               (HASH meaning ((baker ?b)))                    
               --
               (HASH form ((string ?baker-word  "Becher")))))
             :cxn-set mal-cxn)


(def-fcg-cxn Hund-cxn
             ((?dog-word                        
               (referent ?d)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?dog-word                            
               (HASH meaning ((dog ?d)))                    
               --
               (HASH form ((string ?dog-word  "Hund"))))))


(def-fcg-cxn Bäcker-cxn
             ((?baker-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?baker-word                            
               (HASH meaning ((baker ?b)))                    
               --
               (HASH form ((string ?baker-word  "Bäcker"))))))


(def-fcg-cxn König-cxn
             ((?king-word                        
               (referent ?k)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?king-word                            
               (HASH meaning ((king ?k)))                    
               --
               (HASH form ((string ?king-word  "König"))))))


(def-fcg-cxn Jäger-cxn
             ((?hunter-word                        
               (referent ?j)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?hunter-word                            
               (HASH meaning ((hunter ?j)))                    
               --
               (HASH form ((string ?hunter-word  "Jäger"))))))

(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case))
               (sem-cat (animacy ?animacy))
               (subunits (?article ?noun))
               (boundaries (leftmost-unit ?article)
                           (rightmost-unit ?noun)))
              (?article
               (referent ?x)
               (part-of-noun-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?article
               --
               (syn-cat (lex-class article)
                        (case ?case)))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))
              ))
             :disable-automatic-footprints t)

(def-fcg-cxn noun-phrase-incorrect-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case))
               (sem-cat (animacy ?animacy))
               (error-cat (error ?e)
                          (reason ?r))
               (subunits (?article ?noun))
               (boundaries (leftmost-unit ?article)
                           (rightmost-unit ?noun)))
              (?article
               (referent ?x)
               (part-of-noun-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?article
               --
               (syn-cat (lex-class article)
                        (case ?case)))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case))
               (error-cat (error ?e)
                          (reason ?r)))
              (?noun-phrase
               --
               (HASH form ((meets ?article ?noun)))
              ))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)

(def-fcg-cxn sucht-cxn
             ((?search-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (referent ?s))  
                        
              <-
              (?search-word                           
               (HASH meaning ((suchen-01 ?s)))                   
               --
               (HASH form ((string ?search-word  "sucht"))))))


(def-fcg-cxn tötet-cxn
             ((?kill-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (referent ?k))  
                        
              <-
              (?kill-word                           
               (HASH meaning ((töten-01 ?k)))                   
               --
               (HASH form ((string ?kill-word  "tötet"))))))


(def-fcg-cxn ruft-cxn
             ((?call-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (referent ?c))  
                        
              <-
              (?call-word                           
               (HASH meaning ((rufen-01 ?c)))                   
               --
               (HASH form ((string ?call-word  "ruft"))))))



(def-fcg-cxn verfolgt-cxn
             ((?follow-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (referent ?v))  
                        
              <-
              (?follow-word                           
               (HASH meaning ((verfolgen-01 ?v)))                   
               --
               (HASH form ((string ?follow-word  "verfolgt"))))))



(def-fcg-cxn topicalized-transitive-argument-structure-cxn
             ((?topicalized-transitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type transitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy animate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy animate))
              (referent ?arg1))
              
              
              (?topicalized-transitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               )))

(def-fcg-cxn incorrect-patient-in-topicalized-transitive-argument-structure-cxn
             ((?incorrect-patient-in-topicalized-transitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type transitive))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat 
                (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (sem-cat (animacy animate))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
                        (sem-cat (animacy animate))   
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat 
                        (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (sem-cat (animacy animate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (error-cat (error ?e)
                         (reason ?r))
              (sem-cat (animacy animate))
              (referent ?arg1))
              
              
              (?incorrect-patient-in-topicalized-transitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               ))
             :cxn-set mal-cxn)

(def-fcg-cxn arg0-topic-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit))
               (HASH meaning ((topicalized ?arg1 +)))  
                          
               --
               (HASH form ((meets ?rightmost-patient-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-agent-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect ?aspect))     
                --
              (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect ?aspect)))
              
              (?agent-unit
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (referent ?arg1)
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (referent ?arg1)
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))
              
              ))


;;CORRECT SENTENCES

(comprehend "den Bäcker sucht der Polizist")
(comprehend "den Jäger tötet der Tiger")
(comprehend "den Kellner ruft der König")
(comprehend "den Mann verfolgt der Hund")



;INCORRECT SENTENCES
(comprehend "den Becher")
(comprehend "den Becher sucht der Polizist")
(comprehend "den Becher sucht die Polizist")

