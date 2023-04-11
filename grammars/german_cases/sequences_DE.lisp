(ql:quickload :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

(def-fcg-constructions german-case-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (subunits set)
                  (boundaries sequence)
                  (footprints set)
                  (case sequence))
  :fcg-configurations ((:max-nr-of-nodes . 40000)
                       (:de-render-mode . :de-render-sequence)
          
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network :connected-structure)
                       ;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :cxn-sets) ;; returns all cxns at once
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-sets) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       ;; cxn sets
                       (:parse-order morph cxn)
                       (:production-order cxn morph)
                       ;; goal tests
                       (:production-goal-tests
                        :no-applicable-cxns :connected-structure
                        :no-meaning-in-root)))


(def-fcg-cxn die-cxn
             ((?the-word
               (footprints (article))
               (sequences ((sequence "die" ?left ?right)))) 
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
               (HASH form ((sequence "die" ?left ?right)))))
             :disable-automatic-footprints t)

(def-fcg-cxn den-cxn
             ((?the-word
               (footprints (article))
               (sequences ((sequence "den" ?left ?right)))) 
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
               (HASH form ((sequence "den" ?left ?right)))))
             :disable-automatic-footprints t)

(def-fcg-cxn Hund-cxn
             ((?dog-word
               (referent ?d)
               (sequences ((sequence "Hund" ?left ?right)))
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
               (HASH form ((sequence "Hund" ?left ?right))))))



(def-fcg-cxn Frau-cxn
             ((?woman-word                        
               (referent ?w)
               (sequences ((sequence "Frau" ?left ?right)))
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate)))  
              <-
              (?woman-word
               (HASH meaning ((woman ?w)))                     
               --
               (HASH form ((sequence "Frau" ?left ?right))))))


(def-fcg-cxn sucht-cxn
             ((?search-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (boundaries (?verb-left ?verb-right))
               ;(sequences ((sequence "sucht" ?left ?right)))
               (referent ?s))  
                        
              <-
              (?search-word                           
               (HASH meaning ((suchen-01 ?s)))                   
               --
               (HASH form ((sequence "sucht" ?verb-left ?verb-right))))))





(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case))
               (sem-cat (animacy ?animacy))
               (subunits (?article ?noun))
               (boundaries (?article-left ?noun-right)))
              (?article
               (referent ?x)
               (part-of-noun-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?article
               --
               (syn-cat (lex-class article)
                        (case ?case))
               (sequences ((sequence ?article-string ?article-left ?article-right))))
              
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
              (sequences ((sequence ?noun-string ?noun-left ?noun-right))))
              (?noun-phrase
               --
               (HASH form ((sequence " " ?article-right ?noun-left)))
              ))
             :disable-automatic-footprints t)




(def-fcg-cxn transitive-argument-structure-cxn                 
             ((?transitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                        (aspect ?aspect)
                        (type transitive))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                        (aspect ?aspect)
                        (type transitive))
              (referent ?v))
              
              (?agent-unit
               (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
              (referent ?arg0))
              
              (?patient-unit
               (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (referent ?arg1))
              
              (?transitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               )))




(def-fcg-cxn topic-arg0-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-patient-unit)))
               
               (subunits (?verb-unit ?agent-unit ?patient-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type transitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit))   ;from 
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (?leftmost-patient-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (?leftmost-patient-unit ?rightmost-patient-unit)))
              ))



(comprehend "die Frau sucht den Hund")
