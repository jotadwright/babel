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


(def-fcg-cxn der-cxn
             ((?the-word
               (footprints (article))
               (sequences ((sequence "der" ?left ?right))))
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
               (HASH form ((sequence "der" ?left ?right)))))
             :disable-automatic-footprints t)


(def-fcg-cxn dem-cxn
             ((?the-word
               (footprints (article))
               (sequences ((sequence "dem" ?left ?right)))) 
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
               (HASH form ((sequence "dem" ?left ?right)))))
             :disable-automatic-footprints t)


(def-fcg-cxn zum-cxn
             ((?to-word
              (footprints (article))
              (sequences ((sequence "zum" ?left ?right))))
              <-
              (?to-word
               (footprints (not article))
               (syn-cat (lex-class contracted-preposition) 
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -))))
               --
               (HASH form ((sequence "zum" ?left ?right)))))
             :disable-automatic-footprints t)



(def-fcg-cxn Doktor-cxn
             ((?doctor-word
               (referent ?d)
               (sequences ((sequence "Doktor" ?left ?right)))
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?doctor-word
               (HASH meaning ((doctor ?d)))                     
               --
               (HASH form ((sequence "Doktor" ?left ?right))))))


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


(def-fcg-cxn Kino-cxn
             ((?cinema-word
               (referent ?c)
               (sequences ((sequence "Kino" ?left ?right)))
               (syn-cat (lex-class noun)         
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
               (sem-cat (animacy inanimate)))
              <-
              (?cinema-word
               (HASH meaning ((cinema ?c)))                     
               --
               (HASH form ((sequence "Kino" ?left ?right))))))


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


(def-fcg-cxn Arbeit-cxn
             ((?work-word                        
               (referent ?w)
               (sequences ((sequence "Arbeit" ?left ?right)))
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy inanimate)))  
              <-
              (?work-word
               (HASH meaning ((work ?w)))                     
               --
               (HASH form ((sequence "Arbeit" ?left ?right))))))


(def-fcg-cxn Mann-cxn
             ((?man-word
               (referent ?m)
               (sequences ((sequence "Mann" ?left ?right)))
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
               (HASH form ((sequence "Mann" ?left ?right))))))


(def-fcg-cxn Blumen-cxn
             ((?flowers-word
               (referent ?x)
               (sequences ((sequence "Blumen" ?left ?right)));set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
                        (case ((?np - - - ?np)     
                               (?ap - - - ?ap)      
                               (?gp - - - ?gp)       
                               (?dp - - - ?dp)
                               (- - - - +))))
              (sem-cat (animacy inanimate)))
              <-
              (?flowers-word
               (HASH meaning ((flowers ?x)))                     
               --
               (HASH form ((sequence "Blumen" ?left ?right))))))


(def-fcg-cxn schenkt-cxn
             ((?gift-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?g))  
                        
              <-
              (?gift-word                           
               (HASH meaning ((schenken-01 ?g)))                   
               --
               (HASH form ((sequence "schenkt" ?verb-left ?verb-right))))))


(def-fcg-cxn sucht-cxn
             ((?search-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?s))  
                        
              <-
              (?search-word                           
               (HASH meaning ((suchen-01 ?s)))                   
               --
               (HASH form ((sequence "sucht" ?verb-left ?verb-right))))))

(def-fcg-cxn geht-cxn
             ((?go-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?g))  
                        
              <-
              (?go-word                           
               (HASH meaning ((gehen-01 ?g)))                   
               --
               (HASH form ((sequence "geht" ?verb-left ?verb-right))))))


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


(def-fcg-cxn contracted-prep-phrase-cxn
             ((?contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ?case))
               (sem-cat (animacy ?animacy))
               (subunits (?contracted-prep ?noun))
               (boundaries (?contr-prep-left ?noun-right)))
              
              (?contracted-prep
               (referent ?x)
               (part-of-prep-phrase +))

              (?noun
               (footprints (determined)))
              <-
              (?contracted-prep
               --
               (syn-cat (lex-class contracted-preposition)
                        (case ?case))
               (sequences ((sequence ?contr-prep-string ?contr-prep-left ?contr-prep-right))))
              
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
              (?contracted-prep-phrase
               --
               (HASH form ((sequence " " ?contr-prep-right ?noun-left)))
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
               --)))

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
              (boundaries (?leftmost-patient-unit ?rightmost-patient-unit)))))

(def-fcg-cxn topic-arg1-arg0-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit))
               (HASH meaning ((topicalized ?arg1 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-patient-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-agent-unit)))
               
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
               (referent ?arg1)
               (syn-cat (syn-role direct-object))
               (boundaries (?leftmost-patient-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (?leftmost-patient-unit ?rightmost-patient-unit)))))



(def-fcg-cxn intransitive-argument-structure-cxn
             ((?intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive))     
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
              
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (?dat ?dm ?df ?dn ?dp)
                      (?ls ?m ?f ?n ?lp))))
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (?acc ?am ?af ?an ?ap)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg4))
              
              (?intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg4 ?v ?arg4)))                  
               --)))



(def-fcg-cxn topic-arg0-arg4-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-loc-unit)))
               
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit))    
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement))
               (boundaries (?leftmost-loc-unit ?rightmost-loc-unit))
                --
              
              (syn-cat (syn-role locative-complement))
              (boundaries (?leftmost-loc-unit ?rightmost-loc-unit)))))


(def-fcg-cxn topic-arg4-arg0-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg4 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-loc-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-agent-unit)))
               
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit))    
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (referent ?arg4)
               (syn-cat (syn-role locative-complement))
               (boundaries (?leftmost-loc-unit ?rightmost-loc-unit))
                --
              
              (syn-cat (syn-role locative-complement))
              (boundaries (?leftmost-loc-unit ?rightmost-loc-unit)))))


(def-fcg-cxn ditransitive-argument-structure-cxn
             ((?ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role indirect-object)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive))     
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
               (sem-cat (animacy inanimate))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (sem-cat (animacy animate))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (sem-cat (animacy animate))
              (referent ?arg2))
              
              (?ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --)))


(def-fcg-cxn topic-arg0-arg1-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-receiver-unit)
                             (sequence " " ?rightmost-receiver-unit ?leftmost-patient-unit)))
               
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)
                       (aspect ?aspect))
              (boundaries (?verb-left ?verb-right))
              (referent ?v))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries  (?leftmost-patient-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries  (?leftmost-patient-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role indirect-object))
               (boundaries  (?leftmost-receiver-unit ?rightmost-receiver-unit))
                --
              
              (syn-cat (syn-role indirect-object))
              (boundaries  (?leftmost-receiver-unit ?rightmost-receiver-unit)))))






(comprehend "die Frau sucht den Hund")
(comprehend "der Mann sucht den Hund")
(comprehend "den Mann sucht der Hund")


(comprehend "der Mann geht zum Doktor")
(comprehend "zum Kino geht die Frau")
(comprehend "die Frau geht zum Kino")

(comprehend "die Frau schenkt dem Mann die Blumen")


