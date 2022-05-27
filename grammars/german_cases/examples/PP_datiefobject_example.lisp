(ql:quickload :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO DO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;need to create cxns for broader sentence (mit seinem Fahrrad - beim Fahrrad + info struct variation - zum Arbeit);
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :fcg)

(activate-monitor trace-fcg)


(def-fcg-constructions german-case-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set)
                  (case sequence))
  :fcg-configurations ((:max-nr-of-nodes . 40000)
          
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
                       (:production-order cxn mal-cxn)
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

(pushnew '((mal-cxn-applied) .  "#eb4034;") *status-colors*)

;;;;DETERMINERS


;;;;;no meaning - comprehension no need existing unit 

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

(def-fcg-cxn seinem-cxn
             ((?the-word
               (footprints (adjective))) 
             <-
              (?the-word
               (footprints (not adjective))
               (syn-cat (lex-class adjective)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)      ;sing, masc, fem, neut, plural
                               (+ ?dm - ?dn -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?the-word "seinem")))))
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



(def-fcg-cxn Laden-cxn
             ((?store-word                        
               (referent ?s)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy inanimate)))
              <-
              (?store-word                            
               (HASH meaning ((store ?s)))                    
               --
               (HASH form ((string ?store-word  "Laden"))))))

(def-fcg-cxn Mädchen-cxn
             ((?girl-word                        
               (referent ?g)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
               (sem-cat (animacy animate)))
              <-
              (?girl-word                            
               (HASH meaning ((girl ?g)))                    
               --
               (HASH form ((string ?girl-word  "Mädchen"))))))


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


(def-fcg-cxn Junge-cxn
             ((?boy-word
               (referent ?b)                  
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
                       
              <-
              (?boy-word
               (HASH meaning ((boy ?b)))                     
               --
               (HASH form ((string ?boy-word  "Junge"))))))

(def-fcg-cxn Arzt-cxn
             ((?doctor-word
               (referent ?d)                  
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
               (HASH form ((string ?doctor-word  "Arzt"))))))

(def-fcg-cxn Fahrrad-cxn
             ((?bike-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
               (sem-cat (animacy inanimate)))
              <-
              (?bike-word                            
               (HASH meaning ((bike ?b)))                    
               --
               (HASH form ((string ?bike-word  "Fahrrad"))))))


(def-fcg-cxn Arbeit-cxn
             ((?work-word
               (referent ?w)                             ;set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
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
               (HASH form ((string ?work-word  "Arbeit"))))))

(def-fcg-cxn aus-cxn
             ((?from-word
               (footprints (preposition)))
              <-
              (?from-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type from-motion-locative)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (- - - - -)      
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?dm ?df ?dn ?dp))))
               --
               (HASH form ((string ?from-word "aus")))))
             :disable-automatic-footprints t)


(def-fcg-cxn auf-cxn
             ((?on-word
               (footprints (preposition)))
              <-
              (?on-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type on-motion-locative)
                        (case ((- - - - -)      
                               (?acc ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (HASH form ((string ?on-word "auf")))))
             :disable-automatic-footprints t)


(def-fcg-cxn aus-acc-cxn
             ((?from-word
               (footprints (preposition)))
              <-
              (?from-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type from-motion-locative)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap))))
               ;(error-cat (ERROR incorrect-case-selection)
                         ; (REASON dative-needed))
               --
               (HASH form ((string ?from-word "aus")))))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)


(def-fcg-cxn mit-cxn
             ((?with-word
               
               (footprints (preposition)))
              <-
              (?with-word
               (referent ?x)
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (type accompanying)
                        (polarity pos)   ;(polarity +)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (- - - - -)      
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?dm ?df ?dn ?dp))))
               (HASH meaning ((accompany-01 ?x)
                              (polarity ?x pos)))   ;(polarity ?x +)
               --
               (HASH form ((string ?with-word "mit")))))
             :disable-automatic-footprints t)

(def-fcg-cxn zur-cxn
             ((?to-word
              (footprints (article)))
              <-
              (?to-word
               (footprints (not article))
               (syn-cat (lex-class contracted-preposition)
                        (type motion-locative-contracted)
                        (polarity pos)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ - ?df - -)
                               (+ - ?df - -))))
               --
               (HASH form ((string ?to-word "zur"))))))

(def-fcg-cxn zum-cxn
             ((?to-word
               (footprints (preposition)))
              <-
              (?to-word
               (footprints (not preposition))
               (syn-cat (lex-class contracted-preposition)
                        (type motion-locative-contracted)
                        (polarity pos)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (- - - - -)      
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?dm ?df ?dn ?dp))))
               --
               (HASH form ((string ?to-word "zum")))))
             :disable-automatic-footprints t)

(comprehend "zum Arzt")

(def-fcg-cxn zum-err-cxn
             ((?to-word
              (footprints (article)))
              <-
              (?to-word
               (footprints (not article))
               (syn-cat (lex-class preposition)
                        (type motion-locative)
                        (polarity pos)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm ?df - -)
                               (+ ?dm ?df - -))))
               (error-cat (error incorrect-gender-match)
                          (reason zum-accepts-only-masc-or-neut-case))
               --
               (HASH form ((string ?to-word "zum")))))
             :cxn-set mal-cxn)


(def-fcg-cxn beim-cxn
             ((?to-word
              (footprints (article)))
              <-
              (?to-word
               (footprints (not article))
               (syn-cat (lex-class preposition)
                        (type motion-locative-and-temporal)
                        (polarity pos)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -))))
               --
               (HASH form ((string ?to-word "beim"))))))


(def-fcg-cxn fährt-cxn
             ((?drive-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type single-intransitive))
               (referent ?f))  
                        
              <-
              (?drive-word                           
               (HASH meaning ((fahren-01 ?f)))                   
               --
               (HASH form ((string ?drive-word  "fährt"))))))

(def-fcg-cxn geht-cxn
             ((?go-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive))
               (referent ?g))  
                        
              <-
              (?go-word                           
               (HASH meaning ((gehen-01 ?g)))                   
               --
               (HASH form ((string ?go-word  "geht"))))))


(def-fcg-cxn kommt-cxn
             ((?come-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-origin))
               (referent ?k))  
                        
              <-
              (?come-word                           
               (HASH meaning ((kommen-01 ?k)))                   
               --
               (HASH form ((string ?come-word  "kommt"))))))



(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case)
                        )
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


(def-fcg-cxn prepositional-phrase-cxn
             ((?prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type ?type)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (sem-cat (animacy ?animacy))
               (subunits (?preposition ?article ?noun))
               (boundaries (leftmost-unit ?preposition)
                           (rightmost-unit ?noun)))
              (?preposition
               (part-of-prep-phrase +))
              
              (?article
               (referent ?x)
               ;(part-of-noun-phrase +))
               )

              (?noun
               (footprints (determined))
               )
              <-

              (?preposition
               
               --
               (syn-cat (lex-class preposition)
                        (type ?type)
                        (polarity ?polarity)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?prep-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t)


(def-fcg-cxn incorrect-acc-prepositional-phrase-cxn
             ((?incorrect-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))
                        (type from-motion-locative)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (error-cat (ERROR incorrect-case-selection-for-this-preposition)
                        (REASON dative-case-needed-not-accusative))
               (sem-cat (animacy ?animacy))
               (subunits (?preposition ?article ?noun))
               (boundaries (leftmost-unit ?preposition)
                           (rightmost-unit ?noun)))
              (?preposition
               (part-of-prep-phrase +))
              
              (?article
               (referent ?x)
               ;(part-of-noun-phrase +))
               )

              (?noun
               (footprints (determined))
               )
              <-

              (?preposition
               
               --
               (syn-cat (lex-class preposition)
                        (type from-motion-locative)
                        (polarity ?polarity)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?noun
               
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?incorrect-prep-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)


(def-fcg-cxn auf-prepositional-phrase-cxn
             ((?auf-prepositional-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))
                        (type on-motion-locative)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (error-cat (ERROR incorrect-preposition-choice)
                        (REASON auf-is-a-locative-or-directional-prep-not-origin))
               (sem-cat (animacy ?animacy))
               (subunits (?preposition ?article ?noun))
               (boundaries (leftmost-unit ?preposition)
                           (rightmost-unit ?noun)))
              (?preposition
               (part-of-prep-phrase +))
              
              (?article
               (referent ?x)
               ;(part-of-noun-phrase +))
               )

              (?noun
               (footprints (determined))
               )
              <-

              (?preposition
               
               --
               (syn-cat (lex-class preposition)
                        (type on-motion-locative)
                        (polarity ?polarity)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?noun
               
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)      
                               (+ ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?s ?am ?af ?an ?ap)))))
              (?auf-prepositional-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)

(comprehend "auf den Laden")

(def-fcg-cxn contracted-prep-phrase-cxn
             ((?contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type ?type)
                        (polarity ?polarity)
                        (form-type contracted))
               (sem-cat (animacy ?animacy))
               (subunits (?contracted-prep ?noun))
               (boundaries (leftmost-unit ?contracted-prep)
                           (rightmost-unit ?noun)))
              (?contracted-prep
               (part-of-prep-phrase +)
               (referent ?x))
              (?noun
               (footprints (determined)))
              <-
              (?contracted-prep
               --
               (syn-cat (lex-class contracted-preposition)
                        (type ?type)
                        (polarity ?polarity)
                        (case ?case)))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case))
               (sem-cat (animacy ?animacy))
                 
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              
              (?contracted-prep-phrase
               --
               (HASH form ((meets ?contracted-prep ?noun)))
              ))
             :disable-automatic-footprints t)


(def-fcg-cxn accompanying-poss-phrase-cxn
             ((?accompanying-poss-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type accompanying)
                        (form-type extended-accompanying-prep-phrase)
                        (polarity ?polarity))
               (error-cat (error variation-from-stimulus)
                          (reason variation-from-received-stimulus-in-task-but-grammatically-correct))
               (subunits (?preposition ?adj ?noun))
               (boundaries (leftmost-unit ?preposition)
                           (rightmost-unit ?noun)))
              (?preposition
               (part-of-prep-phrase +))
              
              (?adj
              (part-of-prep-phrase +))

              (?noun
               (footprints (determined)))
              <-

              (?preposition
               --
               (referent ?x)
               (syn-cat (lex-class preposition)
                        (type accompanying)
                        (polarity ?polarity)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?adj
               --
               (syn-cat (lex-class adjective)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              
              (?accompanying-poss-phrase
               (HASH meaning ((arg0 ?x ?no)))
               --
               (HASH form ((meets ?preposition ?adj)
                           (meets ?adj ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)


(def-fcg-cxn accompanying-phrase-cxn
             ((?accompanying-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type accompanying)
                        (form-type extended-accompanying-prep-phrase)
                        (polarity ?polarity))
               (subunits (?preposition ?article ?noun))
               (boundaries (leftmost-unit ?preposition)
                           (rightmost-unit ?noun)))
              (?preposition
               (part-of-prep-phrase +))
              
              (?article
              (part-of-prep-phrase +))

              (?noun
               (footprints (determined)))
              <-

              (?preposition
               --
               (referent ?x)
               (syn-cat (lex-class preposition)
                        (type accompanying)
                        (polarity ?polarity)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              
              (?accompanying-phrase
               (HASH meaning ((arg0 ?x ?no)))
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t)


(def-fcg-cxn incorrect-contracted-prep-phrase-cxn
             ((?incorrect-contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - - -)
                               (+ ?dm - - -)))
                        (type ?type)
                        (polarity ?polarity)
                        (form-type contracted))
               (sem-cat (animacy ?animacy))
               (error-cat (ERROR violation-of-prep-noun-gender-agreement)
                        (REASON feminine-form-needed-with-arbeit-instead-of-neut-masc))
               (subunits (?contracted-prep ?noun))
               (boundaries (leftmost-unit ?contracted-prep)
                           (rightmost-unit ?noun)))
              (?contracted-prep
               (part-of-prep-phrase +)
               (referent ?x))
              (?noun
               (footprints (determined)))
              <-
              (?contracted-prep
               --
               (syn-cat (lex-class preposition)
                        (type ?type)
                        (polarity ?polarity)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - - -)
                               (+ ?dm - - -)))))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ - ?df - -)
                               (+ - ?df - -))))
               (sem-cat (animacy ?animacy))
                 
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              
              (?incorrect-contracted-prep-phrase
               --
               (HASH form ((meets ?contracted-prep ?noun)))
              ))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)


(def-fcg-cxn incorrect-bei-contracted-prep-phrase-cxn
             ((?incorrect-contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - - -)
                               (+ ?dm - - -)))
                        (type motion-locative-and-temporal)
                        (polarity ?polarity)
                        (form-type contracted))
               (sem-cat (animacy ?animacy))
               (error-cat (ERROR incorrect-preposition-choice)
                        (REASON bei-is-a-locative-and-temporal-preposition-not-a-means-one))
               (subunits (?contracted-prep ?noun))
               (boundaries (leftmost-unit ?contracted-prep)
                           (rightmost-unit ?noun)))
              (?contracted-prep
               (part-of-prep-phrase +)
               (referent ?x))
              
              (?noun
               (footprints (determined)))
              <-
              (?contracted-prep
               --
               (syn-cat (lex-class preposition)
                        (type motion-locative-and-temporal)
                        (polarity ?polarity)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -)))))
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm ?df ?dn -)
                               (+ ?dm ?df ?dn -))))
               (sem-cat (animacy ?animacy))
                 
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ?case)))
              
              (?incorrect-contracted-prep-phrase
               --
               (HASH form ((meets ?contracted-prep ?noun)))
              ))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)


(def-fcg-cxn intransitive-origin-argument-structure-cxn
             ((?intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive-origin))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive-origin))     
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
                      (- - - - -)         
                      (- - - - -)         
                      (?dat ?dm ?df ?dn ?dp)
                      (?ls ?m ?f ?n ?lp)
                      ))
                   (type from-motion-locative)
                   (form-type extended-prep-phrase)
                   (polarity ?polarity))
               (referent ?arg3)
                --
              (syn-cat (lex-class prep-phrase)
                       (polarity ?polarity)
                       (form-type extended-prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp)
                              )))
              (referent ?arg3))
              
              (?intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg3 ?v ?arg3)))                  
               --
               )))


(def-fcg-cxn topic-arg0-arg3-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive-origin)
                       (aspect ?aspect))     
              
                --
              (syn-cat (lex-class verb)
                       (type intransitive-origin)
                       (aspect ?aspect)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit))
                --
              
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit)))
              ))


;;;aus den Laden

(def-fcg-cxn incorrect-intransitive-origin-argument-structure-cxn
             ((?incorrect-intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement))
               (error-cat (error ?e)
                         (reason ?r)))
              
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-origin))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-origin))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ags ?nm ?nf ?nn ?np))))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ags ?nm ?nf ?nn ?np))))
              (referent ?arg0))
              
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                      (+ ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?ap)
                      ))
                   (type from-motion-locative)
                   (form-type extended-prep-phrase)
                   (polarity ?polarity))
               (referent ?arg3)
                --
              (syn-cat (lex-class prep-phrase)
                       (type from-motion-locative)
                       (polarity ?polarity)
                       (form-type extended-prep-phrase)
                     (case ((- - - - -) 
                      (+ ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?ap)
                      )))
              (error-cat (error ?e)
                        (reason ?r))
              (referent ?arg3))
              
              (?incorrect-intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg3 ?v ?arg3)))                  
               --
               ))
             :cxn-set mal-cxn)


(def-fcg-cxn incorrect-auf-intransitive-origin-argument-structure-cxn
             ((?incorrect-intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-origin))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-origin))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ags ?nm ?nf ?nn ?np))))
               (referent ?arg0)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?ags ?nm ?nf ?nn ?np))))
              (referent ?arg0))
              
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                      (+ ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?ap)
                      ))
                   (type on-motion-locative)
                   (form-type extended-prep-phrase)
                   (polarity ?polarity))
               (referent ?arg3)
                --
              (syn-cat (lex-class prep-phrase)
                       (type on-motion-locative)
                       (polarity ?polarity)
                       (form-type extended-prep-phrase)
                     (case ((- - - - -) 
                      (+ ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?ap)
                      )))
              (error-cat (error ?e)
                        (reason ?r))
              (referent ?arg3))
              
              (?incorrect-intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg3 ?v ?arg3)))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "auf den Laden")

(def-fcg-cxn topic-arg0-incorrect-arg3-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
              (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive-origin)
                       (aspect ?aspect))     
              
                --
              (syn-cat (lex-class verb)
                       (type intransitive-origin)
                       (aspect ?aspect)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (error-cat (error ?e)
                          (reason ?r))
               (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit))
                --
              
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit)))
              )
             :cxn-set mal-cxn)

(comprehend "das Mädchen kommt aus den Laden")

(def-fcg-cxn intransitive-extra-arg1-structure-cxn
             ((?intransitive-extra-arg1-structure-unit
              (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?extra-info-unit
               (syn-cat (syn-role extra-information)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive))     
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                   (case ?case)
                   (type accompanying)
                   ;(form-type extended-accompanying-prep-phrase)
                   ;(polarity +)
                   )
               (referent ?manner)
                   
                --
              (syn-cat (lex-class prep-phrase)
                        (case ?case))
              (referent ?manner))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ ?dm ?df ?dn ?dp)
                          (?ls ?dm ?df ?dn ?lp))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (+ ?dm ?df ?dn ?dp)
                              (?ls ?dm ?df ?dn ?lp))))
              (referent ?arg1))
              
              (?intransitive-extra-arg1-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?manner)
                              (:arg1 ?v ?arg1)
                              (:arg1 ?manner ?arg0)
                              ))                  
               --
               )))

(def-fcg-cxn var-intransitive-extra-arg1-structure-cxn
             ((?var-intransitive-extra-arg1-structure-unit
              (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?extra-info-unit
               (syn-cat (syn-role extra-information)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive))     
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                        (form-type extended-accompanying-prep-phrase)
                   (case ?case))
               (referent ?manner)
                   
                --
              (syn-cat (lex-class prep-phrase)
                       (form-type extended-accompanying-prep-phrase))
              (error-cat (error ?e)
                         (reason ?r))
              (referent ?manner))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ - ?df - -)
                          (?ls - ?df - -))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ - ?df - -)
                          (?ls - ?df - -))))
              (referent ?arg1))
              
              (?var-intransitive-extra-arg1-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?manner)
                              (:arg1 ?v ?arg1)
                              (:arg1 ?manner ?arg0)
                              ))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "der Mann fährt mit seinem Fahrrad zur Arbeit")

(def-fcg-cxn intransitive-incorrect-extra-arg1-structure-cxn
             ((?intransitive-incorrect-extra-arg1-structure-unit
              (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?extra-info-unit
               (syn-cat (syn-role extra-information)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive))     
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-and-temporal)
                   (case ?case))
               (referent ?manner)
                   
                --
              (syn-cat (lex-class prep-phrase)
                       (type motion-locative-and-temporal))
              (error-cat (error ?e)
                         (reason ?r))
              (referent ?manner))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ - ?df - -)
                          (?ls - ?df - -))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ - ?df - -)
                          (?ls - ?df - -))))
              (referent ?arg1))
              
              (?intransitive-incorrect-extra-arg1-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?manner)
                              (:arg1 ?v ?arg1)
                              (:arg1 ?manner ?arg0)
                              ))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "der Mann fährt beim Fahrrad zur Arbeit")

;;;;;;;;;;;NEED TO FIX THIS

(def-fcg-cxn incorrect-intransitive-extra-arg1-structure-cxn
             ((?intransitive-extra-arg1-structure-incorrect-location-unit
              (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?extra-info-unit
               (syn-cat (syn-role extra-information)))
              (?location-unit
               (syn-cat (syn-role locative-complement))
               (error-cat (error ?e)
                          (reason ?r)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive))     
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ ?dm - ?dn ?dp)
                          (?es ?dm - ?dn ?lp))))
               (referent ?manner)
                   
                --
              (syn-cat (lex-class prep-phrase)
                       (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (+ ?dm - ?dn ?dp)
                          (?es ?dm - ?dn ?lp))))
              (referent ?manner))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative)
                        (form-type contracted)
                   (case ?case))
               (error-cat (error ?e)
                         (reason ?r))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                       (type motion-locative)
                       (form-type contracted)
                        (case ?case))
              (error-cat (error ?e)
                         (reason ?r))
              (referent ?arg1))
              
              (?intransitive-extra-arg1-structure-incorrect-location-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?manner)
                              (:arg1 ?v ?arg1)
                              (:arg1 ?manner ?arg0)))                  
               --
               ))
             :cxn-set mal-cxn)

(comprehend "der Mann fährt mit dem Fahrrad zum Arbeit")


(def-fcg-cxn topic-arg0-extra-info-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-extra-info-unit)
                           (meets ?rightmost-extra-info-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive))
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?extra-info-unit
               (syn-cat (syn-role extra-information)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit))
                --
              (syn-cat (syn-role extra-information)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit))
                --
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit)))
              ))



(def-fcg-cxn topic-arg0-extra-inf-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-extra-info-unit)
                           (meets ?rightmost-extra-info-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive))
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?extra-info-unit
               (syn-cat (syn-role extra-information)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit))
                --
              (syn-cat (syn-role extra-information)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit))
                --
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit)))
              )
             :cxn-set mal-cxn)



(def-fcg-cxn topic-arg0-arg1-extra-inf-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-location-unit)
                           (meets ?rightmost-location-unit ?leftmost-extra-info-unit)))
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive))
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?extra-info-unit
               (syn-cat (syn-role extra-information)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit))
                --
              (syn-cat (syn-role extra-information)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-extra-info-unit)
                          (rightmost-unit ?rightmost-extra-info-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit))
                --
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (leftmost-unit ?leftmost-location-unit)
                          (rightmost-unit ?rightmost-location-unit)))
              )
             :cxn-set mal-cxn)


(def-fcg-cxn intransitive-arg0-arg4-argument-structure-cxn
             ((?intransitive-arg0-arg4-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))  
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect non-perfect))     
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
                        (type motion-locative-contracted)
                        (form-type extended-prep-phrase)
                   (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (?dat ?dm ?df ?dn ?dp)
                      (?ls ?dm ?df ?dn ?dp))))
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                       (type motion-locative-contracted)
                        (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (?dat ?dm ?df ?dn ?dp)
                      (?ls ?dm ?df ?dn ?dp))))
              (referent ?arg4))
              
              (?intransitive-arg0-arg4-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg4 ?v ?arg4)))                  
               --
               )))




;;;;FORMULATION

;das Mädchen kommt aus dem Laden
;(formulate '((GIRL g) (STORE s) (KOMMEN-01 k) (ARG3 k s) (ARG0 k g) (TOPICALIZED g +)))
;der Mann fährt mit dem Fahrrad zur Arbeit
;(formulate '((bike b) (work w) (man m) (fahren-01 f) (arg0 f m) (arg1 f w) (topicalized m +) (manner f a) (accompany-01 a) (arg0 a b) (arg1 a m) (polarity a pos)))

;;;;;COMPREHENSION
(comprehend "der Junge geht zum Arzt")


;;;;ERRORS

;(comprehend "das Mädchen kommt aus dem Laden")
;(comprehend "aus den Laden")
(comprehend "mit seinem Fahrrad")
(comprehend "beim Fahrrad")
(comprehend "zum Arbeit")
(comprehend "das Mädchen kommt aus den Laden")
(comprehend "das Mädchen kommt auf den Laden")
(comprehend "der Mann fährt mit dem Fahrrad zum Arbeit") 
(comprehend "der Mann fährt mit seinem Fahrrad zur Arbeit")  
(comprehend "der Mann fährt beim Fahrrad zur Arbeit")  
(comprehend "der Mann fährt zur Arbeit beim Fahrrad")








