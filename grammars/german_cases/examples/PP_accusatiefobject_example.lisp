(ql:quickload :fcg)

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
                       (:parse-order cxn dev-rule mal-cxn)
                       (:production-order cxn mal-cxn dev-rule)
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


(defmethod cip-node-test ((node cip-node) (mode (eql :dev-rule-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'dev-rule)
    (and (push 'deviation-from-input (statuses node))
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
                               (?am ?am - - ?dp))))   
               
               --
               (HASH form ((string ?the-word "den")))))
             :disable-automatic-footprints t)

(def-fcg-cxn ihren-cxn
             ((?her-word
               (footprints (possessive-adj))) 
             <-
              (?her-word
               (footprints (not possessive-adj))
               (syn-cat (lex-class possessive-adj)
                        (case ((- - - - -)        
                               (?am ?am - - -)        
                               (- - - - -)          
                               (?dp - - - ?dp)
                               (?am ?am - - ?dp))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((string ?her-word "ihren")))))
             :disable-automatic-footprints t
             :cxn-set dev-rule)


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


(def-fcg-cxn Wurm-cxn
             ((?worm-word                        
               (referent ?w)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ ?m ?n - -))))
               (error-cat (error incorrect-case-selection)
                          (reason wurm-is-a-neutral-noun-not-masculine)
                          (extra-info in-this-case-wurm-does-not-match-the-intended-stimulus-meaning))
               (sem-cat (animacy animate)))
              <-
              (?worm-word                            
               (HASH meaning ((worm ?w)))                    
               --
               (HASH form ((string ?worm-word  "Wurm")))))
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

(def-fcg-cxn Baum-cxn
             ((?tree-word
               (referent ?b)                  
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy inanimate)))
              <-
              (?tree-word
               (HASH meaning ((tree ?b)))                     
               --
               (HASH form ((string ?tree-word  "Baum"))))))

(def-fcg-cxn Sohn-cxn
             ((?son-word                        
               (referent ?s)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?son-word                            
               (HASH meaning ((son ?s)))                    
               --
               (HASH form ((string ?son-word  "Sohn"))))))

(def-fcg-cxn Mutter-cxn
             ((?mother-word                        
               (referent ?m)
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate)))  
              <-
              (?mother-word
               (HASH meaning ((mother ?m)))                     
               --
               (HASH form ((string ?mother-word  "Mutter"))))))

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

(def-fcg-cxn ohne-cxn
             ((?without-word
               
               (footprints (preposition)))
              <-
              (?without-word
               (referent ?x)
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (type accompanying)
                        (polarity neg)
                        (case ((- - - - -)      
                               (?acc ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?as ?am ?af ?an ?ap))))
               (HASH meaning ((accompany-01 ?x)
                              (polarity ?x neg)))
               --
               (HASH form ((string ?without-word "ohne")))))
             :disable-automatic-footprints t)

(def-fcg-cxn ohne-incorrect-case-cxn
             ((?without-word
               (footprints (preposition)))
              <-
              (?without-word
               (referent ?x)
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (type accompanying)
                        (polarity neg)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (?dat ?dm ?df ?dn ?dp)      
                               (?gen ?gm ?gf ?gn ?gp)
                               (?s ?m ?f ?n ?p))))
               (error-cat (ERROR incorrect-case-selection)
                        (REASON accusative-case-needed))
               (HASH meaning ((accompany-01 ?x)
                              (polarity ?x neg)))
               --
               (HASH form ((string ?without-word "ohne")))))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)

(def-fcg-cxn durch-cxn
             ((?through-word
               (footprints (preposition)))
              <-
              (?through-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type motion-locative-medium)
                        (case ((- - - - -)      
                               (?acc ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?as ?am ?af ?an ?ap))))
               --
               (HASH form ((string ?through-word "durch")))))
             :disable-automatic-footprints t)


(def-fcg-cxn durch-err-cxn
             ((?through-word
               (footprints (preposition)))
              <-
              (?through-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type motion-locative-medium)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (- - - - -)      
                               (+ + - - -)
                               (+ + - - -))))
               (error-cat (error incorrect-case-choice)
                          (reason durch-needs-the-accusative-not-dative-case))
               --
               (HASH form ((string ?through-word "durch")))))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)

(comprehend "durch dem Tunnel")

(def-fcg-cxn gegen-cxn
             ((?against-word
               (footprints (preposition)))
              <-
              (?against-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type motion-locative-end)
                        (case ((- - - - -)      
                               (?acc ?am ?af ?an ?ap)        
                               (- - - - -)      
                               (- - - - -)
                               (?as ?am ?af ?an ?ap))))
               --
               (HASH form ((string ?against-word "gegen")))))
             :disable-automatic-footprints t)


(def-fcg-cxn gegen-nom-cxn
             ((?against-word
               (footprints (preposition)))
              <-
              (?against-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (polarity pos)
                        (type motion-locative-end)
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np))))
               --
               (HASH form ((string ?against-word "gegen")))))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)
             
(def-fcg-cxn zum-cxn
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
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -))))
               --
               (HASH form ((string ?to-word "zum"))))))


(def-fcg-cxn zu-cxn
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
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -))))
               --
               (HASH form ((string ?to-word "zu"))))))

(def-fcg-cxn Tunnel-cxn
             ((?tunnel-word                        
               (referent ?t)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy inanimate)))
              <-
              (?tunnel-word                            
               (HASH meaning ((tunnel ?t)))                    
               --
               (HASH form ((string ?tunnel-word  "Tunnel"))))))

(def-fcg-cxn Sohn-cxn
             ((?son-word                        
               (referent ?s)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?son-word                            
               (HASH meaning ((son ?s)))                    
               --
               (HASH form ((string ?son-word  "Sohn"))))))

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

(def-fcg-cxn spaziert-cxn
             ((?walk-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type intransitive-loc))
               (referent ?s))  
                        
              <-
              (?walk-word                           
               (HASH meaning ((spazieren-01 ?s)))                   
               --
               (HASH form ((string ?walk-word  "spaziert"))))))
(comprehend "spaziert")


(def-fcg-cxn ist-gefahren-cxn
             ((?drove-word
               (subunits (?aux-unit ?participle-unit))
               (syn-cat (lex-class verb)
                        (aspect perfect)
                        (type single-intransitive))
               (boundaries (leftmost-unit ?aux-unit)
                           (rightmost-unit ?participle-unit))
               (referent ?ig))
            
              <-

              (?aux-unit
               --
               (HASH form ((string ?aux-unit "ist"))))

              (?participle-unit
               --
               (HASH form ((string ?participle-unit "gefahren"))))

              (?drove-word                           
               (HASH meaning ((drove-01 ?ig)))                    
               --
               )))

(def-fcg-cxn ist-gefallen-cxn
             ((?fell-word
               (subunits (?aux-unit ?participle-unit))
               (syn-cat (lex-class verb)
                        (aspect perfect)
                        (type single-intransitive))
               (error-cat (error variation-from-stimulus)
                          (reason this-verb-differs-from-the-stimulus-gefahren))
               (boundaries (leftmost-unit ?aux-unit)
                           (rightmost-unit ?participle-unit))
               (referent ?ig))
            
              <-

              (?aux-unit
               --
               (HASH form ((string ?aux-unit "ist"))))

              (?participle-unit
               --
               (HASH form ((string ?participle-unit "gefallen"))))

              (?fell-word                           
               (HASH meaning ((fell-01 ?ig)))                    
               --
               )))

(def-fcg-cxn incorrect-noun-phrase-cxn
             ((?incorrect-noun-phrase
               (referent ?x)
               (syn-cat (lex-class noun-phrase)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               (error-cat (error ?e)
                          (reason ?r)
                          (extra-info ?e))
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
               (referent ?w)
               (syn-cat (lex-class noun)
                        (case ?case)
                        )
               (sem-cat (animacy ?animacy))
               --
               (footprints (not determined))
               (referent ?w)
               (syn-cat (lex-class noun)
                        (case ?case))
               (error-cat (error ?e)
                         (reason ?r)))
              (?incorrect-noun-phrase
               --
               (HASH form ((meets ?article ?noun)))
              ))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)


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
                               (?acc ?am ?af ?an ?ap)      
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
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?prep-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t)


(def-fcg-cxn incorrect-prepositional-phrase-cxn
             ((?incorrect-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type ?type)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (sem-cat (animacy ?animacy))
               (error-cat (error ?e)
                          (reason ?r))
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
                        (type motion-locative-medium)
                        (polarity ?polarity)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               (error-cat (error ?e)
                          (reason ?r)))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?incorrect-prep-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)

(comprehend "durch dem Tunnel")


(def-fcg-cxn prepositional-phrase-with-error-cxn
             ((?prep-phrase-with-error
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type ?type)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (error-cat (error ?e)
                          (reason ?r)
                          (extra-info ?e))
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
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p))))
               (error-cat (error ?e)
                         (reason ?r)))  
              
              (?prep-phrase-with-error
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)


(comprehend "den Wurm")


(def-fcg-cxn incorrect-contracted-prep-phrase-cxn
             ((?incorrect-contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type ?type)
                        (polarity ?polarity)
                        (form-type contracted))
               (sem-cat (animacy ?animacy))
               (error-cat (ERROR missing-determiner)
                        (REASON determiner-in-dative-case-needed))
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
              
              (?incorrect-contracted-prep-phrase
               --
               (HASH form ((meets ?contracted-prep ?noun)))
              ))
             :disable-automatic-footprints t
             :cxn-set mal-cxn)




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


(def-fcg-cxn incorrect-accompanying-phrase-cxn
             ((?incorrect-accompanying-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type accompanying)
                        (form-type extended-accompanying-prep-phrase)
                        (polarity ?polarity))
               (error-cat (ERROR ?error)
                          (REASON ?reason))
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
                               (- - - - -)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p))))
               (error-cat (error ?error)
                        (reason ?reason)))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               --
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))))
              
              (?incorrect-accompanying-phrase
               (HASH meaning ((arg0 ?x ?no)))
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
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
                               (- - - - -)
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
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p))))
               --
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              
              (?accompanying-phrase
               (HASH meaning ((arg0 ?x ?no)))
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t)

(comprehend "durch den Tunnel")

(def-fcg-cxn accompanying-phrase-poss-cxn
             ((?accompanying-phrase-poss
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))
                        (type accompanying)
                        (form-type extended-accompanying-prep-phrase)
                        (polarity ?polarity))
               (error-cat (deviation-from-input possessive-adj-no-art))
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
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              (?adj
               --
               (syn-cat (lex-class possessive-adj)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              (?noun
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p))))
               --
               (referent ?no)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?s ?m ?f ?n ?p)))))
              
              (?accompanying-phrase-poss
               (HASH meaning ((arg0 ?x ?no)))
               --
               (HASH form ((meets ?preposition ?adj)
                           (meets ?adj ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set dev-rule)

(def-fcg-cxn incorrect-prepositional-phrase-cxn
             ((?incorrect-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np)))
                        (type ?type)
                        (form-type extended-prep-phrase)
                        (polarity ?polarity))
               (sem-cat (animacy ?animacy))
               (error-cat (error incorrect-case-selection)
                          (reason gegen-needs-an-accusative-not-dative-case))
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
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np)))))
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np)))))
              (?noun
               (referent ?x)
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np))))
               --
               (footprints (not determined))
               (syn-cat (lex-class noun)
                        (case ((+ ?nm ?nf ?nn ?np)      
                               (- - - - -)        
                               (- - - - -)      
                               (- - - - -)
                               (?ns ?nm ?nf ?nn ?np)))))
              (?incorrect-prep-phrase
               --
               (HASH form ((meets ?preposition ?article)
                           (meets ?article ?noun)))
              ))
              :disable-automatic-footprints t
              :cxn-set mal-cxn)

(def-fcg-cxn intransitive-argument-structure-perfect-cxn
             ((?intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))  
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect perfect))
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

              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-end)
                        (form-type extended-prep-phrase)
                   (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
              (referent ?arg1))
              
              (?intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               )))

(def-fcg-cxn var-intransitive-argument-structure-perfect-cxn
             ((?variation-intransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement))
               (error-cat (error ?e)
                         (reason ?r)
                         (extra-info ?e)))  
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect perfect))
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

              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-end)
                        (form-type extended-prep-phrase)
                   (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
              (error-cat (error ?e)
                         (reason ?r)
                         (extra-info ?e))
              (referent ?arg1))
              
              (?variation-intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               ))
             :cxn-set mal-cxn)


(def-fcg-cxn incorrect-intransitive-argument-structure-perfect-cxn
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
                       (type single-intransitive)
                       (aspect perfect))
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

              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-end)
                        (form-type extended-prep-phrase)
                   (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
              (error-cat (error ?e)
                         (reason ?r))
              (referent ?arg1))
              
              (?incorrect-intransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)))                  
               --
               ))
             :cxn-set mal-cxn)


(def-fcg-cxn topic-arg0-arg1-perfect-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?aux-unit)
                           (meets ?aux-unit ?leftmost-location-unit)
                           (meets ?rightmost-location-unit ?participle-unit)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (subunits (?aux-unit ?participle-unit))
               (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect perfect))
               (boundaries (leftmost-unit ?aux-unit)
                          (rightmost-unit ?participle-unit))
              
                --
              (subunits (?aux-unit ?participle-unit))
              (syn-cat (lex-class verb)
                       (type single-intransitive)
                       (aspect perfect))
              (boundaries (leftmost-unit ?aux-unit)
                          (rightmost-unit ?participle-unit)))
              
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

(def-fcg-cxn incorrect-case-intransitive-extra-argument-structure-cxn
             ((?incorrect-intransitive-extra-argument-structure-unit
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

              (?extra-info-unit
               (referent ?accompany)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (- - - - -)      
                               (- - - - -)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?dat ?dm ?df ?dn ?dp)))
                        (type accompanying))
              (error-cat (error ?error)
                         (reason ?reason))
              (referent ?accompany))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (?dat ?dm ?df ?dn ?dp)
                          (?ls ?dm ?df ?dn ?lp))))
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg4))
              
              (?incorrect-intransitive-extra-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?accompany)
                              (:arg1 ?accompany ?arg0)
                              (:arg4 ?v ?arg4)
                              (:incorrect-part ?accompany)))                  
               --
               ))
             :cxn-set mal-cxn)
             


(def-fcg-cxn incorrect-loc-intransitive-extra-argument-structure-cxn
             ((?incorrect-loc-intransitive-extra-argument-structure-unit
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

              (?extra-info-unit
               (referent ?accompany)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type accompanying))
              
              (referent ?accompany))
         
              (?location-unit
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (error-cat (error ?error)
                         (reason ?reason))
              (referent ?arg4))
              
              (?incorrect-loc-intransitive-extra-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?accompany)
                              (:arg1 ?accompany ?arg0)
                              (:arg4 ?v ?arg4)
                              (:incorrect-part ?arg4)))                  
               --
               ))
             :cxn-set mal-cxn)

(def-fcg-cxn intransitive-extra-argument-structure-cxn
             ((?intransitive-extra-argument-structure-unit
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?as ?am ?af ?an ?ap)))
                   (type accompanying))
               (referent ?accompany)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type accompanying))
              (referent ?accompany))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (?dat ?dm ?df ?dn ?dp)
                          (?ls ?dm ?df ?dn ?lp))))
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg4))
              
              (?intransitive-extra-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?accompany)
                              (:arg1 ?accompany ?arg0)
                              (:arg4 ?v ?arg4)))                  
               --
               )))

(def-fcg-cxn intransitive-arg0-arg2-argument-structure-cxn
             ((?intransitive-arg0-arg2-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))  
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive-loc)
                       (aspect non-perfect))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive-loc)
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
                        (type motion-locative-medium)
                        (form-type extended-prep-phrase)
                   (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
               (referent ?arg2)
                --
              (syn-cat (lex-class prep-phrase)
                       (type motion-locative-medium)
                        (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (- - - - -)
                      (?ls ?am ?af ?an ?lp))))
              (referent ?arg2))
              
              (?intransitive-arg0-arg2-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg2 ?v ?arg2)))                  
               --
               )))

(comprehend "der Mann spaziert durch den Tunnel")

(def-fcg-cxn dev-intransitive-extra-argument-structure-cxn
             ((?intransitive-extra-argument-structure-unit
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

              (?extra-info-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (- - - - -)       
                               (- - - - -)
                               (?as ?am ?af ?an ?ap)))
                   (type accompanying))
               (referent ?accompany)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type accompanying))
              (referent ?accompany))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                        (type motion-locative-contracted)
                        (form-type contracted)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (?dat ?dm ?df ?dn ?dp)
                          (?ls ?dm ?df ?dn ?lp))))
               (referent ?arg4)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg4))
              
              (?intransitive-extra-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:manner ?v ?accompany)
                              (:arg1 ?accompany ?arg0)
                              (:arg4 ?v ?arg4)))                  
               --
               ))
             :cxn-set dev-rule)
             

(def-fcg-cxn topic-arg0-arg2-information-structure-cxn
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
                       (type intransitive-loc))
                --
              (syn-cat (lex-class verb)
                       (type intransitive-loc)))
              
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

(def-fcg-cxn topic-arg0-extra-info-arg4-information-structure-cxn
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
                       (type intransitive))
                --
              (syn-cat (lex-class verb)
                       (type intransitive)))
              
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


(def-fcg-cxn topic-err-arg0-extra-info-arg4-information-structure-cxn
             (
              <-
              (?err-argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-extra-info-unit)
                           (meets ?rightmost-extra-info-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type intransitive))
                --
              (syn-cat (lex-class verb)
                       (type intransitive)))
              
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


(comprehend "die Mutter geht ohne den Sohn zum Laden")


;;;;additional errors



;der Vater zeigt der Sohn die Brille.

;der Mann wurde bei mir gesehen - ich sehe den Mann 
;
(comprehend "der Mann spaziert durch den Tunnel")

;;;;ERRORS

(comprehend "der Mann ist gegen den Wurm gefahren")

(comprehend "zu Laden")
(comprehend "ohne dem Sohn")
(comprehend "ohne ihren Sohn")
(comprehend "gegen der Baum")
(comprehend "der Mann ist gegen der Baum gefahren")
(comprehend "der Mann ist gegen den Baum gefallen")
(comprehend "die Mutter geht ohne den Sohn zu Laden")
(comprehend "die Mutter geht ohne dem Sohn zum Laden")
(comprehend "die Mutter geht ohne dem Sohn zu Laden")
(comprehend "die Mutter geht ohne ihren Sohn zu Laden")
(comprehend "die Mutter geht ohne ihren Sohn zum Laden")


;;;FORMULATION

;der Mann ist gegen den Baum gefahren
;(formulate '((tree b) (man m) (drove-01 ig) (arg1 ig b) (arg0 ig m) (topicalized m +)))

; die Mutter geht ohne den Sohn zum Laden
;(formulate '((MOTHER m) (ACCOMPANY-01 x) (POLARITY x neg) (STORE l) (SON s) (ARG0 x s) (GEHEN-01 g) (ARG4 g l) (ARG1 x m) (MANNER g x) (ARG0 g m) (TOPICALIZED m +)))
