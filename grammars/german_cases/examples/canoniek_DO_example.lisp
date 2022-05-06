
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
                       (:node-tests :malrule-applied :restrict-search-depth :restrict-nr-of-nodes :check-duplicate)
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-sets) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       ;; cxn sets
                       (:parse-order cxn dev-rule malrule)
                       (:production-order cxn malrule dev-rule)
                       ;; goal tests
                       (:production-goal-tests
                        :no-applicable-cxns :connected-structure
                        :no-meaning-in-root)))


(defmethod cip-node-test ((node cip-node) (mode (eql :malrule-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'malrule)
    (and (push 'malrule-applied (statuses node))
         t)
      t
      ))


#|(defmethod cip-node-test ((node cip-node) (mode (eql :dev-rule-applied)))
  (if (equal (attr-val (first (applied-constructions node)) :label) 'dev-rule)
    (and (push 'deviation-from-input (statuses node))
         t)
      t
      ))|#



(pushnew '((malrule-applied) .  "#eb4034;") *status-colors*)

;(pushnew '((dev-rule-applied) .  "#4c34eb;") *status-colors*)

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


(def-fcg-cxn Direktor-cxn
             ((?director-word                      
               (referent ?d)
               (syn-cat (lex-class noun)
                        (case ((?nom ?nm - - ?np)    
                               (?acc ?am - - -)     
                               (?pg - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?director-word                           
               (HASH meaning ((director ?d)))              
               --
               (HASH form ((string ?director-word "Direktor"))))))

(def-fcg-cxn Doktor-cxn
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
               (HASH form ((string ?doctor-word  "Doktor"))))))




(def-fcg-cxn Buch-cxn
             ((?book-word                        
               (referent ?b)
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))
                        (sem-cat (animacy inanimate)))
              <-
              (?book-word                            
               (HASH meaning ((book ?b)))                    
               --
               (HASH form ((string ?book-word  "Buch"))))))



(def-fcg-cxn Clown-cxn
             ((?clown-word                        
               (referent ?c)
               (syn-cat (lex-class noun)
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?clown-word                            
               (HASH meaning ((clown ?c)))                    
               --
               (HASH form ((string ?clown-word  "Clown"))))))


(def-fcg-cxn Lehrerin-cxn
             ((?teacher-word                        
               (referent ?t)
               (syn-cat (lex-class noun)
                        (case ((?nf - ?nf - -)     
                               (?af - ?af - -)      
                               (?gf - ?gf - -)       
                               (?df - ?df - -)
                               (+ - + - -))))
               (sem-cat (animacy animate)))
              <-
              (?teacher-word
               (HASH meaning ((teacher ?t)))                     
               --
               (HASH form ((string ?teacher-word  "Lehrerin"))))))

(def-fcg-cxn Blumen-cxn
             ((?flowers-word
               (referent ?fl)                             ;set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
                        (case ((?np - - - ?np)     
                               (?ap - - - ?ap)      
                               (?gp - - - ?gp)       
                               (?dp - - - ?dp)
                               (- - - - +))))
              (sem-cat (animacy inanimate)))
              <-
              (?flowers-word
               (HASH meaning ((flowers ?fl)))                     
               --
               (HASH form ((string ?flowers-word  "Blumen"))))))


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

(def-fcg-cxn verkauft-cxn
             ((?sell-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?v))  
                        
              <-
              (?sell-word                           
               (HASH meaning ((verkaufen-01 ?v)))
               --
               (HASH form ((string ?sell-word  "verkauft"))))))


(def-fcg-cxn schenkt-cxn
             ((?gift-word
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (referent ?g))  
                        
              <-
              (?gift-word                           
               (HASH meaning ((schenken-01 ?g)))
               --
               (HASH form ((string ?gift-word  "schenkt"))))))


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
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase)
               (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (referent ?arg2))
              
              (?ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --
               )))

(def-fcg-cxn incorrect-receiver-ditransitive-argument-structure-cxn
             ((?double-accusative-incorrect-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object)))
              (?receiver-unit
               (syn-cat (syn-role direct-object))
               (error-cat (error incorrect-case-selection-for-this-argument-role)
                          (reason this-argument-should-be-a-receiver-in-dative-case-not-another-object-in-accusative)))
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
                               (?as ?nm ?nf ?nn ?np))) )
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
                        )
               (referent ?arg1e)
                --
              (syn-cat 
                (lex-class noun-phrase)
                        )
              (sem-cat (animacy animate))
              (referent ?arg1e))
              
              (?double-accusative-incorrect-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg1)
                              (:arg1 ?v ?arg1e)
                              ;(:arg1-error ?v ?arg1e)
                              (:arg2 ?v missing-because-of-error)
                              ))                  
               --
               ))
             :cxn-set malrule)
             

(comprehend "der Doktor verkauft den Clown das Buch")


(def-fcg-cxn topic-arg0-arg1-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-receiver-unit)
                           (meets ?rightmost-receiver-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (referent ?arg0)
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role indirect-object))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (syn-cat (syn-role indirect-object))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              ))


;;;error dative plural
;"die Lehrerin schenkt dem Direktor den Blumen"

(def-fcg-cxn incorrect-patient-ditransitive-argument-structure-cxn
             ((?double-dative-incorrect-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (error-cat (error incorrect-case-selection-for-this-argument-role)
                          (extra-error-info incorrect-accusative-determiner-for-plural-noun-or-incorrect-dative-case-selection)
                          (reason this-argument-should-be-a-patient-or-object-in-accusative-not-another-receiver-in-dative-plural)))
              (?receiver-unit
               (syn-cat (syn-role subject))
               )
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
                        )
               (sem-cat (animacy inanimate))
               (referent ?arg2)
                --
              (syn-cat (lex-class noun-phrase))
              (sem-cat (animacy inanimate))
              (referent ?arg2))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp)))
                        )
               (referent ?arg2)
                --
              (syn-cat 
                (lex-class noun-phrase)
                        (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              ;(sem-cat (animacy animate))
              (referent ?arg2))
              
              (?double-dative-incorrect-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v missing-because-of-error)
                              (:arg2 ?v ?arg2)
                              ))                  
               --
               ))
             :cxn-set malrule)


;;; two errors
;"die Lehrerin schenkt den Direktor den Blumen"

(def-fcg-cxn double-incorrect-role-ditransitive-argument-structure-cxn
             ((?mixed-accusative-and-dative-in-ditransitive-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (error-cat (error incorrect-case-selection-for-this-argument-role)
                          (extra-error-info incorrect-accusative-determiner-for-plural-noun-or-incorrect-dative-case-selection)
                          (reason this-argument-should-be-a-patient-or-object-in-accusative-not-dative-plural)))
              (?receiver-unit
               (syn-cat (syn-role subject)
                        (error-cat (error incorrect-case-selection-for-this-argument-role)
                          (reason this-argument-should-be-a-receiver-in-dative-not-another-patient-or-object-in-accusative))))
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
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                       (case ((- - - - -) 
                      (- - - - -)         
                      (- - - - -)         
                      (+ ?dm ?df ?dn ?dp)
                      (?rs ?dm ?df ?dn ?dp))))
              (sem-cat (animacy inanimate))
              (referent ?arg1))
              
              (?receiver-unit
               (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
               (referent ?arg2)
                --
              (syn-cat 
                (lex-class noun-phrase)
                (case ((- - - - -) 
                               (+ ?am ?af ?an ?ap)         
                               (- - - - -)         
                               (- - - - -)
                               (?ps ?am ?af ?an ?ap))))
              (referent ?arg2))
              
              (?mixed-accusative-and-dative-in-ditransitive-argument-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:arg1 ?v ?arg2)
                              (:arg2 ?v ?arg1)
                              ))                  
               --
               ))
             :cxn-set malrule)

(comprehend "die Lehrerin schenkt den Direktor den Blumen")

(def-fcg-cxn topic-arg0-arg1-arg2-incorrect-information-structure-cxn
             (
              <-
              (?incorrect-argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               ;(HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((meets ?rightmost-agent-unit ?verb-unit)
                           (meets ?verb-unit ?leftmost-receiver-unit)
                           (meets ?rightmost-receiver-unit ?leftmost-patient-unit)))
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type ditransitive))     
              
                --
              (syn-cat (lex-class verb)
                       (type ditransitive)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (referent ?arg0)
              (boundaries (leftmost-unit ?leftmost-agent-unit)
                          (rightmost-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries (leftmost-unit ?leftmost-patient-unit)
                          (rightmost-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (syn-cat (syn-role subject))
               (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries (leftmost-unit ?leftmost-receiver-unit)
                          (rightmost-unit ?rightmost-receiver-unit)))
              
              )
             :cxn-set malrule)



;;;;FORMULATION

;;;der Doktor verkauft dem Clown das Buch
(formulate '((verkaufen-01 s) (doctor d) (clown c) (book b) (arg0 s d) (arg1 s b) (arg2 s c) (topicalized d +)))

;;;die Lehrerin schenkt dem Direktor die Blumen
(formulate '((teacher t) (flowers f) (director d) (schenken-01 g) (arg2 g d) (arg1 g f) (arg0 g t) (topicalized t +)))




;;;;;;ERRORS

(comprehend "der Doktor verkauft den Clown das Buch")

(comprehend "die Lehrerin schenkt dem Direktor den Blumen")

(comprehend "die Lehrerin schenkt den Direktor den Blumen")


