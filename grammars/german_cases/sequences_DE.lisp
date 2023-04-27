;; April 2023, Veronica Schmalz 
;; German grammar based on sequences of chars instead of words ( after introduction of form as set of sequence predicates)


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
                       (:render-mode . :render-sequences)
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




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; DETERMINERS CONSTRUCTIONS                 ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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


(def-fcg-cxn das-cxn
             ((?the-word
               (footprints (article))
               (sequences ((sequence "das" ?left ?right)))) 
              <-
              (?the-word
               (footprints (not article))
               (syn-cat (lex-class article)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -))))   ;sing, masc, fem, neut, plural
               
               --
               (HASH form ((sequence "das" ?left ?right)))))
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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; PREPOSITIONS CONSTRUCTIONS                ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-fcg-cxn mit-cxn
             ((?with-word
               (footprints (preposition))
               (sequences ((sequence "mit" ?left ?right))))
              <-
              (?with-word
               (footprints (not preposition))
               (syn-cat (lex-class preposition)
                        (case ((- - - - -)      
                               (- - - - -)        
                               (- - - - -)      
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?dm ?df ?dn ?dp))))
               --
               (HASH form ((sequence "mit" ?left ?right)))))
             :disable-automatic-footprints t)


(def-fcg-cxn zur-cxn
             ((?to-word
              (footprints (article))
              (sequences ((sequence "zur" ?left ?right))))
              <-
              (?to-word
               (footprints (not article))
               (syn-cat (lex-class contracted-preposition)
                        (type contracted)
                        (locative motion)
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ - ?df - -)
                               (+ - ?df - -))))
               --
               (HASH form ((sequence "zur" ?left ?right)))))
              :disable-automatic-footprints t)


(def-fcg-cxn zum-cxn
             ((?to-the-word
              (footprints (article))
              (sequences ((sequence "zum" ?left ?right))))
              <-
              (?to-the-word
               (footprints (not article))
               (syn-cat (lex-class contracted-preposition) 
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -)))
                        (type contracted)
                        (locative motion))
               --
               (HASH form ((sequence "zum" ?left ?right)))))
             :disable-automatic-footprints t)


(def-fcg-cxn beim-cxn
             ((?at-the-word
              (footprints (article))
              (sequences ((sequence "beim" ?left ?right))))
              <-
              (?at-the-word
               (footprints (not article))
               (syn-cat (lex-class contracted-preposition) 
                        (case ((- - - - -)    ;nom, acc, gen, dat  (nom masculine)
                               (- - - - -)        ;masc, fem, neut, plural
                               (- - - - -)    ;genitive feminine
                               (+ ?dm - ?dn -)
                               (+ ?dm - ?dn -)))
                        (type contracted)
                        (locative static))
               --
               (HASH form ((sequence "beim" ?left ?right)))))
              :disable-automatic-footprints t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; LEXICAL CONSTRUCTIONS                     ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(def-fcg-cxn Vater-cxn
             ((?father-word
               (referent ?f)
               (sequences ((sequence "Vater" ?left ?right)))
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?father-word
               (HASH meaning ((father ?f)))                     
               --
               (HASH form ((sequence "Vater" ?left ?right))))))

(def-fcg-cxn Sohn-cxn
             ((?son-word
               (referent ?s)
               (sequences ((sequence "Sohn" ?left ?right)))
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
               (HASH form ((sequence "Sohn" ?left ?right))))))


(def-fcg-cxn Clown-cxn
             ((?clown-word
               (referent ?c)
               (sequences ((sequence "Clown" ?left ?right)))
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
               (HASH form ((sequence "Clown" ?left ?right))))))

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


(def-fcg-cxn Apfel-cxn
             ((?apple-word
               (referent ?a)
               (sequences ((sequence "Apfel" ?left ?right)))
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy inanimate)))
              <-
              (?apple-word
               (HASH meaning ((apple ?a)))                     
               --
               (HASH form ((sequence "Apfel" ?left ?right))))))


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

(def-fcg-cxn Baecker-cxn
             ((?baker-word
               (referent ?b)
               (sequences ((sequence "Baecker" ?left ?right)))
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
               (HASH form ((sequence "Baecker" ?left ?right))))))

;(formulate-all '((baker ?b)))


(def-fcg-cxn Polizist-cxn
             ((?policeman-word
               (referent ?p)
               (sequences ((sequence "Polizist" ?left ?right)))
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
               (HASH form ((sequence "Polizist" ?left ?right))))))


(def-fcg-cxn Jaeger-cxn
             ((?hunter-word
               (referent ?j)
               (sequences ((sequence "Jaeger" ?left ?right)))
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
               (HASH form ((sequence "Jaeger" ?left ?right))))))


(def-fcg-cxn Koenig-cxn
             ((?king-word
               (referent ?k)
               (sequences ((sequence "Koenig" ?left ?right)))
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
               (HASH form ((sequence "Koenig" ?left ?right))))))

(def-fcg-cxn Kellner-cxn
             ((?waiter-word
               (referent ?k)
               (sequences ((sequence "Kellner" ?left ?right)))
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
               (HASH form ((sequence "Kellner" ?left ?right))))))

(def-fcg-cxn Direktor-cxn
             ((?director-word
               (referent ?d)
               (sequences ((sequence "Direktor" ?left ?right)))
               (syn-cat (lex-class noun)         
                        (case ((?nm ?nm - - -)     
                               (?am ?am - - -)      
                               (- - - - -)       
                               (?dm ?dm - - -)
                               (+ + - - -))))
               (sem-cat (animacy animate)))
              <-
              (?director-word
               (HASH meaning ((principal ?d)))                     
               --
               (HASH form ((sequence "Direktor" ?left ?right))))))

(def-fcg-cxn Tiger-cxn
             ((?tiger-word
               (referent ?t)
               (sequences ((sequence "Tiger" ?left ?right)))
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
               (HASH form ((sequence "Tiger" ?left ?right))))))


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


(def-fcg-cxn Lehrerin-cxn
             ((?teacher-word                        
               (referent ?t)
               (sequences ((sequence "Lehrerin" ?left ?right)))
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
               (HASH form ((sequence "Lehrerin" ?left ?right))))))


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

(def-fcg-cxn Fahrrad-cxn
             ((?bike-word                        
               (referent ?x)
               (sequences ((sequence "Fahrrad" ?left ?right)))
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -)))))
              <-
              (?bike-word                            
               (HASH meaning ((bike ?x)))                
               --
               (HASH form ((sequence "Fahrrad" ?left ?right))))))

(def-fcg-cxn Maedchen-cxn
             ((?girl-word                        
               (referent ?g)
               (sequences ((sequence "Maedchen" ?left ?right)))
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
               (HASH form ((sequence "Maedchen" ?left ?right))))))


(def-fcg-cxn Buch-cxn
             ((?book-word                        
               (referent ?b)
               (sequences ((sequence "Buch" ?left ?right)))
               (syn-cat (lex-class noun)
                        (case ((?nn - - ?nn -)     
                               (?an - - ?an -)      
                               (- - - - -)       
                               (?dn - - ?dn -)
                               (+ - - + -)))))
              <-
              (?book-word                            
               (HASH meaning ((book ?b)))                
               --
               (HASH form ((sequence "Buch" ?left ?right))))))


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


(def-fcg-cxn Brille-cxn
             ((?glasses-word
               (referent ?g)
               (sequences ((sequence "Brille" ?left ?right)));set of values
               (syn-cat (lex-class noun)                   ;sure nominative and masculine
                        (case ((?np - - - ?np)     
                               (?ap - - - ?ap)      
                               (?gp - - - ?gp)       
                               (?dp - - - ?dp)
                               (- - - - +))))
              (sem-cat (animacy inanimate)))
              <-
              (?glasses-word
               (HASH meaning ((glasses ?g)))                     
               --
               (HASH form ((sequence "Brille" ?left ?right))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; DITRANSITIVE VERBS CXNS                   ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(def-fcg-cxn gibt-cxn
             ((?give-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?g))  
                        
              <-
              (?give-word                           
               (HASH meaning ((geben-01 ?g)))                   
               --
               (HASH form ((sequence "gibt" ?verb-left ?verb-right))))))


(def-fcg-cxn zeigt-cxn
             ((?show-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?s))  
                        
              <-
              (?show-word                           
               (HASH meaning ((zeigen-01 ?s)))                   
               --
               (HASH form ((sequence "zeigt" ?verb-left ?verb-right))))))


(def-fcg-cxn verkauft-cxn
             ((?sell-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type ditransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?s))  
                        
              <-
              (?sell-word                           
               (HASH meaning ((verkaufen-01 ?s)))                   
               --
               (HASH form ((sequence "verkauft" ?verb-left ?verb-right))))))



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;       MOTION VERBS CXNS                   ;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INTRANSITIVES


(def-fcg-cxn ist-gefahren-cxn
             ((?drove-word
               (subunits (?aux-unit ?participle-unit))
               (syn-cat (lex-class verb)
                        (aspect perfect)
                        (type intransitive))
               (boundaries (?left-aux-unit ?right-participle-unit))
               (referent ?ig))
            
              <-

              (?aux-unit
               --
               (HASH form ((sequence "ist" ?left-aux-unit ?right-aux-unit))))

              (?participle-unit
               --
               (HASH form ((sequence "gefahren" ?left-participle-unit ?right-participle-unit))))

              (?drove-word                           
               (HASH meaning ((drove-01 ?ig)))                    
               --
               )))


(def-fcg-cxn fährt-cxn
             ((?drive-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type single-intransitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?f))  
                        
              <-
              (?drive-word                           
               (HASH meaning ((fahren-01 ?f)))                   
               --
               (HASH form ((sequence "fährt" ?verb-left ?verb-right))))))


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


(def-fcg-cxn ist-cxn
             ((?be-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type copula))
               (boundaries (?verb-left ?verb-right))
               (referent ?s))  
                        
              <-
              (?be-word                           
               (HASH meaning ((sein-01 ?s)))                   
               --
               (HASH form ((sequence "ist" ?verb-left ?verb-right))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TRANSITIVE VERBS CXNS         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(def-fcg-cxn verfolgt-cxn
             ((?follow-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?f))  
                        
              <-
              (?follow-word                           
               (HASH meaning ((verfolgen-01 ?f)))                   
               --
               (HASH form ((sequence "verfolgt" ?verb-left ?verb-right))))))


(def-fcg-cxn toetet-cxn
             ((?kill-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?k))  
                        
              <-
              (?kill-word                           
               (HASH meaning ((toeten-01 ?k)))                   
               --
               (HASH form ((sequence "toetet" ?verb-left ?verb-right))))))


(def-fcg-cxn ruft-cxn
             ((?call-word                         
               (syn-cat (lex-class verb)
                        (aspect non-perfect)
                        (type transitive))
               (boundaries (?verb-left ?verb-right))
               (referent ?r))  
                        
              <-
              (?call-word                           
               (HASH meaning ((rufen-01 ?r)))                   
               --
               (HASH form ((sequence "ruft" ?verb-left ?verb-right))))))






  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; PHRASEAL CONSTRUCTIONS                    ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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


(def-fcg-cxn prepositional-phrase-cxn
             ((?prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                        (type non-contracted))
               (subunits (?preposition ?article ?noun))
               (boundaries (?prep-left ?noun-right)))
              (?preposition
               (referent ?x)
               (part-of-prep-phrase +))
              
              (?article
               (referent ?x))
              
              (?noun
               (footprints (determined)))
              <-

              (?preposition
               --
               (syn-cat (lex-class preposition)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p)))
                       )
               (sequences ((sequence ?prep-string ?prep-left ?prep-right))))
              
              (?article
               --
               (syn-cat (lex-class article)
                        (case ((- - - - -)     
                               (?acc ?am ?af ?an ?ap)      
                               (?gen ?gm ?gf ?gn ?gp)       
                               (?dat ?dm ?df ?dn ?dp)
                               (?s ?m ?f ?n ?p))))
               (sequences ((sequence ?article-string ?article-left ?article-right))))
              
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
                               (?s ?m ?f ?n ?p))))
               (sequences ((sequence ?noun-string ?noun-left ?noun-right))))
              (?prep-phrase
               --
               (HASH form ((sequence " " ?prep-right ?article-left)
                           (sequence " " ?article-right ?noun-left)))
              ))
              :disable-automatic-footprints t)



(def-fcg-cxn contracted-prep-phrase-cxn
             ((?contracted-prep-phrase
               (referent ?x)
               (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type contracted)
                        (locative ?locative))
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
                        (type contracted)
                        (case ?case)
                        (locative ?locative))
               (sequences ((sequence ?contr-prep-string ?contr-prep-left ?contr-prep-right))))
              
              (?noun
               (footprints (not determined))
               (referent ?x)
               (syn-cat (lex-class noun)
                        (case ?case))
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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; ARG AND INF CONSTRUCTIONS                 ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                       (type intransitive)
                       (aspect ?a))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect ?a))     
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
                      (?ls ?m ?f ?n ?lp)))
                   (locative ?l))
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
               --
               )))

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
               (syn-cat (syn-role locative-complement)
                        (locative ?locative))
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
               --
               )))


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


(def-fcg-cxn topic-arg2-arg0-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?patient-unit ?receiver-unit))
               (HASH meaning ((topicalized ?arg2 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-receiver-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-agent-unit)
                             (sequence " " ?rightmost-agent-unit ?leftmost-patient-unit)))
               
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
               (syn-cat (syn-role subject))
               (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              (syn-cat (syn-role subject))
              (boundaries  (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?patient-unit
               (syn-cat (syn-role direct-object))
               (boundaries  (?leftmost-patient-unit ?rightmost-patient-unit))
                --
              
              (syn-cat (syn-role direct-object))
              (boundaries  (?leftmost-patient-unit ?rightmost-patient-unit)))

              (?receiver-unit
               (referent ?arg2)
               (syn-cat (syn-role indirect-object))
               (boundaries  (?leftmost-receiver-unit ?rightmost-receiver-unit))
                --
              
              (syn-cat (syn-role indirect-object))
              (referent ?arg2)
              (boundaries  (?leftmost-receiver-unit ?rightmost-receiver-unit)))))

(def-fcg-cxn intransitive-extra-argument-arg1-structure-cxn
             ((?intransitive-extra-argument-arg1-structure-unit
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
                       (type single-intransitive))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive)
                       )     
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
                   (type non-contracted))
               (referent ?extra-info)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ?case)
                        (type non-contracted))
              (referent ?extra-info))
         
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                          (- - - - -)         
                          (- - - - -)         
                          (?dat ?dm ?df ?dn ?dp)
                          (?ls ?dm ?df ?dn ?lp)))
                   (type contracted)
                   (locative motion))
               (referent ?arg1)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (- - - - -)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg1))
              
              (?intransitive-extra-argument-arg1-structure-unit
               (HASH meaning ((:arg0 ?v ?arg0)
                              (:extra-info ?v ?extra-info)
                              (:arg1 ?v ?arg1)))                  
               --
               )))


(def-fcg-cxn topic-arg0-extra-info-arg1-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                             (sequence " " ?verb-right ?leftmost-extra-info-unit)
                             (sequence " " ?rightmost-extra-info-unit ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?extra-info-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type single-intransitive))
               (boundaries (?verb-left ?verb-right))
                --
              (syn-cat (lex-class verb)
                       (type single-intransitive))
              (boundaries (?verb-left ?verb-right)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?extra-info-unit
               (syn-cat (syn-role extra-information)
                        (lex-class prep-phrase)
                        (type non-contracted))
               (boundaries (?leftmost-extra-info-unit ?rightmost-extra-info-unit))
                --
              (syn-cat (syn-role extra-information)
                       (lex-class prep-phrase)
                       (type non-contracted))
              (boundaries (?leftmost-extra-info-unit ?rightmost-extra-info-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase)
                        (type contracted))
               (boundaries (?leftmost-location-unit ?rightmost-location-unit))
                --
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase)
                       (type contracted))
              (boundaries (?leftmost-location-unit ?rightmost-location-unit)))))


;;verb in perfect with locative movement (arg4) der Mann ist zum Kino gefahren

(def-fcg-cxn topic-arg0-arg4-perfect-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg0 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                           (sequence " " ?verb-left ?leftmost-location-unit)
                           (sequence " " ?rightmost-location-unit ?verb-right)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (subunits (?aux-unit ?participle-unit))
               (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect perfect))
               (boundaries (?verb-left ?verb-right))
              
                --
              (subunits (?aux-unit ?participle-unit))
              (syn-cat (lex-class verb)
                       (type intransitive)
                       (aspect perfect))
              (boundaries (?verb-left ?verb-right)))
              
              (?agent-unit
               (referent ?arg0)
               (syn-cat (syn-role subject))
               (boundaries (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              (referent ?arg0)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (?leftmost-location-unit ?rightmost-location-unit))
                --
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (?leftmost-location-unit ?rightmost-location-unit)))))

;locative-copula verbs "die Frau ist beim Doktor"

(def-fcg-cxn locative-copula-argument-structure-cxn
             ((?locative-copula-argument-structure-unit
              (subunits (?verb-unit ?agent-unit ?location-unit)))
              (?agent-unit
               (syn-cat (syn-role subject)))
              (?location-unit
               (syn-cat (syn-role locative-complement)))
              <-
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type copula))
               (referent ?v)
                --
              (syn-cat (lex-class verb)
                       (type copula))     
              (referent ?v))
              
              (?agent-unit
               (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
               (referent ?arg1)
                --
              (syn-cat (lex-class noun-phrase)
                        (case ((+ ?nm ?nf ?nn ?np) 
                               (- - - - -)         
                               (- - - - -)        
                               (- - - - -)
                               (?as ?nm ?nf ?nn ?np))))
              (referent ?arg1))
              
              (?location-unit
               (syn-cat (lex-class prep-phrase)
                   (case ((- - - - -) 
                      (?acc ?am ?af ?an ?ap)         
                      (- - - - -)         
                      (?dat ?dm ?df ?dn ?dp)
                      (?ls ?m ?f ?n ?lp)))
                   (type contracted)
                   (locative static))
               (referent ?arg2)
                --
              (syn-cat (lex-class prep-phrase)
                        (case ((- - - - -) 
                              (?acc ?am ?af ?an ?ap)         
                              (- - - - -)         
                              (?dat ?dm ?df ?dn ?dp)
                              (?ls ?m ?f ?n ?lp))))
              (referent ?arg2))
              
              (?locative-copula-argument-structure-unit
               (HASH meaning ((:arg1 ?v ?arg1)
                              (:arg2 ?v ?arg2)))                  
               --
               )))

(def-fcg-cxn topic-arg1-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg1 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-agent-unit ?verb-left)
                           (sequence " " ?verb-right ?leftmost-location-unit)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type copula))
               (boundaries (?verb-left ?verb-right))
              
                --
              (syn-cat (lex-class verb)
                       (type copula))
              (boundaries (?verb-left ?verb-right)))
              
              (?agent-unit
               (referent ?arg1)
               (syn-cat (syn-role subject))
               (boundaries (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              (referent ?arg1)
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (?leftmost-location-unit ?rightmost-location-unit))
                --
              
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (?leftmost-location-unit ?rightmost-location-unit)))))


(def-fcg-cxn arg1-topic-arg2-information-structure-cxn
             (
              <-
              (?argument-structure-unit
               (subunits (?verb-unit ?agent-unit ?location-unit))
               (HASH meaning ((topicalized ?arg2 +)))  
                          
               --
               (HASH form ((sequence " " ?rightmost-location-unit ?verb-left)
                           (sequence " " ?verb-right ?leftmost-agent-unit)))
               (subunits (?verb-unit ?agent-unit ?location-unit)))
              
              (?verb-unit
               (syn-cat (lex-class verb)
                       (type copula))
               (boundaries (?verb-left ?verb-right))
              
                --
              (syn-cat (lex-class verb)
                       (type copula))
              (boundaries (?verb-left ?verb-right)))
              
              (?agent-unit
               (syn-cat (syn-role subject))
               (boundaries (?leftmost-agent-unit ?rightmost-agent-unit))
                --
              
              (syn-cat (syn-role subject))
              (boundaries (?leftmost-agent-unit ?rightmost-agent-unit)))
              
              (?location-unit
               (referent ?arg2)
               (syn-cat (syn-role locative-complement)
                        (lex-class prep-phrase))
               (boundaries (?leftmost-location-unit ?rightmost-location-unit))
                --
              (referent ?arg2)
              (syn-cat (syn-role locative-complement)
                       (lex-class prep-phrase))
              (boundaries (?leftmost-location-unit ?rightmost-location-unit)))))





;;;;;;TRANSITIVE VERBS;;;;;;;

;(comprehend-and-formulate "der Polizist sucht den Baecker")
;(comprehend-and-formulate "der Koenig ruft den Kellner")
;(comprehend-and-formulate "den Mann verfolgt der Hund")
;(comprehend-and-formulate "den Jaeger toetet der Tiger")

;;;FORMULATION
;(formulate-all '((policeman p) (baker b) (suchen-01 s) (:arg0 s p) (:arg1 s b) (topicalized p +)))
;(formulate '((policeman p) (baker b) (suchen-01 s) (:arg0 s p) (:arg1 s b) (topicalized b +)))


;;;;;DITRANSITIVE VERBS;;;;;;
;(comprehend-and-formulate "die Frau gibt dem Mann den Apfel")
;(comprehend-and-formulate "der Clown verkauft dem Doktor das Buch")
;(comprehend-and-formulate "die Lehrerin schenkt dem Direktor die Blumen")
;(comprehend-and-formulate "dem Sohn zeigt der Vater die Brille")

;;;;FORMULATION

;(formulate '((woman w) (man m) (apple a) (geben-01 g) (:arg0 g m) (:arg1 g a) (:arg2 g w) (topicalized w +))) 
;(formulate '((son s) (father f) (glasses g) (zeigen-01 z) (:arg0 z f) (:arg1 z g) (:arg2 z s) (topicalized f +)))



;;;;;INTRANSITIVE VERBS;;;;;;
;(comprehend-and-formulate "der Mann geht zum Doktor")
;(comprehend-and-formulate "zum Kino geht die Frau")
;(comprehend-and-formulate "das Maedchen ist beim Baecker")
;(comprehend-and-formulate "die Frau ist beim Doktor")
;(comprehend-and-formulate "beim Baecker ist die Frau")


;;;;;INTRANSITIVE VERB WITH EXTRA ARGUMENT;;;;;;
;(comprehend "der Mann fährt mit dem Fahrrad zur Arbeit")


;;;;VERB IN PERFECT FORM
;(comprehend-and-formulate "der Mann ist zum Kino gefahren")    ;error in formulation 









