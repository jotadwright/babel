(in-package :propbank-english)


(def-fcg-constructions propbank-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents))
  :visualization-configurations ((:show-constructional-dependencies . nil))
 
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))

;;;arg-m cxns

;;without auxiliaries

(def-fcg-cxn believe.01-arg-adv-cxn
               ((?believe-unit
                 (args (referent ?o) 
                  (:arg-m-adv ?beta-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-adv ?o ?rb-unit))))
                 <-
                 (?rb-unit
                 --
                 (parent ?vp-unit)
                 (lex-class rb))
                (?vp-unit
                 --
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))

  

;;with auxiliaries

    (def-fcg-cxn believe.01-aux-arg-m-adv-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:arg-m-adv ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-adv ?o ?rb-unit))))
                 <-
                 (?s-unit
                 --
                 (phrase-type (s)))
                 (?rb-unit
                 --
                 (parent ?vp-unit2)
                 (lex-class rb))                 
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                (parent ?vp-unit2))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))
    

        (def-fcg-cxn believe.01-aux-arg-m-mod-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:arg-m-mod ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-mod ?o ?md-unit))))
                 <-
                 (?s-unit
                 --
                 (phrase-type (s)))
                 (?md-unit
                 --
                 (parent ?vp-unit2)
                 (lex-class md))                 
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                (parent ?vp-unit2))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))


;;;sentential complement cxns
;; with auxiliaries

  (def-fcg-cxn believe.01-prp-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp)
                 (dependency-label nsubj)) ;distinguish from believe-01-pass-subj-extraposition-cxn
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))
       


   (def-fcg-cxn believe.01-nnp-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp)) 
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))
       


     (def-fcg-cxn believe.01-np-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np))) 
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))


      (def-fcg-cxn believe.01-nn-aux-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn)) 
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))


;; without auxiliaries

  (def-fcg-cxn believe.01-prp-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp)) 
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))))


    (def-fcg-cxn believe.01-nnp-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))))


       (def-fcg-cxn believe.01-np-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))))


            (def-fcg-cxn believe.01-nn-sentential-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))))
            

;;; prepositional complement cxns
;; with auxiliaries

   (def-fcg-cxn believe.01-prp-aux-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type(vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (pp))) ))


      (def-fcg-cxn believe.01-nnp-aux-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type(vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (pp))) ))


      (def-fcg-cxn believe.01-np-aux-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type(vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (pp))) ))


      (def-fcg-cxn believe.01-nn-aux-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type(vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (pp))) ))
      

;;without auxiliaries

   (def-fcg-cxn believe.01-prp-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (pp))) ))

   (def-fcg-cxn believe.01-nnp-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (pp))) ))


   (def-fcg-cxn believe.01-np-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (pp))) ))



      (def-fcg-cxn believe.01-nn-prepositional-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (pp))) ))
      

;;; nominal complement cxns

;; with auxiliaries

    (def-fcg-cxn believe.01-prp-aux-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (np)))))


        (def-fcg-cxn believe.01-nnp-aux-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (np)))))
        
        (def-fcg-cxn believe.01-np-aux-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (np)))))
        
        (def-fcg-cxn believe.01-nn-aux-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                 (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (np)))))



;; without auxiliaries
   
    (def-fcg-cxn believe.01-prp-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (np)))))


    (def-fcg-cxn believe.01-nnp-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (np)))))

    
   (def-fcg-cxn believe.01-np-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (np)))))


   (def-fcg-cxn believe.01-nn-nominal-complement-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))
                (?y-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (np)))))


;;; sentential complement passive extraction cxns

    (def-fcg-cxn believe.01-prp-sentential-complement-pass-extraction-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit)
                       (:arg2 ?z-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)  
                           (frame-element believed ?o ?y-unit)
                           (frame-element attribute ?o ?z-unit))));attribute, y-unit is believed to be what?
                <-
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?aux-unit
                 --
                 (lemma be)
                 (parent ?vp-unit2))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                --
                (phrase-type (vp))
                (parent ?vp-unit2))
                        
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?z-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit1))))


        (def-fcg-cxn believe.01-nnp-sentential-complement-pass-extraction-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit)
                       (:arg2 ?z-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)  
                           (frame-element believed ?o ?y-unit)
                           (frame-element attribute ?o ?z-unit))));attribute, y-unit is believed to be what?
                <-
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?aux-unit
                 --
                 (lemma be)
                 (parent ?vp-unit2))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                --
                (phrase-type (vp))
                (parent ?vp-unit2))
                        
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?z-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit1))))

  
    (def-fcg-cxn believe.01-np-sentential-complement-pass-extraction-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit)
                       (:arg2 ?z-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)  
                           (frame-element believed ?o ?y-unit)
                           (frame-element attribute ?o ?z-unit))));attribute, y-unit is believed to be what?
                <-
                (?y-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?aux-unit
                 --
                 (lemma be)
                 (parent ?vp-unit2))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                --
                (phrase-type (vp))
                (parent ?vp-unit2))
                        
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?z-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit1))))

        (def-fcg-cxn believe.01-nn-sentential-complement-pass-extraction-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit)
                       (:arg2 ?z-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)  
                           (frame-element believed ?o ?y-unit)
                           (frame-element attribute ?o ?z-unit))));attribute, y-unit is believed to be what?
                <-
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?aux-unit
                 --
                 (lemma be)
                 (parent ?vp-unit2))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                --
                (phrase-type (vp))
                (parent ?vp-unit2))
                        
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?z-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit1))))

        
        (def-fcg-cxn believe.01-dt-sentential-complement-pass-extraction-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit)
                       (:arg2 ?z-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)  
                           (frame-element believed ?o ?y-unit)
                           (frame-element attribute ?o ?z-unit))));attribute, y-unit is believed to be what?
                <-
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class dt))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?aux-unit
                 --
                 (lemma be)
                 (parent ?vp-unit2))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                --
                (phrase-type (vp))
                (parent ?vp-unit2))
                        
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?z-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit1))))


;;; clausal complement cxns

;add: to believe him/her/Winston, etc.; prp, nnp, nn, dt


        (def-fcg-cxn believe.01-prp-clausal-complement-pp-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class prp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (pp)))))



        (def-fcg-cxn believe.01-nnp-clausal-complement-pp-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nnp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (pp)))))


    (def-fcg-cxn believe.01-np-clausal-complement-pp-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (phrase-type (np))
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (pp)))))

    
    (def-fcg-cxn believe.01-nnp-clausal-complement-pp-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nnp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (pp)))))



    (def-fcg-cxn believe.01-prp-clausal-complement-np-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class prp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (np)))))


    (def-fcg-cxn believe.01-nnp-clausal-complement-np-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nnp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (np)))))


    (def-fcg-cxn believe.01-np-clausal-complement-np-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (phrase-type (np))
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (np)))))


    (def-fcg-cxn believe.01-nn-clausal-complement-np-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nn)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (np)))))
    

    
    (def-fcg-cxn believe.01-prp-clausal-complement-sbar-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class prp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (sbar)))))


    (def-fcg-cxn believe.01-np-clausal-complement-sbar-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (phrase-type (np))
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (sbar)))))

    

    (def-fcg-cxn believe.01-nn-clausal-complement-sbar-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nn)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?np-unit1
                 --
                 (phrase-type (np))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?np-unit1)
                 (phrase-type (s vp)))
                
                (?vp-unit1
                 --
                 (parent ?s-vp-unit)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                (phrase-type (sbar)))))


;;; passive subject extraposition construction
   
  (def-fcg-cxn believe.01-pass-subj-extraposition-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believed ?o ?y-unit))))
        
                <-
                (?subj-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp)
                 (dependency-label nsubjpass)) ;distinguish from prp-aux-sentential-complement-cxn
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit2
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (parent ?vp-unit2)
                 (phrase-type (vp)))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
                (?y-unit
                 --
                 (parent ?vp-unit1)
                 (phrase-type (sbar)))))



;;;passive constructions
;;general passive constructions

  (def-fcg-cxn believe.01-prp-passive-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                            (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp))
                (?vp-unit
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?auxpass-unit
                 --
                 (parent ?vp-unit)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))              
 

 (def-fcg-cxn believe.01-np-passive-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                            (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?vp-unit
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?auxpass-unit
                 --
                 (parent ?vp-unit)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))
 

  (def-fcg-cxn believe.01-nnp-passive-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                            (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp))
                (?vp-unit
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?auxpass-unit
                 --
                 (parent ?vp-unit)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))
  

  (def-fcg-cxn believe.01-nn-passive-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                            (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn))
                (?vp-unit
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?auxpass-unit
                 --
                 (parent ?vp-unit)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))
  

        (def-fcg-cxn believe.01-dt-passive-cxn
               ((?believe-unit
                 (args (referent ?o)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                            (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class dt))
                (?vp-unit
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?auxpass-unit
                 --
                 (parent ?vp-unit)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit))))





;;; to do: frames in subordinate clauses, e.g. I say this because I believe it is true

;;; to do: add nnp, nns, etc. in subject positions

;;; to do: negation

;;; to do: adjectival complement cxns  e.g. the chinese are willing to believe ... are eager to believe ...


   )






;;; examples

 (def-fcg-cxn proper-noun-feel-y-cxn
               ((?feel-unit
                 (frame-evoking +)
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (meaning ((frame opine.01 ?o)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element topic ?o ?y-unit))))
                <-
                (?x-unit
                 --
                 (lex-class nnp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?feel-unit
                 --
                 (lemma feel)
                 (parent ?vp-unit))
                (?sbar-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))
                (?y-unit
                 --
                 (parent ?sbar-unit)
                 (phrase-type (s)))))


  (def-fcg-cxn feel-opine-cxn
               ((?feel-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame opine.01 ?o)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element topic ?o ?y-unit))))
                <-
                (?feel-unit
                 --
                 (lemma feel))))

    (def-fcg-cxn think-opine-cxn
               ((?think-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame opine.01 ?o)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element topic ?o ?y-unit))))
                <-
                (?think-unit
                 --
                 (lemma think))))

    (def-fcg-cxn think-opine-cxn
               ((?think-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame opine.01 ?o)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element topic ?o ?y-unit))))
                <-
                (?think-unit
                 --
                 (lemma think))))

    (def-fcg-cxn expect-expect-cxn
               ((?expect-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame expect.01 ?o)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element phenomenon ?o ?y-unit))))
                <-
                (?expect-unit
                 --
                 (lemma expect))))

    
  (def-fcg-cxn proper-noun-verb-compclause-cxn
               ((?verb-unit
                 (footprints (proper-noun-verb-compclause-cxn)))
                <-
                (?x-unit
                 --
                 (lex-class nnp)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s)))
                (?vp-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?verb-unit
                 --
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (parent ?vp-unit)
                 (footprints (NOT proper-noun-verb-compclause-cxn)))
                (?sbar-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))
                (?y-unit
                 --
                 (parent ?sbar-unit)
                 (phrase-type (s))))
               :disable-automatic-footprints t)




(make-propbank-english-cxns)

