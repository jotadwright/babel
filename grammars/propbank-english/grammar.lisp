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


;;;; Exploratory hand-written grammar

;;;arg-m-cxns

;;arg-m-neg-cxns

    (def-fcg-cxn believe.01-arg-m-neg-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:arg-m-neg ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-neg ?o ?rb-unit))))
                 <-
                 (?rb-unit
                 --
                 (parent ?vp-unit2)
                 (lex-class rb)
                 (dependency-label neg))                 
                (?vp-unit2
                 --
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                (parent ?vp-unit2))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))


        (def-fcg-cxn believe.01-pass-arg-m-neg-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:arg-m-neg ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-neg ?o ?rb-unit))))
                 <-
                 (?rb-unit
                 --
                 (parent ?vp-unit3)
                 (lex-class rb)
                 (dependency-label neg))                 
                (?vp-unit3
                 --
                 (phrase-type (vp)))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                (parent ?vp-unit3))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))


   
;;arg-m-adv-cxns

(def-fcg-cxn believe.01-arg-m-adv-cxn
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


    (def-fcg-cxn believe.01-aux-arg-m-adv-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:arg-m-adv ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element arg-m-adv ?o ?rb-unit))))
                 <-
                 (?rb-unit
                 --
                 (parent ?vp-unit2)
                 (lex-class rb)
                 (dependency-label advmod))                 
                (?vp-unit2
                 --
                 (phrase-type (vp)))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                (parent ?vp-unit2))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))

;;arg-m-mod-cxns

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


        (def-fcg-cxn believe.01-aux-pass-arg-m-mod-cxn
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
                 (?vp-unit3
                  --
                  (phrase-type (vp))
                  (parent ?s-unit))
                 (?md-unit
                 --
                 (parent ?vp-unit3)
                 (lex-class md))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit3))
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


        (def-fcg-cxn believe.01-nns-aux-sentential-complement-cxn
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
                 (lex-class nns)) 
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
       
(def-fcg-cxn believe.01-nnps-aux-sentential-complement-cxn
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
                 (lex-class nnps)) 
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


(def-fcg-cxn believe.01-nnps-aux-sentential-complement-cxn
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
                 (lex-class nnps)) 
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


    (def-fcg-cxn believe.01-nns-sentential-complement-cxn
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
                 (lex-class nns))
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

       (def-fcg-cxn believe.01-nnps-sentential-complement-cxn
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
                 (lex-class nnps))
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
                 (?nsubj-unit
                  --
                  (parent ?x-unit)
                  (dependency-label nsubj))
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
                 (phrase-type (pp)))))



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
                 (lex-class prp)
                 (dependency-label nsubj)) ;distinguish from believe-01-prp-passive-cxn
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
                 (phrase-type (pp)))))


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
                 (lex-class nn)
                 (dependency-label nsubj))
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
                 (phrase-type (pp)))))



      (def-fcg-cxn believe.01-nns-aux-prepositional-complement-cxn
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
                 (lex-class nns)
                 (dependency-label nsubj))
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
                 (phrase-type (pp)))))


      
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
                 (lex-class nnp)
                 (dependency-label nsubj))
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
                 (phrase-type (pp)))))


      (def-fcg-cxn believe.01-nnps-aux-prepositional-complement-cxn
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
                 (lex-class nnps)
                 (dependency-label nsubj))
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
                 (phrase-type (pp)))))
      

;;without auxiliaries

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
                 (phrase-type (pp)))))


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
                 (phrase-type (pp)))))
   

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
                 (phrase-type (pp)))))

   
      (def-fcg-cxn believe.01-nns-prepositional-complement-cxn
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
                 (lex-class nns))
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
                 (phrase-type (pp)))))



      (def-fcg-cxn believe.01-nnps-prepositional-complement-cxn
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
                 (lex-class nnps))
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
                 (phrase-type (pp)))))
      
      
     
;;; nominal complement cxns
;; with auxiliaries

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
        
  
        (def-fcg-cxn believe.01-nns-aux-nominal-complement-cxn
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
                 (lex-class nns))
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



;; without auxiliaries

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

   (def-fcg-cxn believe.01-nns-nominal-complement-cxn
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
                 (lex-class nns))
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


      (def-fcg-cxn believe.01-nnps-nominal-complement-cxn
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
                 (lex-class nnps))
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

;;nominal complement dobj

(def-fcg-cxn believe.01-np-nominal-complement-dobj-cxn
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
                (dependency-label dobj))))


   
    (def-fcg-cxn believe.01-prp-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))


    (def-fcg-cxn believe.01-nn-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))

   (def-fcg-cxn believe.01-nns-nominal-complement-dobj-cxn
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
                 (lex-class nns))
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
                 (dependency-label dobj))))

   (def-fcg-cxn believe.01-nnp-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))


      (def-fcg-cxn believe.01-nnps-nominal-complement-dobj-cxn
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
                 (lex-class nnps))
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
                 (dependency-label dobj))))

   
;;; sentential complement passive extraction cxns

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

  

    (def-fcg-cxn believe.01-nns-sentential-complement-pass-extraction-cxn
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
                 (lex-class nns))
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


      (def-fcg-cxn believe.01-nnps-sentential-complement-pass-extraction-cxn
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
                 (lex-class nnps))
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
;; clausal-complement-pp cxns
;without auxiliaries

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



        (def-fcg-cxn believe.01-nn-clausal-complement-pp-cxn
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
                (phrase-type (pp)))))


    
    (def-fcg-cxn believe.01-nns-clausal-complement-pp-cxn
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
                 (lex-class nns)
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
        

        (def-fcg-cxn believe.01-nnps-clausal-complement-pp-cxn
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
                 (lex-class nnps)
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


;with auxiliaries





        

;;clausal-complement-np-cxns

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


  (def-fcg-cxn believe.01-nns-clausal-complement-np-cxn
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
                 (lex-class nns)
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


        (def-fcg-cxn believe.01-nnps-clausal-complement-np-cxn
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
                 (lex-class nnps)
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
        



(def-fcg-cxn believe.01-np-clausal-complement-dobj-cxn 
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
                (dependency-label dobj))))


    (def-fcg-cxn believe.01-prp-clausal-complement-dobj-cxn 
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
                (dependency-label dobj))))

    (def-fcg-cxn believe.01-nn-clausal-complement-dobj-cxn 
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
                (dependency-label dobj))))

        (def-fcg-cxn believe.01-nns-clausal-complement-dobj-cxn 
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
                 (lex-class nns)
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
                (dependency-label dobj))))
        

        (def-fcg-cxn believe.01-nnp-clausal-complement-dobj-cxn 
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
                (dependency-label dobj))))

        (def-fcg-cxn believe.01-nnps-clausal-complement-dobj-cxn 
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
                 (lex-class nnps)
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
                (dependency-label dobj))))


 
    
;;clausal-complement-sbar
;without auxiliaries

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


    (def-fcg-cxn believe.01-nns-clausal-complement-sbar-cxn
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
                 (lex-class nns)
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

       (def-fcg-cxn believe.01-nnp-clausal-complement-sbar-cxn
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
                (phrase-type (sbar)))))

       (def-fcg-cxn believe.01-nnps-clausal-complement-sbar-cxn
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
                 (lex-class nnps)
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
       
;with auxiliaries

  (def-fcg-cxn believe.01-prp-aux-clausal-complement-sbar-cxn
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
                (?vp-unit3
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit3))
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
      
       
;;;adjectival complement cxns
;; adjectival-complement-pp cxns

    (def-fcg-cxn believe.01-np-adj-complement-pp-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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


     (def-fcg-cxn believe.01-prp-adj-complement-pp-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

         (def-fcg-cxn believe.01-nn-adj-complement-pp-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

         (def-fcg-cxn believe.01-nns-adj-complement-pp-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

         (def-fcg-cxn believe.01-nnp-adj-complement-pp-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

         (def-fcg-cxn believe.01-nnps-adj-complement-pp-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

         
;;adjectival-complement-sbar-cxns

    (def-fcg-cxn believe.01-np-adj-complement-sbar-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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


        (def-fcg-cxn believe.01-prp-adj-complement-sbar-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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


        (def-fcg-cxn believe.01-nn-adj-complement-sbar-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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


        (def-fcg-cxn believe.01-nns-adj-complement-sbar-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

        (def-fcg-cxn believe.01-nnp-adj-complement-sbar-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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

        (def-fcg-cxn believe.01-nnps-adj-complement-sbar-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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


;adjectival-complement-general/dobj

 (def-fcg-cxn believe.01-np-adj-complement-dobj-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))



        (def-fcg-cxn believe.01-prp-adj-complement-dobj-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))


        (def-fcg-cxn believe.01-nn-adj-complement-dobj-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))

      (def-fcg-cxn believe.01-nns-adj-complement-dobj-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))


      (def-fcg-cxn believe.01-nnp-adj-complement-dobj-cxn
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
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))

 (def-fcg-cxn believe.01-nnps-adj-complement-dobj-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?adjp-unit1
                 --
                 (phrase-type (adjp))
                 (parent ?vp-unit2))
                (?s-vp-unit
                 --
                 (parent ?adjp-unit1)
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
                (dependency-label dobj))))


;;;verbal-complement -cxns
;;verbal-complement-sbar-cxns
    (def-fcg-cxn believe.01-np-verb-complement-sbar-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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


 (def-fcg-cxn believe.01-prp-verb-complement-sbar-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

 (def-fcg-cxn believe.01-nn-verb-complement-sbar-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

  (def-fcg-cxn believe.01-nns-verb-complement-sbar-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

    (def-fcg-cxn believe.01-nnp-verb-complement-sbar-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

        (def-fcg-cxn believe.01-nnps-verb-complement-sbar-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
 
    
;;verbal-complement-pp-cxns

  (def-fcg-cxn believe.01-np-verb-complement-pp-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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


 (def-fcg-cxn believe.01-prp-verb-complement-pp-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

 (def-fcg-cxn believe.01-nn-verb-complement-pp-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
 

  (def-fcg-cxn believe.01-nns-verb-complement-pp-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

    (def-fcg-cxn believe.01-nnp-verb-complement-pp-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

        (def-fcg-cxn believe.01-nnps-verb-complement-pp-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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

;verbal complement dobj cxns

  (def-fcg-cxn believe.01-np-verb-complement-dobj-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))


  (def-fcg-cxn believe.01-prp-verb-complement-dobj-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))


    (def-fcg-cxn believe.01-nn-verb-complement-dobj-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))

       (def-fcg-cxn believe.01-nns-verb-complement-dobj-cxn
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
                 (lex-class nns)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))

       (def-fcg-cxn believe.01-nnp-verb-complement-dobj-cxn
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
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))
  

       (def-fcg-cxn believe.01-nnps-verb-complement-dobj-cxn
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
                 (lex-class nnps)
                 (parent ?s-unit))
                (?s-unit
                 --
                 (phrase-type (s))
                )
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?s-vp-unit
                 --
                 (phrase-type (s vp))
                 (parent ?vp-unit2))
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
                (dependency-label dobj))))

      
;;; passive constructions
;; passive subject extraposition construction
   
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



  (def-fcg-cxn believe.01-aux-pass-subj-extraposition-cxn
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
                (?vp-unit3
                 --
                 (parent ?s-unit)
                 (phrase-type (vp)))
                (?vp-unit2
                 --
                 (parent ?vp-unit3)
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
  


;;general passive constructions
; without auxiliaries

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
 

  (def-fcg-cxn believe.01-prp-passive-cxn;
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

  (def-fcg-cxn believe.01-nns-passive-cxn 
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
                 (lex-class nns))
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

      (def-fcg-cxn believe.01-nnps-passive-cxn 
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
                 (lex-class nnps))
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

;passives with auxiliaries

  (def-fcg-cxn believe.01-prp-aux-passive-cxn;
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
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))

    (def-fcg-cxn believe.01-np-aux-passive-cxn;
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
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))

    
  (def-fcg-cxn believe.01-nn-aux-passive-cxn;
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
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))
  
  (def-fcg-cxn believe.01-nns-aux-passive-cxn;
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
                 (lex-class nns))
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))    

  (def-fcg-cxn believe.01-nnp-aux-passive-cxn;
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
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))    

  (def-fcg-cxn believe.01-nnps-aux-passive-cxn;
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
                 (lex-class nnps))
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))

  (def-fcg-cxn believe.01-dt-aux-passive-cxn;
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
                (?vp-unit2
                 --
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit1)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))))   

  
;;passive-pp-cxns e.g. "He is believed by them."
; without auxiliaries 
  
      (def-fcg-cxn believe.01-prp-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))


      (def-fcg-cxn believe.01-np-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (phrase-type (np)))
                (?nn-unit
                 --
                 (parent ?y-unit)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))


           (def-fcg-cxn believe.01-nn-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nn)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))
                       

            (def-fcg-cxn believe.01-nns-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nns)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))
            
          (def-fcg-cxn believe.01-nnp-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnp)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))

          (def-fcg-cxn believe.01-nnps-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class nnps)
                 (dependency-label nsubjpass))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))

;with auxiliaries

      (def-fcg-cxn believe.01-prp-aux-passive-pp-cxn
                 ((?believe-unit
                 (args (referent ?o)
                       (:arg0 ?x-unit)
                       (:arg1 ?y-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element believer ?o ?x-unit)
                           (frame-element believed ?o ?y-unit))))
                <-
                (?s-unit
                 --
                 (phrase-type (s)))
                (?y-unit
                 --
                 (parent ?s-unit)
                 (lex-class prp)
                 (dependency-label nsubjpass))
                (?vp-unit3
                 --
                 (phrase-type (vp))
                 (parent ?s-unit))
                (?vp-unit2
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit3))
                (?vp-unit1
                 --
                 (phrase-type (vp))
                 (parent ?vp-unit2))
                (?auxpass-unit
                 --
                 (parent ?vp-unit2)
                 (dependency-label auxpass))
                (?believe-unit
                 --
                 (lemma believe)
                 (parent ?vp-unit1))
               (?pp-unit
                --
                (phrase-type (pp))
                (parent ?vp-unit1))
               (?x-unit
                --
                (parent ?pp-unit)
                (dependency-label pobj))))

)           
                        

#|

;;;; To do:

3 levels of VPs?  
generalize cxns for test data
Frame-element-rel
Other arg-m cxns (temp etc.)


;;;; Initial examples

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


 |# 

(make-propbank-english-cxns)

