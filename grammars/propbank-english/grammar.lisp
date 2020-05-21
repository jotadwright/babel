(in-package :propbank-english)


(def-fcg-constructions propbank-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents)
                       (:node-tests :restrict-nr-of-nodes :restrict-search-depth))
  :visualization-configurations ((:show-constructional-dependencies . nil))
 
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))

;;;; Generalized grammar

;;; arg0-cxns

;;active, main clause

(def-fcg-cxn believe.01-root-arg0-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                     (:arg0 ?arg0-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg0 ?o ?arg0-unit))))
              <-
              (?arg0-unit
              --
              (dependency-label nsubj)
              (head ?frame-unit)
              (parent ?s-unit))
              (?s-unit
               --
               (phrase-type (s)))
              (?frame-unit
               --
               (dependency-label root)
               (lemma believe))))

(def-fcg-cxn believe.01-root-arg0-np-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                      (:arg0 ?arg0-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg0 ?o ?arg0-unit))))
              <-
              (?arg0-unit
              --
              (phrase-type (np)))
              (?nsubj-unit
               --
               (parent ?arg0-unit)
              (dependency-label nsubj)
              (head ?frame-unit))
              (?frame-unit
               --
               (dependency-label root)
               (lemma believe))))

;;active, subordinate clause

(def-fcg-cxn believe.01-advcl-arg0-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                      (:arg0 ?arg0-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg0 ?o ?arg0-unit))))
              <-
              (?arg0-unit
              --
              (dependency-label nsubj)
              (head ?frame-unit)
              (parent ?s-unit))
              (?s-unit
               --
               (phrase-type (s)))
              (?frame-unit
               --
               (dependency-label advcl)
               (lemma believe))))

(def-fcg-cxn believe.01-advcl-arg0-np-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                      (:arg0 ?arg0-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg0 ?o ?arg0-unit))))
              <-
              (?arg0-unit
              --
              (phrase-type (np)))
              (?nsubj-unit
               --
               (parent ?arg0-unit)
              (dependency-label nsubj)
              (head ?frame-unit))
              (?frame-unit
               --
               (dependency-label advcl)
               (lemma believe))))

;;active, questions
(def-fcg-cxn believe.01-root-arg0-sq-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                      (:arg0 ?arg0-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg0 ?o ?arg0-unit))))
              <-
              (?arg0-unit
              --
              (dependency-label nsubj)
              (head ?frame-unit)
              (parent ?s-unit))
              (?s-unit
               --
               (phrase-type (sq)))
              (?frame-unit
               --
               (dependency-label root)
               (lemma believe))))



;;; arg-1 cxns
;; active, main clause

(def-fcg-cxn believe.01-root-arg1-cxn
             ((?frame-unit
               (args (referent ?o)                                               
                     (:v ?frame-unit)
                     (:arg1 ?arg1-unit))                                              
               (frame-evoking +)
               (meaning ((frame believe.01 ?o)
                         (frame-element v ?o ?frame-unit)
                         (frame-element arg1 ?o ?arg1-unit))))
              <-
              (?vp-unit
               --
               (phrase-type (vp)))
              (?arg1-unit
               --
               (parent ?vp-unit))
              (?frame-unit
               --
               (dependency-label root)
               (lemma believe)
              (parent ?vp-unit))))





;;; argm cxns

  
  
  )



#|
;;;; Exploratory hand-written grammar

;;;argm-cxns

;;argm-tmp-cxns

    (def-fcg-cxn believe.01-argm-tmp-date-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-tmp ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-tmp ?o ?tmp-unit))))
                 <-
                 (?believe-unit
                  --
                  (lemma believe))
                  (?tmp-unit
                   --
                   ())
                   (?ner-unit
                   --
                   (named-entity-type date)
                   (parent ?tmp-unit))))
    

    (def-fcg-cxn believe.01-argm-tmp-time-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-tmp ?tmp-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-tmp ?o ?tmp-unit))))
                 <-
                 (?believe-unit
                  --
                  (lemma believe))
                 (?tmp-unit
                  --
                  ())
                 (?ner-unit
                  --
                  (named-entity-type time)
                  (parent ?tmp-unit))))

;;argm-loc-cxns

   (def-fcg-cxn believe.01-argm-loc-gpe-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-loc ?loc-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-loc ?o ?loc-unit))))
                 <-
                 (?believe-unit
                  --
                  (lemma believe))
                 (?loc-unit
                  --
                  ())
                 (?ner-unit
                  --
                  (named-entity-type gpe)
                  (parent ?loc-unit))))

      (def-fcg-cxn believe.01-argm-loc-fac-cxn ;optional, fac is for facilities
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-loc ?loc-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-loc ?o ?loc-unit))))
                 <-
                 (?believe-unit
                  --
                  (lemma believe))
                 (?loc-unit
                  --
                  ())
                 (?ner-unit
                  --
                  (named-entity-type fac)
                  (parent ?loc-unit))))



;;argm-neg-cxns

    (def-fcg-cxn believe.01-argm-neg-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-neg ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-neg ?o ?rb-unit))))
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


        (def-fcg-cxn believe.01-pass-argm-neg-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-neg ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-neg ?o ?rb-unit))))
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

(def-fcg-cxn believe.01-argm-adv-cxn
               ((?believe-unit
                 (args (referent ?o) 
                  (:argm-adv ?beta-unit))
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-adv ?o ?rb-unit))))
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


    (def-fcg-cxn believe.01-aux-argm-adv-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-adv ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-adv ?o ?rb-unit))))
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

;;argm-mod-cxns

        (def-fcg-cxn believe.01-aux-argm-mod-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-mod ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-mod ?o ?md-unit))))
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


        (def-fcg-cxn believe.01-aux-pass-argm-mod-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-mod ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-mod ?o ?md-unit))))
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


        (def-fcg-cxn believe.01-sq-argm-mod-cxn
               ((?believe-unit
                  (args (referent ?o)                                               
                        (:argm-mod ?rb-unit))                                              
                 (frame-evoking +)
                 (meaning ((frame believe.01 ?o)
                           (frame-element argm-mod ?o ?md-unit))))
                 <-
                 (?s-unit
                 --
                 (phrase-type (sq)))
                 (?md-unit
                 --
                 (parent ?s-unit)
                 (lex-class md))
                 (?vp-unit1
                  --
                  (phrase-type (vp))
                  (parent ?s-unit))
                (?believe-unit
                 --
                 (lemma believe)
                (parent ?vp-unit1))))


;;argm-cau cxns
 ;pp's starting with 'because', 'cause', etc.


;;arg-m-ext cxns
 ;advs such as 'completely' etc.

;;argm-dis
 ;discourse markers ('however', 'well', 'of course', 'and ...', )
 ;dependency-label intj 

        
        

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


;; nominal complement dobj

 (def-fcg-cxn believe.01-np-aux-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))



    (def-fcg-cxn believe.01-prp-aux-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))


        (def-fcg-cxn believe.01-nn-aux-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))
        
  
        (def-fcg-cxn believe.01-nns-aux-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))
        

        (def-fcg-cxn believe.01-nnp-aux-nominal-complement-dobj-cxn
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
                 (dependency-label dobj))))






        

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


        (def-fcg-cxn believe.01-prp-sq-nominal-complement-dobj-cxn
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
                 (phrase-type (sq)))
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
                        

;;;; To do:

3 levels of VPs (e.g. should have been ...)
Questions; phrase-type  sq instead of s

Make arg0 dependency label nsubj explicit (to avoid confusion with a.o. arg-m-tmp)


Add frame-element rel
Check argm-adv vs. mnr
Refine argm-time, date, etc.
Check possibilities of NER for location, etc.
Explore ways of generalizing NER prps etc.
 

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

