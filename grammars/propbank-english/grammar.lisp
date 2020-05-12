(in-package :propbank-english)


(def-fcg-constructions propbank-english
  :fcg-configurations ((:de-render-mode .  :de-render-constituents-dependents))
  :hierarchy-features (constituents dependents)
  :feature-types ((constituents set)
                  (dependents set)
                  (span sequence)
                  (phrase-type set)
                  (word-order set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))

  (def-fcg-cxn proper-noun-feel-y-cxn
               ((?feel-unit
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
                (?s-bar-unit
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
                           (frame-evoking-element ?o ?frame-evoking-element)
                           (frame-element cognizer ?o ?x-unit)
                           (frame-element topic ?o ?y-unit))))
                <-
                (?think-unit
                 --
                 (lemma think)
                 (string ?frame-evoking-element))))

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
                (?s-bar-unit
                 --
                 (parent ?vp-unit)
                 (phrase-type (sbar)))
                (?y-unit
                 --
                 (parent ?sbar-unit)
                 (phrase-type (s))))
               :disable-automatic-footprints t))


;; (comprehend-all "Katrien felt that Paul was right.")

;; (comprehend "Remi expected that Katrien thinks that Paul was right.")


;; (comprehend "Katrien thinks that Paul was right.")
;; (comprehend "The apple was eaten by the man.")
;; (comprehend "The man was eating an apple.")
