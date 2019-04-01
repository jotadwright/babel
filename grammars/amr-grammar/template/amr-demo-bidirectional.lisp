
;; (ql:quickload :amr-grammar)

(in-package :amr-grammar)

(activate-monitor trace-fcg)

(def-fcg-constructions amr-grammar
  :fcg-configurations ((:max-nr-of-nodes . 2500)
                       (:production-goal-tests :connected-structure :no-applicable-cxns))
  :cxn-inventory *amr-bidirectional*
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Morphological and Lexical Constructions ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Nouns
                       (def-fcg-cxn boy-lex-cxn
                                    ((?boy-unit
                                      (referent ?b)
                                      (syn-cat (lex-class noun)))
                                     <-
                                     (?boy-unit
                                      (HASH meaning ((boy ?b)))
                                      --
                                      (syn-cat (lemma boy)
                                               (number ?number))))
                                    :cxn-set lex)

                       (def-fcg-cxn boy-morph-cxn
                                    (<-
                                     (?boy-unit
                                      (syn-cat (lemma boy)
                                               (number sg))
                                      --
                                      (HASH form ((string ?boy-unit "boy")))))
                                    :cxn-set morph)

                       (def-fcg-cxn boys-morph-cxn
                                    (<-
                                     (?boys-unit
                                      (syn-cat (lemma boy)
                                               (number pl))
                                      --
                                      (HASH form ((string ?boys-unit "boys")))))
                                    :cxn-set morph)

                       ;; Articles
                         (def-fcg-cxn the-cxn
                                    (<-
                                     (?the-unit
                                      (syn-cat (lex-class article)
                                                (definite +)
                                                (number ?number)) ;;sg or pl
                                      --
                                      (HASH form ((string ?the-unit "the")))))
                                    :cxn-set morph)

                       (def-fcg-cxn a-cxn
                                    (<-
                                     (?a-unit
                                      (syn-cat (lex-class article)
                                                (definite -)
                                                (number sg))
                                      --
                                      (HASH form ((string ?a-unit "a")))))
                                    :cxn-set morph)

                       
                       ;; Auxiliaries
                       (def-fcg-cxn want-lex-cxn
                                    ((?want-unit
                                      (referent ?w)
                                      (syn-cat (lex-class verb))
                                      (sem-valence (arg0 ?arg0)
                                                   (arg1 ?arg1)))
                                     <-
                                     (?want-unit
                                      (HASH meaning ((want-01 ?w)
                                                     (:arg0 ?w ?arg0)
                                                     (:arg1 ?w ?arg1)))
                                      --
                                      (syn-cat (lemma want)
                                               (finite ?fin)
                                               (number ?number))))
                                    :cxn-set lex)

                       (def-fcg-cxn wants-morph-cxn
                                    (<-
                                     (?want-unit
                                      (syn-cat (lemma want)
                                               (number sg)
                                               (finite +))
                                      --
                                      (HASH form ((string ?want-unit "wants")))))
                                    :cxn-set morph)

                       (def-fcg-cxn want-morph-cxn
                                    (<-
                                     (?want-unit
                                      (syn-cat (lemma want)
                                               (number pl)
                                               (finite ?fin))
                                      --
                                      (HASH form ((string ?want-unit "want")))))
                                    :cxn-set morph)

                       
                       
                       ;; Verbs
                       (def-fcg-cxn go-lex-cxn
                                    ((?go-unit
                                      (referent ?g)
                                      (syn-cat (lex-class verb))
                                      (sem-valence (arg0 ?arg0)))
                                     <-
                                     (?go-unit
                                      (HASH meaning ((go-01 ?g)
                                                     (:arg0 ?g ?arg0)))
                                      --
                                      (syn-cat (lemma go)
                                               (finite ?fin)
                                               (number ?nb))))
                                    :cxn-set lex)

                       (def-fcg-cxn go-morph-cxn
                                    (<-
                                     (?go-unit
                                      (syn-cat (lemma go)
                                               (finite -)
                                               (number ?number))
                                      --
                                      (HASH form ((string ?go-unit "go")))))
                                    :cxn-set morph)


                       
                       
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ;; Building Noun Phrases ;;
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;

                       ;; single noun
                       (def-fcg-cxn noun-nominal-cxn
                                    ((?nominal-unit
                                      (referent ?ref)
                                      (syn-cat (phrase-type nominal)
                                               (number ?number)
                                               (determination undetermined))
                                      (subunits (?noun-unit))
                                      (bounds (left ?noun-unit)
                                              (right ?noun-unit)))
                                     <-
                                     (?noun-unit
                                      (referent ?ref)
                                      (syn-cat (lex-class noun)
                                               (number ?number))
                                      --
                                      (syn-cat (lex-class noun)
                                               (number ?number)))))


                       ;; determining noun phrases
                       (def-fcg-cxn determined-np-cxn
                                    ((?art-unit
                                      (part-of-np +))
                                     (?determined-np-unit
                                      (referent ?ref)
                                      (syn-cat (phrase-type np)
                                               (number ?number)
                                               (determination determined)
                                               (definite ?definite))
                                      (subunits (?art-unit ?nominal-unit))
                                      (bounds (left ?art-unit)
                                              (right ?np-right)))
                                     (?nominal-unit
                                      (footprints (determination-cxn)))
                                     
                                     <-
                                     (?art-unit
                                      --
                                      (syn-cat (lex-class article)
                                               (definite ?definite)
                                               (number ?number)))
                                     (?nominal-unit
                                      (referent ?ref)
                                      (syn-cat (phrase-type nominal)
                                               (number ?number)
                                               (determination undetermined))
                                      (bounds (left ?np-left)
                                              (right ?np-right))
                                      (footprints (not determination-cxn))
                                      --
                                      (footprints (not determination-cxn))
                                      (syn-cat (phrase-type nominal)
                                               (number ?number)
                                               (determination undetermined))
                                      (bounds (left ?np-left)
                                              (right ?np-right)))
                                     (?determined-np-unit
                                      --
                                      
                                      (HASH form ((meets ?art-unit ?np-left))))))

                       (def-fcg-cxn generic-np-cxn
                                    ((?generic-np-unit
                                      (referent ?ref)
                                      (syn-cat
                                       (phrase-type np)
                                       (number pl)
                                       (determination generic)
                                       (definite -))
                                      (subunits (?nominal-unit))
                                      (bounds (left ?np-left)
                                              (right ?np-right)))
                                     (?nominal-unit
                                      (footprints (determination-cxn)))
                                     <-
                                     (?nominal-unit
                                      (referent ?ref)
                                      (syn-cat (phrase-type nominal)
                                               (number pl)
                                               (determination undetermined))
                                      (bounds (left ?np-left)
                                              (right ?np-right))
                                      (footprints (not determination-cxn))
                                      --
                                      (footprints (not determination-cxn))
                                      (syn-cat (phrase-type nominal)
                                               (number pl)
                                               (determination undetermined))
                                      (bounds (left ?np-left)
                                              (right ?np-right)))))

                       (def-fcg-cxn to-infinitive-cxn
                                   ((?infinitival-clause-unit
                                     (referent ?inf-ref)
                                     (sem-valence ?sem-val)
                                     (subunits (?to-unit ?infinitive-unit))
                                     (syn-cat (phrase-type infinitival-clause)
                                              (finite -))
                                     (bounds (left ?to-unit)
                                             (right ?infinitive-unit)))
                                    <-
                                    (?to-unit
                                     --
                                     (HASH form ((string ?to-unit "to"))))
                                    (?infinitive-unit
                                     (referent ?inf-ref)
                                     (sem-valence ?sem-val)
                                     (syn-cat (lex-class verb)
                                              (finite -))
                                     --
                                     (syn-cat (lex-class verb)
                                              (finite -))
                                     (HASH form ((meets ?to-unit ?infinitive-unit))))))

                       (def-fcg-cxn infinitival-object-cxn
                                    ((?clausal-unit
                                      (subunits (?subject-unit ?verb-unit ?infinitival-clause-unit))
                                      (referent ?ev)
                                      (bounds (left ?left-subj)
                                              (right ?inf-unit)))
                                     <-
                                     (?clausal-unit
                                      --
                                      (HASH form ((meets ?right-subj ?verb-unit)
                                                  (meets ?verb-unit ?to-unit))))
                                     (?subject-unit
                                      (syn-cat (phrase-type np))
                                      (referent ?subj)
                                      --
                                      (syn-cat (phrase-type np)
                                               (number ?nb))
                                      (bounds (left ?left-subj)
                                              (right ?right-subj)))
                                     (?verb-unit
                                      (referent ?ev)
                                      (sem-valence (arg0 ?subj)
                                                   (arg1 ?inf))
                                      --
                                      (syn-cat (lex-class verb)
                                               (number ?nb)
                                               (finite +)))
                                     (?infinitival-clause-unit
                                      (referent ?inf)
                                      (sem-valence (arg0 ?subj))
                                      (syn-cat (phrase-type infinitival-clause))
                                      --
                                      (bounds (left ?to-unit)
                                              (right ?inf-unit)))))
                       )


(comprehend-and-formulate "the boy wants to go" :cxn-inventory *amr-bidirectional*)

(formulate-all '((want-01 w)
                 (boy b)
                 (go-01 g)
                 (:arg0 w b)
                 (:arg1 w g)
                 (:arg0 g b))  :cxn-inventory *amr-bidirectional*)

(pprint (amr:predicates->penman '((want-01 w)
                                  (boy b)
                                  (go-01 g)
                                  (:arg0 w b)
                                  (:arg1 w g)
                                  (:arg0 g b))))


(amr:penman->predicates (amr:penman->predicates '(w / want-01
                                                     :arg0 (b / boy)
                                                     :arg1 (g / go-01
                                                              :arg0 b))))
