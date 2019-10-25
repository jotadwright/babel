
;; (ql:quickload :amr-grammar)

(in-package :amr-grammar)

(activate-monitor trace-fcg)

(def-fcg-constructions amr-grammar
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-semantic-network) 
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :re-enter-produced-utterance) 
                       
                       (:node-tests :check-duplicate)
                       
                       (:cxn-supplier-mode . :all-cxns-except-incompatible-hashed-cxns) ;;which cxns to try
                       (:queue-mode . :greedy-best-first) ;;search algorithm (:depth-first :breadth-first)
                       (:priority-mode . :priming) ;;how good is a node? (evaluation function)
                       )
  :visualization-configurations ((:with-search-debug-data . t))
  :cxn-inventory *amr-bidirectional*
  :hashed t
  
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
                          (number ?number))
                 (lex-id boy)))
               :attributes (:meaning boy :lex-id boy))

  (def-fcg-cxn boy-morph-cxn
               (<-
                (?boy-unit
                 (syn-cat (lemma boy)
                          (number sg))
                 (lex-id boy)
                 --
                 (HASH form ((string ?boy-unit "boy")))))
               :attributes (:string "boy" :lex-id boy))

  (def-fcg-cxn boys-morph-cxn
               (<-
                (?boys-unit
                 (syn-cat (lemma boy)
                          (number pl))
                 (lex-id boy)
                 --
                 (HASH form ((string ?boys-unit "boys")))))
               :attributes (:string "boys" :lex-id boy))

  ;; Articles
  (def-fcg-cxn the-cxn
               (<-
                (?the-unit
                 (syn-cat (lex-class article)
                          (definite +)
                          (number ?number)) ;;sg or pl
                 (lex-id the)
                 --
                 (HASH form ((string ?the-unit "the")))))
               :attributes (:string "the" :lex-id the))

  (def-fcg-cxn a-cxn
               (<-
                (?a-unit
                 (syn-cat (lex-class article)
                          (definite -)
                          (number sg))
                 (lex-id a)
                 --
                 (HASH form ((string ?a-unit "a")))))
               :attributes (:string "a" :lex-id a))

                       
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
                          (number ?number))
                 (lex-id want-01)))
               :attributes (:meaning want-01 :lex-id want-01))

  (def-fcg-cxn wants-morph-cxn
               (<-
                (?want-unit
                 (syn-cat (lemma want)
                          (number sg)
                          (finite +))
                 (lex-id want-01)
                 --
                 (HASH form ((string ?want-unit "wants")))))
               :attributes (:string "wants" :lex-id want-01))

  (def-fcg-cxn want-morph-cxn
               (<-
                (?want-unit
                 (syn-cat (lemma want)
                          (number pl)
                          (finite ?fin))
                 (lex-id want-01)
                 --
                 (HASH form ((string ?want-unit "want")))))
               :attributes (:string "want" :lex-id want-01))

                       
                       
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
                          (number ?nb))
                 (lex-id go-01)))
               :attributes (:meaning go-01 :lex-id go-01))

  (def-fcg-cxn go-morph-cxn
               (<-
                (?go-unit
                 (syn-cat (lemma go)
                          (finite -)
                          (number ?number))
                 (lex-id go-01)
                 --
                 (HASH form ((string ?go-unit "go")))))
               :attributes (:string "go" :lex-id go-01))


                       
                       
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
                          (number ?number))
                 (lex-id ?lex-id))
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
                 (syn-cat (phrase-type np)
                          (number ?nb))
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
                 (syn-cat (number ?nb)
                          (finite +))
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
                         (right ?inf-unit))))))


(formulate '((want-01 w)
             (boy b)
             (go-01 g)
             (:arg0 w b)
             (:arg1 w g)
             (:arg0 g b))  :cxn-inventory *amr-bidirectional*)



(comprehend "the boy wants to go" :cxn-inventory *amr-bidirectional*)

;;test priming
(loop for i from 0 to 10 
      do (comprehend "the boy wants to go" :cxn-inventory *amr-bidirectional*)
      (comprehend "the boys want to go" :cxn-inventory *amr-bidirectional*)
      (comprehend "boys want to go" :cxn-inventory *amr-bidirectional*)
      (comprehend "a boy wants to go" :cxn-inventory *amr-bidirectional*))





;;always the same utterance is produced (no exploration):
(loop for i from 0 to 10
      do (formulate-all '((want-01 w)
                      (boy b)
                      (go-01 g)
                      (:arg0 w b)
                      (:arg1 w g)
                      (:arg0 g b))  :cxn-inventory *amr-bidirectional*))

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
