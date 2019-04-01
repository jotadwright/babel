
;(ql:quickload :amr-grammar)

(in-package :amr-grammar)
(activate-monitor trace-fcg)

;; Example sentence: The boy wants to go

;; Grammar
 
(def-fcg-constructions amr-grammar

   (def-fcg-cxn boy-cxn
                ((?boy-unit
                  (referent ?b))
                 <-
                 (?boy-unit
                  (HASH meaning ((boy ?b)))
                  --
                  (HASH form ((string ?boy-unit "boy"))))))

   (def-fcg-cxn want-cxn
                ((?want-unit
                  (referent ?w)
                  (syn-cat (lex-class verb)
                           (finite +))
                  (sem-valence (arg0 ?arg0)
                               (arg1 ?arg1)))
                 <-
                 (?want-unit
                  (HASH meaning ((want-01 ?w)
                                 (:arg0 ?w ?arg0)
                                 (:arg1 ?w ?arg1)))
                  --
                  (HASH form ((string ?want-unit "wants"))))))

   (def-fcg-cxn go-cxn
                ((?go-unit
                  (referent ?g)
                  (syn-cat (lex-class verb)
                           (finite -))
                  (sem-valence (arg0 ?arg0)))
                 <-
                 (?go-unit
                  (HASH meaning ((go-01 ?g)
                                 (:arg0 ?g ?arg0)))
                  --
                  (HASH form ((string ?go-unit "go"))))))

   
   (def-fcg-cxn V-to-infinitive-cxn
                ((?finite-verb-unit
                  (subunits (?subject-unit ?main-verb-unit)))
                 (?main-verb-unit
                  (subunits (?to-unit)))
                 <-
                 (?subject-unit
                  --
                  (referent ?subject))
                 (?finite-verb-unit
                  ;;add more constraints (not all verbs can be followed by a To-inf)
                  --
                  (sem-valence (arg0 ?subject)
                               (arg1 ?main-event))
                  (syn-cat (lex-class verb)
                           (finite +)))
                 (?to-unit
                  --
                  (HASH form ((string ?to-unit "to")
                              (meets ?finite-verb-unit ?to-unit)
                              (meets ?to-unit ?main-verb-unit))))
                 (?main-verb-unit
                  --
                  (referent ?main-event)
                  (sem-valence (arg0 ?subject))
                  (syn-cat (lex-class verb)
                           (finite -))))))


(comprehend "the boy wants to go")
(pprint (predicates->penman (comprehend "the boy wants to go")))
