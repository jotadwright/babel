;;(ql:quickload :fcg)
;;(ql:quickload :amr)
;;(ql:quickload :irl)
(in-package :fcg)


(def-fcg-constructions  little-prince-grammar-sequences
  ;; These are the feature-types that we declare. All other features are treated as feature-value pairs.
  :feature-types ((meaning set-of-predicates)
                  (form set-of-predicates :handle-regex-sequences)
                  (subunits set)
                  (lex-class set)
                  (footprints set)
                  (meaning-args sequence)
                  (form-args sequence))
  ;; We specify the goal tests here.
  :fcg-configurations ((:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure )
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network :connected-structure )
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) ;; returns all cxns at once
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first :random
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic) - only used with best-first search
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                       (:de-render-mode . :de-render-sequence-predicates)
                       (:render-mode . :render-sequences)
                       (:max-nr-of-nodes . 1000))

  (def-fcg-cxn fox-cxn
               ((?fox-unit
                 (meaning-args (?f))
                 (form-args (?left ?right))
                 (lex-class (noun))
                 (sem-class physical-entity))
                <-
                (?fox-unit
                 (HASH meaning ((fox ?f)))
                 --
                 (HASH form ((sequence "fox" ?left ?right))))))

  (def-fcg-cxn prince-cxn
               ((?prince-unit
                 (meaning-args (?p))
                 (form-args (?left ?right))
                 (lex-class (noun))
                 (sem-class physical-entity))
                <-
                (?prince-unit
                 (HASH meaning ((prince ?p)))
                 --
                 (HASH form ((sequence "prince" ?left ?right))))))

  (def-fcg-cxn he-cxn
               (<-
                (?he-unit
                 (meaning-args (?h))
                 (form-args (?left ?right))
                 (lex-class (pronoun))
                 (phrase-type noun-phrase)
                 (sem-class physical-entity)
                 --
                 (HASH form ((sequence "he" ?left ?right))))))

  (def-fcg-cxn nothing-cxn
               ((?nothing-unit
                 (meaning-args (?n))
                 (form-args (?left ?right))
                 (lex-class (pronoun))
                 (phrase-type noun-phrase)
                 (sem-class physical-entity))
                <-
                (?nothing-unit
                 (HASH meaning ((nothing ?n)))
                 --
                 (HASH form ((sequence "nothing" ?left ?right))))))

  (def-fcg-cxn little-cxn
               ((?little-unit
                 (meaning-args (?l))
                 (form-args (?left ?right))
                 (lex-class (adjective))
                 (sem-class property))
                <-
                (?little-unit
                 (HASH meaning ((little ?l)))
                 --
                 (HASH form ((sequence "little" ?left ?right))))))
  
  (def-fcg-cxn noun-adjective-cxn
               ((?nominal-unit
                 (subunits (?noun-unit ?adjective-unit))
                 (meaning-args (?n))
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (form-args (?left-adj ?right-noun)))
                (?noun-unit
                 (footprints (nominal-cxn)))
                <-
                (?adjective-unit
                 (meaning-args (?a))
                 (lex-class (adjective))
                 --
                 (form-args (?left-adj ?right-adj))
                 (lex-class (adjective)))
                (?noun-unit
                 (meaning-args (?n))
                 (lex-class (noun))
                 (footprints (not nominal-cxn))
                 --
                 (footprints (not nominal-cxn))
                 (form-args (?left-noun ?right-noun))
                 (lex-class (noun)))
                (?nominal-unit
                 (HASH meaning ((:mod ?n ?a)))
                 --
                 (HASH form ((sequence " " ?right-adj ?left-noun)))))
               :disable-automatic-footprints t)
  
  (def-fcg-cxn noun-nominal-cxn
               ((?nominal-unit
                 (subunits (?noun-unit))
                 (meaning-args (?ref))
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (form-args (?left ?right)))
                (?noun-unit
                 (footprints (nominal-cxn)))
                <-
                (?noun-unit
                 (meaning-args (?ref))
                 (lex-class (noun))
                 (sem-class ?sem-class)
                 (footprints (not nominal-cxn))
                 --
                 (footprints (not nominal-cxn))
                 (form-args (?left ?right))
                 (lex-class (noun))))
               :disable-automatic-footprints t)
                 
  (def-fcg-cxn the-nominal-cxn
               ((?noun-phrase-unit
                 (meaning-args (?ref))
                 (form-args (?left-the ?right-nominal))
                 (phrase-type noun-phrase)
                 (sem-class ?sem-class)
                 (sem-function referring-expression)
                 (subunits (?the-unit ?nominal-unit)))
                <-
                (?the-unit
                 --
                 (HASH form ((sequence "the " ?left-the ?right-the))))
                (?nominal-unit
                 (meaning-args (?ref))
                 (sem-class ?sem-class)
                 (syn-function nominal)
                 --
                 (form-args (?right-the ?right-nominal))
                 (syn-function nominal))))

  (def-fcg-cxn appear-lex-cxn
               ((?appear-unit
                 (meaning-args (?a))
                 (lex-class (ergative-verb verb))
                 (sem-class activity))
                <-
                (?appear-unit
                 (HASH meaning ((appear-01 ?a)))
                 --
                 (lemma appear)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t)
                 (form-args ?left ?right))))

  (def-fcg-cxn appear-appeared-morph
           (
            <-
            (?appeared-unit
             (lemma appear)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             (form-args (?left ?right))
             --
             (HASH form ((sequence "appeared" ?left ?right))))))

  (def-fcg-cxn turn-lex-cxn
               ((?turn-unit
                 (meaning-args (?t))
                 (lex-class (ergative-verb verb))
                 (sem-class activity)
                 (sem-frame motion))
                <-
                (?turn-unit
                 (HASH meaning ((turn-01 ?t)))
                 --
                 (lemma turn)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?tense)
                 (form-args (?left ?right)))))

  (def-fcg-cxn turn-turned-morph
           (<-
            (?turned-unit
             (lemma turn)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             (form-args (?left ?right))
             --
             (HASH form ((sequence "turned" ?left ?right))))))

    (def-fcg-cxn say-lex-cxn
               ((?say-unit
                 (meaning-args (?s))
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame statement))
                <-
                (?say-unit
                 (HASH meaning ((say-01 ?s)))
                 --
                 (lemma say)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t)
                 (form-args (?left ?right)))))

  (def-fcg-cxn say-said-morph
           (<-
            (?said-unit
             (lemma say)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             (form-args (?left ?right))
             --
             (HASH form ((sequence "said" ?left ?right))))))

  (def-fcg-cxn respond-lex-cxn
               ((?respond-unit
                 (meaning-args (?r))
                 (lex-class (verb))
                 (sem-class activity))
                <-
                (?respond-unit
                 (HASH meaning ((respond-01 ?r)))
                 --
                 (lemma respond)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t)
                 (form-args (?left ?right)))))

  (def-fcg-cxn respond-responded-morph
           (<-
            (?responded-unit
             (lemma respond)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             (form-args (?left ?right))
             --
             (HASH form ((sequence "responded" ?left ?right))))))

  (def-fcg-cxn see-lex-cxn
               ((?see-unit
                 (meaning-args (?s))
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame perception))
                <-
                (?see-unit
                 (HASH meaning ((see-01 ?s)))
                 --
                 (lemma see)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t)
                 (form-args (?left ?right)))))

  (def-fcg-cxn see-saw-morph
           (<-
            (?saw-unit
             (lemma see)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             (form-args (?left ?right))
             --
             (HASH form ((sequence "saw" ?left ?right))))))
  

  (def-fcg-cxn intransitive-ergative-cxn
               ((?ergative-clause-unit
                 (subunits (?arg1-unit ?ergative-verb-unit))
                 (form-args (?arg1-left ?verb-right))
                 (syn-valence (subject ?arg1-unit))
                 (phrase-type clause)
                 (meaning-args (?event)))
                (?ergative-verb-unit
                 (footprints (argument-structure-cxn)))
                <-
                (?arg1-unit
                 (meaning-args (?arg1))
                 (sem-function referring-expression)
                 --
                 (form-args (?arg1-left ?arg1-right))
                 (sem-function referring-expression))
                (?ergative-verb-unit
                 (meaning-args (?event))
                 (lex-class (ergative-verb))
                 (footprints (not argument-structure-cxn))
                 --
                 (footprints (not argument-structure-cxn))
                 (form-args (?verb-left ?verb-right))
                 (lex-class (ergative-verb)))
                (?ergative-clause-unit
                 (HASH meaning ((:arg1 ?event ?arg1)))
                 --
                 (HASH form ((sequence " " ?arg1-right ?verb-left)))))
               :disable-automatic-footprints t)

  (def-fcg-cxn it-was-then-that-X-cxn
               ((?clause-unit
                 (subunits (?then-unit)))
                (?then-unit
                 (meaning-args (?t))
                 (lex-class (holophrase)))
                <-
                (?then-unit
                 (HASH meaning ((then ?t)))
                 --
                 (HASH form ((sequence "it was then that " ?then-left ?then-right))))
                (?clause-unit
                 (HASH meaning ((:time ?event ?t)))
                 (meaning-args (?event))
                 (phrase-type clause)
                 --
                 (phrase-type clause)
                 (form-args (?then-right ?clause-right)))))

  (def-fcg-cxn good-morning-cxn
               ((?expression-unit
                 (meaning-args (?m))
                 (form-args (?left ?right))
                 (sem-class greeting)
                 (lex-class (fixed-expression))
                 (phrase-type clause))
                <-
                (?expression-unit
                 (HASH meaning ((good-02 ?g)
                                (morning ?m)
                                (:arg1-of ?m ?g)))
                 --
                 (HASH form ((sequence "Good morning" ?left ?right))))))

  (def-fcg-cxn fronted-direct-speech-cxn
               ((?direct-speech-unit
                 (meaning-args (?s))
                 (form-args (?left-opening-quote ?arg0-right))
                 (phrase-type direct-speech)
                 (subunits (?statement-unit ?speech-verb-unit ?speaker-unit)))
                <-
                (?statement-unit
                 (meaning-args (?s))
                 (phrase-type clause)
                 --
                 (form-args (?statement-left ?statement-right))
                 (phrase-type clause))
                (?speech-verb-unit
                 (lex-class (verb))
                 (meaning-args (?s2))
                 (sem-class activity)
                 (footprints (not argument-structure-cxn))
                 --
                 (lex-class (verb))
                 (form-args (?left-speech-verb ?right-speech-verb))
                 (footprints (not argument-structure-cxn)))
                (?speaker-unit
                 (meaning-args (?arg0))
                 (sem-class physical-entity)
                 --
                 (form-args (?arg0-left ?arg0-right))
                 (phrase-type noun-phrase))
                (?direct-speech-unit
                 (HASH meaning ((:arg0 ?s2 ?arg0)
                                (:arg1 ?s2 ?s)))
                 --
                 (HASH form ((sequence "'" ?left-opening-quote ?statement-left)
                             (sequence ",' " ?statement-right ?right-final-quote)
                             (precedes ?right-final-quote ?left-speech-verb)
                             (precedes ?right-final-quote ?arg0-left)))))
               :disable-automatic-footprints t)
  
  (def-fcg-cxn politely-adverb
               ((?verb-manner-unit
                 (subunits (?event-unit ?politely-unit))
                 (form-args (?event-left ?politely-right))
                 (meaning-args (?e))
                 (sem-class activity)
                 (lex-class (verb)))
                (?politely-unit
                 (lex-class (adverb))
                 (meaning-args (?p)))
                <-
                (?politely-unit
                 (HASH meaning ((polite-01 ?p)))
                 --
                 (HASH form ((sequence "politely" ?politely-left ?politely-right))))
                (?event-unit
                 (meaning-args (?e))
                 (sem-class activity)
                 --
                 (lex-class (verb))
                 (form-args (?event-left ?event-right)))
                (?verb-manner-unit
                 (HASH meaning ((:manner ?e ?p)))
                 --
                 (HASH form ((precedes ?event-right ?politely-left))))))


  (def-fcg-cxn active-transitive-cxn
               ((?transitive-clause-unit
                 (syn-valence (subject ?arg0-unit)
                              (direct-object ?arg1-unit))
                 (meaning-args (?event))
                 (phrase-type clause)
                 (sem-function proposition)
                 (form-args (?arg0-left ?arg1-right))
                 (subunits (?arg0-unit ?event-unit ?arg1-unit)))
                (?event-unit
                 (footprints (argument-structure-cxn)))
                <-
                (?arg0-unit
                 (meaning-args (?arg0))
                 (phrase-type noun-phrase)
                 --
                 (phrase-type noun-phrase)
                 (form-args (?arg0-left ?arg0-right)))
                (?event-unit
                 (meaning-args (?event))
                 (lex-class (verb))
                 (footprints (not argument-structure-cxn))
                 --
                 (form-args (?arg0-right ?event-left))
                 (lex-class (verb))
                 (footprints (not argument-structure-cxn)))
                (?arg1-unit
                 (meaning-args (?arg1))
                 (phrase-type noun-phrase)
                 --
                 (form-args (?event-left ?arg1-right))
                 (phrase-type noun-phrase))
                (?transitive-clause-unit
                 (HASH meaning ((:arg0 ?event ?arg0)
                                (:arg1 ?event ?arg1)))
                 --))
               :disable-automatic-footprints t)

  (def-fcg-cxn around-cxn
               ((?around-unit
                 (meaning-args (?a))
                 (form-args (?left ?right))
                 (lex-class (adverb))
                 (sem-class direction))
                <-
                (?around-unit
                 (HASH meaning ((around ?a)))
                 --
                 (HASH form ((sequence "around" ?left ?right))))))
  
  (def-fcg-cxn ergative-motion-direction-cxn
               ((?ergative-clause-unit
                 (meaning-args (?motion))
                 (form-args (?arg1-left ?direction-right))
                 (phrase-type clause)
                 (sem-function proposition)
                 (syn-valence (subject ?arg1-unit))
                 (subunits (?arg1-unit ?motion-unit ?direction-unit)))
                (?motion-unit
                 (footprints (argument-structure-cxn)))
                 <-
                (?arg1-unit
                 (meaning-args (?arg1))
                 (phrase-type noun-phrase)
                 --
                 (form-args (?arg1-left ?arg1-right))
                 (phrase-type noun-phrase))
                (?motion-unit
                 (meaning-args (?motion))
                 (lex-class (ergative-verb))
                 (sem-frame motion)
                 (footprints (not argument-structure-cxn))
                 --
                 (form-args (?arg1-right ?motion-left))
                 (lex-class (ergative-verb))
                 (footprints (not argument-structure-cxn)))
                (?direction-unit
                 (meaning-args (?direction))
                 (sem-class direction)
                 --
                 (form-args (?motion-left ?direction-right))
                 (sem-class direction))
                (?ergative-clause-unit
                 (HASH meaning ((:arg1 ?motion ?arg1)
                                (:direction ?motion ?direction)))
                 --))
               :disable-automatic-footprints t)

  (def-fcg-cxn when-cxn
               (
                <-
                (?when-unit
                 (form-args (?left ?right))
                 (lex-class (adverb temporal-adverb))
                 --
                 (HASH form ((sequence "when" ?left ?right))))))
  
  (def-fcg-cxn temporal-chained-propositions-cxn
               ((?temporal-subclause-unit
                 (phrase-type clause)
                 (meaning-args (?proposition-2))
                 (form-args (?temporal-left ?second-prop-right))
                 (syn-valence (subject ?second-subject-unit))
                 (subunits (?temporal-adverb-unit ?first-proposition-unit ?second-proposition-unit)))
                <-
                (?temporal-adverb-unit
                 --
                 (form-args (?temporal-left ?temporal-right))
                 (lex-class (temporal-adverb)))
                (?first-proposition-unit
                 (meaning-args (?proposition-1))
                 (sem-function proposition)
                 --
                 (form-args (?temporal-right ?first-prop-left))
                 (syn-valence (subject ?first-subject-unit)))
                (?first-subject-unit
                 (meaning-args (?same-ref))
                 --
                 (meaning-args (?same-ref)))
                (?second-proposition-unit
                 (meaning-args (?proposition-2))
                 (sem-function proposition)
                 --
                 (form-args (?first-prop-left ?second-prop-right))
                 (syn-valence (subject ?second-subject-unit)))
                (?second-subject-unit
                 (meaning-args (?same-ref))
                 --
                 (meaning-args (?same-ref)))
                (?temporal-subclause-unit
                 (HASH meaning ((:time ?proposition-2 ?proposition-1)))
                 --
                 )))

  (def-fcg-cxn although-cxn
               (
                <-
                (?although-unit
                 (form-args (?left ?right))
                 (lex-class (conjunction concessive-conjunction))
                 --
                 (HASH form ((sequence "although" ?left ?right))))))
               
  (def-fcg-cxn concessive-subclause-cxn
               ((?concessive-clause-unit
                 (subunits (?concessive-conjunction-unit ?clause-unit))
                 (phrase-type subclause)
                 (syn-valence (subject ?subject-unit))
                 (referent ?concession)
                 (meaning-args (?proposition ?concession))
                 (form-args (?conjuction-left ?clause-right)))
                <-
                (?concessive-conjunction-unit
                 --
                 (form-args (?conjuction-left ?conjunction-right))
                 (lex-class (concessive-conjunction)))
                (?clause-unit
                 (phrase-type clause)
                 (meaning-args (?concession))
                 --
                 (phrase-type clause)
                 (form-args (?conjunction-right ?clause-right))
                 (syn-valence (subject ?subject-unit)))
                (?concessive-clause-unit
                 (HASH meaning ((:concession ?proposition ?concession)))
                 --
                 )))

  (def-fcg-cxn main+subclause-cxn
               ((?sentence-unit
                 (meaning-args (?proposition))
                 (form-args (?main-clause-left ?sub-clause-right))
                 (syn-valence (subject ?subject-unit))
                 (subunits (?main-clause-unit ?comma-unit ?sub-clause-unit)))
                <-
                (?main-clause-unit
                 (phrase-type clause)
                 (sem-function proposition)
                 (meaning-args (?proposition))
                 --
                 (form-args (?main-clause-left ?main-clause-right))
                 (syn-valence (subject ?subject-main-clause-unit)))
                (?subject-main-clause-unit
                 (meaning-args (?same-ref))
                 --
                 (meaning-args (?same-ref)))
                (?comma-unit
                 --
                 (HASH form ((sequence "," ?comma-left ?comma-right)
                             (precedes ?main-clause-right ?comma-left)
                             (precedes ?comma-right ?sub-clause-left))))
                (?sub-clause-unit
                 (phrase-type subclause)
                 (referent ?subclause-ref)
                 (meaning-args (?proposition ?subclause-ref))
                 --
                 (phrase-type subclause)
                 (syn-valence (subject ?subject-sub-clause-unit))
                 (form-args (?sub-clause-left ?sub-clause-right)))
                (?subject-sub-clause-unit
                 (meaning-args (?same-ref))
                 --
                 (meaning-args (?same-ref))))))


(activate-monitor trace-fcg)
;;(comprehend "the fox")
;;(formulate '((fox f)))

;# ::id lpp_1943.1043 
(amr::equivalent-amr-predicate-networks
 (comprehend "it was then that the fox appeared")
 (amr::penman->predicates '(a / appear-01
                              :ARG1 (f / fox)
                              :time (t / then))))

(formulate (amr::penman->predicates '(a / appear-01
                              :ARG1 (f / fox)
                              :time (t / then))))



;# ::id lpp_1943.1044
(amr::equivalent-amr-predicate-networks
 (comprehend "'Good morning,' said the fox")
 (amr::penman->predicates '(s / say-01
                              :ARG0 (f / fox)
                              :ARG1 (m / morning
                                       :ARG1-of (g / good-02)))))

(formulate (amr::penman->predicates '(s / say-01
                              :ARG0 (f / fox)
                              :ARG1 (m / morning
                                       :ARG1-of (g / good-02)))))

;# ::id lpp_1943.1045

(comprehend "the little prince")

;;no equivalence, as respond-01 uses :arg2 instead of :arg1
(amr::equivalent-amr-predicate-networks
 (comprehend "'Good morning,' the little prince responded politely")
 (amr::penman->predicates '(r / respond-01
                              :ARG0 (p / prince
                                       :mod (l / little))
                              :ARG2 (m / morning
                                       :ARG1-of (g / good-02))
                              :manner (p2 / polite-01))))

(formulate (amr::penman->predicates '(r / respond-01
                              :ARG0 (p / prince
                                       :mod (l / little))
                              :ARG1 (m / morning
                                       :ARG1-of (g / good-02))
                              :manner (p2 / polite-01))))



;;Tot hier
;;;;;;;;;;;;;;;;;
(amr::equivalent-amr-predicate-networks
 (comprehend "when he turned around he saw nothing")
 (amr::penman->predicates '(s / see-01
                              :ARG0 p
                              :ARG1 (n / nothing)
                              :time (t / turn-01
                                       :ARG1 p
                                       :direction (a / around)))))

(comprehend-all " ' Good morning , ' the little prince responded politely , although when he turned around he saw nothing ")
(equivalent-amr-predicate-networks
 (comprehend " ' Good morning , ' the little prince responded politely , although when he turned around he saw nothing ")
 (amr::penman->predicates '(r / respond-01
                              :ARG0 (p / prince
                                       :mod (l / little))
                              :ARG2 (m / morning
                                       :ARG1-of (g / good-02))
                              :manner (p2 / polite-01)
                              :concession (s / see-01
                                             :ARG0 p
                                             :ARG1 (n / nothing)
                                             :time (t / turn-01
                                                      :ARG1 p
                                                      :direction (a / around))))))


