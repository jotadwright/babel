;;(ql:quickload :fcg)

(in-package :fcg)

#|
It was then that the fox appeared .
(a / appear-01
      :ARG1 (f / fox)
      :time (t / then))

 " Good morning , " said the fox .
(s / say-01
      :ARG0 (f / fox)
      :ARG1 (m / morning
            :ARG1-of (g / good-02)))

" Good morning , " the little prince responded politely , although when he turned around he saw nothing .
(r / respond-01
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
                  :direction (a / around))))
|#

(def-fcg-constructions little-prince-grammar
  ;; These are the feature-types that we declare. All other features are treated as feature-value pairs.
  :feature-types ((meaning set-of-predicates)
                  (form set-of-predicates)
                  (subunits set)
                  (lex-class set))
  ;; We specify the goal tests here.
  :fcg-configurations ((:production-goal-tests  :no-applicable-cxns)
                       (:parse-goal-tests :no-strings-in-root :connected-semantic-network :no-applicable-cxns))

  (def-fcg-cxn fox-cxn
               ((?fox-unit
                 (referent ?f)
                 (lex-class (noun))
                 (sem-class physical-entity)
                 )
                <-
                (?fox-unit
                 (HASH meaning ((fox ?f)))
                 --
                 (HASH form ((string ?fox-unit "fox"))))))

  (def-fcg-cxn noun-nominal-cxn
               ((?nominal-unit
                 (subunits (?noun-unit))
                 (referent ?ref)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?leftmost-nominal-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                <-
                (?noun-unit
                 (referent ?ref)
                 (sem-class ?sem-class)
                 --
                 (lex-class (noun)))))
                 
  (def-fcg-cxn the-x-cxn
               ((?noun-phrase-unit
                 (referent ?ref)
                 (phrase-type noun-phrase)
                 (sem-class ?sem-class)
                 (sem-function referring-expression)
                 (subunits (?the-unit ?nominal-unit))
                 (boundaries (leftmost-unit ?the-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                <-
                (?the-unit
                 --
                 (HASH form ((string ?the-unit "the"))))
                (?nominal-unit
                 --
                 (referent ?ref)
                 (syn-function nominal)
                 (sem-class ?sem-class)
                 (boundaries (leftmost-unit ?leftmost-nominal-unit)
                             (rightmost-unit ?rightmost-nominal-unit)))
                (?noun-phrase-unit
                 --
                 (HASH form ((meets ?the-unit ?leftmost-nominal-unit))))))

  (def-fcg-cxn appear-lex-cxn
               ((?appear-unit
                 (referent ?a)
                 (lex-class (ergative-verb verb))
                 (sem-class activity))
                <-
                (?appear-unit
                 (HASH meaning ((appear-01 ?a)))
                 --
                 (lemma appear)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn appear-appeared-morph
           (<-
            (?appeared-unit
             (lemma appear)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?appeared-unit "appeared"))))))

    (def-fcg-cxn say-lex-cxn
               ((?say-unit
                 (referent ?a)
                 (lex-class (verb))
                 (sem-class activity)
                 (sem-frame statement))
                <-
                (?say-unit
                 (HASH meaning ((say-01 ?a)))
                 --
                 (lemma say)
                 (agreement (person ?p)
                            (number ?n))
                 (tense ?t))))

  (def-fcg-cxn say-said-morph
           (<-
            (?said-unit
             (lemma say)
             (agreement (person ?p)
                        (number ?n))
             (tense past)
             --
             (HASH form ((string ?said-unit "said"))))))
  

  (def-fcg-cxn intransitive-ergative-cxn
               ((?ergative-clause-unit
                 (subunits (?arg1-unit ?ergative-verb-unit))
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?ergative-verb-unit))
                 (syn-valence (subject ?arg1-unit))
                 (phrase-type clause)
                 (referent ?event))
               
                <-
                (?arg1-unit
                 --
                 (referent ?arg1)
                 (sem-function referring-expression)
                 (boundaries (leftmost-unit ?leftmost-arg1-unit)
                             (rightmost-unit ?rightmost-arg1-unit)))
                (?ergative-verb-unit
                 (HASH meaning ((:arg1 ?event ?arg1)))
                 --
                 (referent ?event)
                 (lex-class (ergative-verb)))))

  (def-fcg-cxn it-was-then-that-X-cxn
               (<-
                (?then-unit
                 (referent ?t)
                 (lex-class holophrase)
                 --
                 (HASH form ((string ?it "it")
                             (string ?was "was")
                             (string ?then-unit "then")
                             (string ?that "that")
                             (meets ?it ?was)
                             (meets ?was ?then-unit)
                             (meets ?then-unit ?that))))
                (?clause-unit
                 (HASH meaning ((:time ?event ?t)))
                 --
                 (referent ?event)
                 (phrase-type clause)
                 (boundaries (leftmost-unit ?leftmost-event-unit)
                             (rightmost-unit ?rightmost-event-unit))
                 (HASH form ((meets ?that ?leftmost-event-unit))))))

  (def-fcg-cxn good-morning-cxn
               ((?good-unit
                 (referent ?g))
                (?morning-unit
                 (referent ?m))
                (?expression-unit
                 (referent ?m)
                 (sem-class greeting)
                 (lex-class fixed-expression)
                 (subunits (?good-unit ?morning-unit))
                 (boundaries (leftmost-unit ?good-unit)
                             (rightmost-unit ?morning-unit)))
                <-
                (?good-unit
                 (HASH meaning ((good-02 ?g)))
                 --
                 (HASH form ((string ?good-unit "Good"))))
                (?morning-unit
                 (HASH meaning ((morning ?m)))
                 --
                 (HASH form ((string ?morning-unit "morning"))))
               
                (?expression-unit
                 (HASH meaning ((:arg1-of ?m ?g)))
                 --
                 (HASH form ((meets ?good-unit ?morning-unit))))))

  (def-fcg-cxn topicalised-statement-cxn
               ((?clause-unit
                 (subunits (?statement-unit ?statement-verb-unit ?arg0-unit))
                 (syn-valence (subject ?arg0-unit))
                 (boundaries (leftmost-unit ?quote-1)
                             (rightmost-unit ?righmost-arg0-unit))
                 (phrase-type clause)
                 (referent ?s2))
                 
                <-
                (?statement-unit
                 --
                 (referent ?s)
                 (boundaries (leftmost-unit ?leftmost-statement-unit)
                             (rightmost-unit ?rightmost-statement-unit))
                 (HASH form ((string ?quote-1 "'")
                             (string ?comma ",")
                             (string ?quote-2 "'")
                             (meets ?comma ?quote-2)
                             (meets ?quote-1 ?leftmost-statement-unit)
                             (meets ?rightmost-statement-unit ?comma))))
                (?statement-verb-unit
                 (HASH meaning ((:arg0 ?s2 ?arg0)
                                (:arg1 ?s2 ?s)))
                 --
                 (sem-frame statement)
                 (lex-class (verb))
                 (referent ?s2)
                 (HASH form ((meets ?quote-2 ?statement-verb-unit))))
                (?arg0-unit
                 --
                 (referent ?arg0)
                 (boundaries (leftmost-unit ?leftmost-arg0-unit)
                             (rightmost-unit ?righmost-arg0-unit))
                 (HASH form ((meets ?statement-verb-unit ?leftmost-arg0-unit))))))

  

  
)

(activate-monitor trace-fcg)

(comprehend "it was then that the fox appeared")

(comprehend " ' Good morning , ' said the fox")


" Good morning , " the little prince responded politely , although when he turned around he saw nothing .


(r / respond-01
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
                  :direction (a / around))))

