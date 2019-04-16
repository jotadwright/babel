;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; AMR grammar Banarescu Corpus developed by Martina Galletti (Spring 2019)
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(in-package :amr-grammar)
(def-fcg-constructions amr-Banarescu-grammar
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (args sequence)
                  (footprints set))
  :fcg-configurations ((:de-render-mode . :de-render-string-meets-precedes-first)
                       (:parse-goal-tests :no-strings-in-root :connected-structure :no-applicable-cxns)
                       (:max-nr-of-nodes . 1000))

;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Lexical Constructions
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; ---------------------------------------------------------------------------------------------------
;; Articles
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn the-cxn
              (<-
               (?the-unit
                (syn-cat (lex-class article)
                          (definite +)
                          (number ?number))
                --
                (HASH form ((string ?the-unit "the"))))))

(def-fcg-cxn an-cxn
             (<-
              (?an-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg))
                --
                (HASH form ((string ?an-unit "an"))))))

(def-fcg-cxn a-cxn
             (<-
              (?a-unit
               (syn-cat (lex-class article)
                        (definite -)
                        (number sg))
               --
               (HASH form ((string ?a-unit "a"))))))


;; ---------------------------------------------------------------------------------------------------
;; Adjectives
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn afraid-cxn
            ((?afraid-unit
              (referent ?f)
              (meaning ((fear-01 ?f)))
              (syn-cat (lex-class adjective)
                       (syn-function verbal)
                       (arg2 -)))
             <-
             (?afraid-unit
              --
              (HASH form ((string ?afraid-unit "afraid"))))))

(def-fcg-cxn appropriate-cxn
            ((?appropriate-unit
              (referent ?a)
              (meaning ((appropriate ?a)))
              (syn-cat (lex-class adjective)
                       (syn-function predicative)
                       (derived-from-verb -)))
             <-
             (?appropriate-unit
              --
              (HASH form ((string ?appropriate-unit "appropriate"))))))

(def-fcg-cxn atomic-cxn
             ((?atomic-unit
               (referent ?a)
                (meaning ((atom ?a)))
                (syn-cat (lex-class adjective)
                         (syn-function adjectival)
                         (pertainym +)))
               <-
               (?atomic-unit
                --
                (HASH form ((string ?atomic-unit "atomic"))))))

(def-fcg-cxn attractive-cxn
             ((?attractive-unit
               (referent ?a)
                (meaning ((attract-01 ?a)))
                (syn-cat (lex-class adjective)
                         (syn-function verbal)
                         (arg2 -))
                (sem-cat (sem-class quality))
                (sem-valence (:arg0-of ?arg0-of)))
              <-
              (?attractive-unit
               --
               (HASH form  ((string ?attractive-unit "attractive"))))))
 
(def-fcg-cxn edible-cxn
             ((?edible-unit
               (referent ?e)
               (meaning ((eat-01 ?e)
                          (possible ?p)
                          (:domain-of ?e ?p)))
               (syn-cat (lex-class adjective)
                         (syn-function adjectival))
               (sem-cat (sem-class possibility)))
              <-
              (?edible-unit
               --
               (HASH form ((string ?edible-unit "edible"))))))

(def-fcg-cxn inappropriate-cxn
            ((?inappropriate-unit
              (referent ?a)
              (meaning ((appropriate ?a)
                        (:polarity ?a -)))
              (syn-cat (lex-class adjective)
                       (syn-function predicative)
                       (derived-from-verb -)))
             <-
             (?inappropriate-unit
              --
              (HASH form ((string ?inappropriate-unit "inappropriate"))))))

(def-fcg-cxn small-cxn
             ((?small-unit
               (referent ?s)
               (meaning ((small ?s)))
                 (syn-cat (lex-class adjective)
                          (syn-function adjectival))
                 (sem-cat (sem-class manner)))
              <-
              (?small-unit
               --
               (HASH form ((string ?small-unit "small"))))))

(def-fcg-cxn taxable-cxn
             ((?taxable-unit
                (referent ?t)
                (meaning ((tax-01 ?t)))
                (syn-cat (lex-class adjective)
                         (syn-function adjectival))
                (sem-cat (sem-class possibility)))
              <-
              (?taxable-unit
               --
               (HASH form  ((string ?taxable-unit "taxable"))))))

(def-fcg-cxn tough-cxn
             ((?tough-unit
               (referent ?t)
               (meaning ((tough ?t)))
               (syn-cat (lex-class adjective)
                        (syn-function predicative)
                        (arg0-of +)))
              <-
              (?tough-unit
              --
              (HASH form ((string ?tough-unit "tough"))))))

;; ---------------------------------------------------------------------------------------------------
;; Adverb
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn after-cxn
             ((?after-unit
               (referent ?a)
               (meaning ((after ?a)))
               (syn-cat (lex-class adverb))
               (sem-cat (sem-class time)))
              <-
              (?after-unit
              --
              (HASH form ((string ?after-unit "after"))))))

(def-fcg-cxn hard-cxn
             ((?hard-unit
               (referent ?h)
               (meaning ((hard ?h)))
               (syn-cat (lex-class adverb)
                         (syn-function adverbial))
               (sem-cat (sem-class manner)))
              <-
              (?hard-unit
              --
              (HASH form ((string ?hard-unit "hard"))))))

(def-fcg-cxn yesterday-cxn
             ((?yesterday-unit
               (referent ?y)
               (meaning ((yesterday ?y)))
               (syn-cat (lex-class adverb)
                        (syn-function adverbial-time))
               (sem-cat (sem-class time))
               (boundaries (rightmost-unit ?yesterday-unit)
                            (leftmost-unit ?yesterday-unit)))
              <-
              (?yesterday-unit
              --
              (HASH form ((string ?yesterday-unit "yesterday"))))))

(def-fcg-cxn not-cxn
             ((?not-unit
               (syn-cat (lex-class adverb)
                        (polarity -)))
               <-
              (?not-unit
               --
               (HASH form ((string ?not-unit "not"))))))
;; ---------------------------------------------------------------------------------------------------
;; Nouns
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn adjustments-cxn
            ((?adjustments-unit
              (referent ?a)
              (meaning ((adjust-01 ?a)))
              (syn-cat (lex-class noun)
                       (syn-function verbal)
                       (arg2 -)))
             <-
             (?adjustments-unit
              --
              (HASH form ((string ?adjustments-unit "adjustments"))))))

(def-fcg-cxn answer-cxn
             ((?answer-unit
               (referent ?a)
               (meaning ((answer ?a)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
               <-
               (?answer-unit
                --
                (HASH form ((string ?answer-unit "answer"))))))

(def-fcg-cxn atom-cxn
             ((?atom-unit
               (referent ?a)
               (meaning ((atom ?a)))
               (syn-cat (lex-class noun)
                        (syn-function adjectival))
               (sem-cat (sem-role pertainym)))
              <-
               (?atom-unit
                --
                (HASH form ((string ?atom-unit "atom"))))))

(def-fcg-cxn battle-cxn
             ((?battle-unit
               (referent ?b)
               (meaning ((battle-01 ?b)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
              <-
              (?battle-unit
               --
               (HASH form ((string ?battle-unit "battle"))))))

(def-fcg-cxn bomb-cxn
             ((?bomb-unit
               (referent ?b)
               (meaning ((bomb ?b)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-class inanimate-object)))
              <-
              (?bomb-unit
               --
               (HASH form ((string ?bomb-unit "bomb"))))))

(def-fcg-cxn bond-cxn
             ((?bond-unit
                 (referent ?b)
                 (meaning ((bond ?b)))
                 (syn-cat (lex-class adjectival-noun)
                          (pertainym -)
                          (syn-function adjectival)))
              <-
              (?bond-unit
               --
               (HASH form ((string ?bond-unit "bond"))))))

(def-fcg-cxn boy-cxn
             ((?boy-unit
               (referent ?b)
               (meaning ((boy ?b)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role agent)))
              <-
              (?boy-unit
               --
               (HASH form ((string ?boy-unit "boy"))))))

(def-fcg-cxn case-cxn
             ((?case-unit
               (referent ?c)
               (meaning ((case ?c)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?case-unit
               --
               (HASH form ((string ?case-unit "case"))))))

(def-fcg-cxn city-cxn
             ((?city-unit
               (referent ?c)
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?city-unit
              --
              (HASH form ((string ?city-unit "city"))))))

(def-fcg-cxn college-cxn
             ((?college-unit
               (referent ?c)
               (meaning ((college ?c)))
               (syn-cat (lex-class adjectival-noun)
                        (syn-function adjectival))
              (sem-cat (sem-class age)))
               <-
               (?college-unit
                --
                (HASH form ((string ?college-unit "college"))))))

(def-fcg-cxn comment-cxn
             ((?comment-unit
               (referent ?c)
               (meaning ((comment ?c)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?comment-unit
               --
               (HASH form ((string ?comment-unit "comment"))))))

(def-fcg-cxn description-cxn
             ((?description-unit
               (referent ?d)
               (meaning ((describe-01 ?d)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
               <-
               (?description-unit
                --
                (HASH form ((string ?description-unit "description"))))))

(def-fcg-cxn destruction-cxn
             ((?destruction-unit
               (referent ?d)
               (meaning ((destroy-01 ?d)))
               (syn-cat (lex-class noun)
                        (syn-function patient)))
              <-
              (?destruction-unit
               --
               (HASH form ((string ?destruction-unit "destruction"))))))

(def-fcg-cxn disaster-cxn
             ((?disaster-unit
               (referent ?d)
               (meaning ((disaster ?d)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?disaster-unit
               --
               (HASH form ((string ?disaster-unit "disaster"))))))

(def-fcg-cxn explosion-cxn
             ((?explosion-unit
               (referent ?e)
               (meaning ((explode-01 ?e)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?explosion-unit
               --
               (HASH form ((string ?explosion-unit "explosion"))))))

(def-fcg-cxn fear-cxn
            ((?fear-unit
              (referent ?f)
              (meaning ((fear-01 ?f)))
              (syn-cat (lex-class noun)
                       (syn-function verbal)
                       (preposition +)
                       (arg2 -)
                       (quantifier -))) ;; exclude quant-of
             <-
             (?fear-unit
              --
              (HASH form ((string ?fear-unit "fear"))))))

(def-fcg-cxn fund-cxn
             ((?fund-unit
               (referent ?f)
                (meaning ((fund ?f)))
                (syn-cat (lex-class noun)
                         (syn-function nominal)))
               <-
               (?fund-unit
                --
                (HASH form ((string ?fund-unit "fund"))))))

(def-fcg-cxn girl-lex-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)))
              <-
              (?girl-unit
               (lex-id girl))))
 
(def-fcg-cxn girl-morph-cxn
             ((?girl-unit
               (referent ?g)
               (meaning ((girl ?g)))
               (lex-id girl)
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role agent)))
              <-
              (?girl-unit
               --
               (HASH form ((string ?girl-unit "girl"))))))
                  
(def-fcg-cxn group-cxn
             ((?group-unit
               (referent ?g)
               (meaning ((group ?g)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?group-unit
               --
               (HASH form ((string ?group-unit "group"))))))

(def-fcg-cxn group-morph-cxn
             ((?girls-unit
               (referent ?g)
               (lex-id girl)
               (meaning ((girl ?g)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function direct-object)
                        (inverse-arg0 +)))
              <-
              (?girls-unit
               --
               (HASH form ((string ?girls-unit "girls"))))))

(def-fcg-cxn history-cxn
             ((?history-unit
               (referent ?h)
               (meaning ((history ?h)))
               (syn-cat (lex-class adjectival-noun)
                        (pertainym -)
                        (syn-function adjectival))
               (sem-cat (sem-role patient)))
              <-
              (?history-unit
               --
               (HASH form ((string ?history-unit "history"))))))

(def-fcg-cxn investor-cxn 
             ((?investor-unit
               (referent ?i)
               (meaning ((person ?p)
                         (invest-01 ?i)
                         (:arg0-of ?p ?i)))
               (sem-valence (arg0-of ?i)) ;;a person or thing that does something
               (syn-cat (lex-class noun)
                        (syn-function nominal)
                        (adjectival -))
               (sem-cat (sem-class person)))
              <-
              (?investor-unit
               --
               (HASH form ((string ?investor-unit "investor"))))))

(def-fcg-cxn jar-cxn
             ((?jar-unit
               (referent ?j)
               (meaning ((jar ?j)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-class location)))
              <-
              (?jar-unit
               --
               (HASH form ((string ?jar-unit "jar"))))))

(def-fcg-cxn judge-cxn
             ((?judge-unit
               (referent ?j)
               (meaning ((judge ?j)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?judge-unit
               --
                (HASH form ((string ?judge-unit "judge"))))))

(def-fcg-cxn june-cxn
             ((?june-unit
               (referent ?d2)
               (meaning ((date-entity ?d2)
                         (:month ?d2 6)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-role date-entity)))
              <-
              (?june-unit
               --
               (HASH form ((string ?june-unit "June"))))))

(def-fcg-cxn lawyer-cxn
             ((?lawyer-unit
               (referent ?l)
               (meaning ((lawyer ?l)))
               (syn-cat (lex-class noun)
                        (syn-function predicative)
                        (derived-from-verb -)))
               <-
               (?lawyer-unit
                --
                (HASH form ((string ?lawyer-unit "lawyer"))))))

(def-fcg-cxn machine-cxn
             ((?machine-unit
               (referent ?m)
               (meaning ((machine ?m)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
               <-
               (?machine-unit
                --
                (HASH form ((string ?machine-unit "machine"))))))

(def-fcg-cxn man-cxn
             ((?man-unit
               (referent ?m)
               (meaning ((man ?m)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?man-unit
               --
               (HASH form ((string ?man-unit "man"))))))

(def-fcg-cxn marble-cxn
             ((?marble-unit
               (referent ?m)
               (meaning ((marble ?m)))
               (syn-cat (lex-class noun)
                        (person 3)
                        (number sg)
                        (syn-function nominal)))
              <-
              (?marble-unit
               --
               (HASH form ((string ?marble-unit "marble"))))))

(def-fcg-cxn mission-cxn
             ((?mission-unit
               (referent ?m2)
               (meaning ((mission ?m2)))
               (syn-cat (lex-class noun)
                        (syn-function patient)))
               <-
               (?mission-unit
                --
                (HASH form ((string ?mission-unit "mission"))))))

(def-fcg-cxn nation-cxn
             ((?nation-unit
               (referent ?n)
               (meaning ((nation ?n)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
              <-
              (?nation-unit
               --
               (HASH form ((string ?nation-unit "nation"))))))

(def-fcg-cxn non-jar-cxn
             ((?non-jar-unit
               (referent ?j)
               (meaning ((jar ?j)
                         (:polarity ?j -)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class location)))
              <-
              (?non-jar-unit
               --
               (HASH form ((string ?non-jar-unit "non-jar"))))))

(def-fcg-cxn number-cxn
             ((?number-unit
               (referent ?n)
               (meaning ((number ?n)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)
                        (quantifier +))
                (sem-cat (sem-role direct-object)))
              <-
               (?number-unit
                --
                (HASH form ((string ?number-unit "number"))))))

(def-fcg-cxn opinion-cxn
             ((?opinion-unit
               (referent ?o)
               (meaning ((thing ?t)
                         (opine-01 ?o)
                         (:arg1-of ?t ?o)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
               <-
               (?opinion-unit
                --
                (HASH form ((string ?opinion-unit "opinion"))))))

(def-fcg-cxn orc-lex-cxn
             ((?orc-unit
               (referent ?o)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?orc-unit
               (lex-id orc))))

(def-fcg-cxn orcs-morph-cxn
             ((?orcs-unit
               (referent ?o)
               (lex-id orc)
               (meaning ((orc ?o)))
               (syn-cat (lex-class noun)
                        (number pl)
                        (syn-function direct-object)))
              <-
              (?orcs-unit
               --
               (HASH form ((string ?orcs-unit "orcs"))))))

(def-fcg-cxn orc-slaying-cxn
             ((?orc-slaying-unit
               (referent ?s)
               (meaning ((slay-01 ?s)
                         (orc ?o)
                         (:arg1 ?s ?o)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?orc-slaying-unit
               --
               (HASH form ((string ?orc-slaying-unit "orc-slaying"))))))

(def-fcg-cxn pandas-cxn
             ((?pandas-unit
               (referent ?p)
               (meaning ((panda ?p)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?pandas-unit
               --
               (HASH form ((string ?pandas-unit "pandas"))))))

(def-fcg-cxn possible-cxn
             ((?possible-unit
               (referent ?p)
               (meaning ((possible ?p)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?possible-unit
               --
               (HASH form ((string ?possible-unit "possible"))))))

(def-fcg-cxn president-capitalized-cxn
             ((?president-unit
               (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                         (syn-function nominal))
                (sem-valence (name ?n)))
              <-
              (?president-unit
               --
               (HASH form ((string ?president-unit "President"))))))

(def-fcg-cxn president-cxn
             ((?president-unit
                (referent ?p)
                (meaning ((president ?p)))
                (syn-cat (lex-class noun)
                         (syn-function nominal))
                (sem-valence (name ?n)))
              <-
              (?president-unit
               --
               (HASH form ((string ?president-unit "president"))))))

(def-fcg-cxn professor-cxn
             ((?professor-unit
               (referent ?t)
               (meaning ((person ?p)
                         (teach-01 ?t)
                         (:arg0-of ?p ?t)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?professor-unit
               --
               (HASH form ((string ?professor-unit "professor"))))))

(def-fcg-cxn proposal-cxn
             ((?proposal-unit
               (referent ?t)
               (meaning ((thing ?t)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
              <-
              (?proposal-unit
               --
               (HASH form ((string ?proposal-unit "proposal"))))))

(def-fcg-cxn responsibility-cxn
             ((?responsibility-unit
               (referent ?r)
               (meaning ((responsible-41 ?r)))
               (syn-cat (lex-class noun)
                        (syn-function verbal)
                        (preposition +)))
              <-
              (?responsibility-unit
               --
               (HASH form ((string ?responsibility-unit "responsibility"))))))

(def-fcg-cxn responsible-cxn
             ((?responsible-unit
               (referent ?r)
               (meaning ((responsible-41 ?r)))
               (syn-cat (lex-class noun)
                        (syn-function verbal)
                        (preposition +)))
              <-
              (?responsible-unit
               --
               (HASH form ((string ?responsible-unit "responsible"))))))
 
 (def-fcg-cxn room-cxn
             ((?room-unit
               (referent ?r)
               (meaning ((room ?r)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-role direct-object)))
               <-
               (?room-unit
                --
                (HASH form ((string ?room-unit "room"))))))

(def-fcg-cxn sandwich-cxn
              ((?sandwich-unit
                (referent ?s)
                (meaning ((sandwich ?s)))
                (syn-cat (lex-class noun)
                         (syn-function nominal)))
               <-
               (?sandwich-unit
                --
                (HASH form ((string ?sandwich-unit "sandwich"))))))

(def-fcg-cxn soldier-cxn
             ((?soldier-unit
               (referent ?s)
               (meaning ((soldier ?s)))
               (syn-cat (lex-class noun)
                        (syn-function nominal))
               (sem-cat (sem-class agent)))
              <-
              (?soldier-unit
               --
               (HASH form ((string ?soldier-unit "soldier"))))))

(def-fcg-cxn spy-cxn
              ((?spy-unit
                (referent ?s)
                (meaning ((spy ?s)))
                (syn-cat (lex-class noun)
                         (syn-function nominal))
                (sem-cat (sem-class person)))
               <-
               (?spy-unit
                --
                (HASH form  ((string ?spy-unit "spy"))))))

(def-fcg-cxn teacher-cxn
             ((?teacher-unit
               (referent ?t)
               (meaning ((person ?p)
                         (teach-01 ?t)
                         (:arg0-of ?p ?t)))
                (syn-cat (lex-class noun)
                         (syn-function nominal)
                         (adjectival -))
                (sem-cat (sem-class title)))
              <-
              (?teacher-unit
                --
              (HASH form ((string ?teacher-unit "teacher"))))))

(def-fcg-cxn team-cxn
             ((?team-unit
               (referent ?t-2)
               (meaning ((team ?t-2)))
                (syn-cat (lex-class noun)))
              <-
              (?team-unit
                --
              (HASH form ((string ?team-unit "team"))))))

(def-fcg-cxn town-cxn
             ((?town-unit
               (referent ?t)
               (meaning ((town ?t)))
                (syn-cat (lex-class noun)))
              <-
              (?town-unit
                --
              (HASH form ((string ?town-unit "town"))))))

(def-fcg-cxn toy-cxn
             ((?toy-unit
               (referent ?t)
               (meaning ((toy ?t)))
                (syn-cat (lex-class noun)))
              <-
              (?toy-unit
                --
              (HASH form ((string ?toy-unit "toy"))))))

(def-fcg-cxn war-cxn
             ((?war-unit
               (referent ?w)
               (meaning ((war-01 ?w)))
               (syn-cat (lex-class noun)
                        (syn-function nominal)))
               <-
               (?war-unit
                --
                (HASH form ((string ?war-unit "war"))))))

(def-fcg-cxn white-cxn
             ((?white-unit
               (referent ?w)
               (meaning ((white ?w)))
               (syn-cat (lex-class adjective)
                        (syn-function predicative)
                        (derived-from-verb -)
                        (arg0-of -)))
              <-
              (?white-unit
              --
              (HASH form ((string ?white-unit "white"))))))

(def-fcg-cxn woman-cxn
             ((?woman-unit
               (referent ?w)
               (meaning ((woman ?w)))
               (syn-cat (lex-class noun)
                         (syn-function agent))
               (sem-cat (sem-class agent)))
               <-
               (?woman-unit
                --
                (HASH form ((string ?woman-unit "woman"))))))

(def-fcg-cxn woman-lex-cxn
             ((?woman-unit
               (referent ?w)
               (meaning ((woman ?w)))
               (syn-cat (lex-class noun)))
              <-
              (?woman-unit
               (lex-id woman))))

(def-fcg-cxn woman-cxn
             ((?woman-unit
               (referent ?w)
               (lex-id woman)
               (meaning ((woman ?w)))
               (syn-cat (lex-class noun)
                        (number sg)
                        (syn-function agent))
               (sem-cat (sem-class agent)))
               <-
               (?woman-unit
                --
                (HASH form ((string ?woman-unit "woman"))))))


(def-fcg-cxn women-cxn
             ((?women-unit
               (referent ?w)
               (meaning ((woman ?w)))
               (lex-id woman)
               (syn-cat (lex-class noun)
                        (number pl)))
               <-
               (?women-unit
                --
                (HASH form ((string ?women-unit "women"))))))

(def-fcg-cxn work-noun-cxn
             ((?work-noun-unit
               (referent ?w)
               (meaning ((work ?w)))
                (syn-cat (lex-class noun)))
              <-
              (?work-noun-unit
               --
              (HASH form ((string ?work-noun-unit "work"))))))

(def-fcg-cxn worker-cxn
             ((?worker-unit
               (referent ?w)
               (meaning ((work-01 ?w)))
                (syn-cat (lex-class noun)
                          (derived-from-verb +)))
              <-
              (?worker-unit
               --
              (HASH form ((string ?worker-unit "worker"))))))

;; ---------------------------------------------------------------------------------------------------
;; Particular Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn colon-cxn
             ((?colon-unit
               (syn-cat(syn-function preposition-arg2)))
               <-
               (?colon-unit
                --
                (HASH form ((string ?colon-unit ":"))))))

(def-fcg-cxn interrogative-cxn
             ((?interrogative-unit
               (syn-cat (syn-function interrogative-form)))
               <-
               (?interrogative-unit
                --
                (HASH form ((string ?interrogative-unit "?"))))))

(def-fcg-cxn its-contracted-cxn
             ((?its-contracted-unit
              (syn-cat (syn-function subject)))
              <-
              (?its-contracted-unit
               --
               (HASH form ((string ?its-contracted-unit "it's"))))))

(def-fcg-cxn s-possessive-cxn
             ((?s-possessive-unit
              (syn-cat (syn-function possessive-form)))
              <-
              (?s-possessive-unit
               --
               (HASH form ((string ?s-possessive-unit "'s"))))))

;; ---------------------------------------------------------------------------------------------------
;; Prepositions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn as-cxn
             ((?as-unit
               (syn-cat (lex-class preposition)
                        (syn-function preposition-arg2)))
              <-
              (?as-unit
               --
               (HASH form ((string ?as-unit "as"))))))

(def-fcg-cxn by-cxn
             ((?by-unit
               (syn-cat (lex-class preposition)))
              <-
              (?by-unit
               --
               (HASH form ((string ?by-unit "by"))))))

(def-fcg-cxn for-cxn
             ((?for-unit
               (syn-cat (lex-class preposition)))
              <-
              (?for-unit
               --
               (HASH form ((string ?for-unit "for"))))))

(def-fcg-cxn from-cxn
             ((?from-unit
               (syn-cat (lex-class preposition)))
              <-
              (?from-unit
               --
               (HASH form ((string ?from-unit "from"))))))

(def-fcg-cxn in-cxn
             ((?in-unit
               (syn-cat (lex-class preposition)))
              <-
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))))

(def-fcg-cxn of-cxn
               ((?of-unit
                 (syn-cat (lex-class preposition)
                           (syn-function specification)))
                <-
                (?of-unit
                 --
                 (HASH form ((string ?of-unit "of"))))))

(def-fcg-cxn to-cxn
               ((?to-unit
                 (syn-cat (lex-class preposition)))
                <-
                (?to-unit
                 --
                 (HASH form ((string ?to-unit "to"))))))
;; ---------------------------------------------------------------------------------------------------
;; Proper Nouns
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn Zintan-cxn
             ((?Zintan-unit
               (referent ?c)
               (syn-cat (lex-class proper-noun)
                         (syn-function nominal))
               (sem-cat (sem-class location)))
              <-
              (?Zintan-unit
               (HASH meaning ((city ?c)
                              (name ?n)
                              (:name ?c ?n)
                              (:op1 ?n "Zintan")))
               --
               (HASH form ((string ?Zintan-unit "Zintan"))))))
 
(def-fcg-cxn Obama-cxn
              ((?Obama-unit
                (referent ?n)
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)
                         (proper-noun +))
                (sem-cat (sem-class person))
                (meaning ((name ?n)
                          (:op1 ?n "Obama"))))
               <-
               (?Obama-unit
                --
                (HASH form ((string ?Obama-unit "Obama"))))))

(def-fcg-cxn Mollie-cxn
             ((?Mollie-unit
               (referent ?p)
               (meaning ((person ?p)
                         (name ?n)
                         (:name ?p ?n)
                         (:op1 ?n "Mollie")))
               (sem-valence (name ?n))
               (sem-cat (sem-class person))
               (syn-cat (lex-class proper-noun)
                        (syn-function nominal)))
              <-
              (?Mollie-unit
               --
               (HASH form ((string ?Mollie-unit "Mollie"))))))

(def-fcg-cxn Elsevier-cxn
             ((?Elsevier-unit
               (referent ?n)
               (meaning ((name ?n)
                         (:op1 ?n "Elsevier")))
               (sem-valence (name ?n))
               (sem-cat (sem-class person))
               (syn-cat (lex-class proper-noun)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?Elsevier-unit)
                           (rightmost-unit ?Elsevier-unit)))
              <-
              (?Elsevier-unit
               --
               (HASH form ((string ?Elsevier-unit "Elsevier"))))))

(def-fcg-cxn Dutch-cxn
             ((?Dutch-unit
               (referent ?c)
               (meaning ((country ?c)
                         (name ?n2)
                         (:name ?c ?n2)
                         (:op1 ?n2 "Netherlands")))
               (syn-cat (lex-class proper-noun)
                        (syn-function nominal)))
              <-
              (?Dutch-unit
               --
               (HASH form ((string ?Dutch-unit "Dutch"))))))

(def-fcg-cxn Brown-last-name-cxn
             ((?named-entity-unit
               (subunits (?brown-unit ?first-name-unit))
               (referent ?p)
               (syn-cat (phrase-type noun-phrase)
                        (person 3)
                        (number sg)
                        (syn-function nominal)
                        (part-of-phrase +)
                        (named-entity-type person))
               (boundaries (leftmost-unit ?first-name-unit)
                           (rightmost-unit ?brown-unit)))
              (?brown-unit
               (referent ?m)
               (meaning ((:op2 ?m "Brown")))
                (sem-cat (sem-class person))
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)))
              <-
              (?first-name-unit
               --
               (referent ?p)
               (sem-valence (name ?m))
               (sem-cat (sem-class person)))
               (?brown-unit
                --
                (HASH form ((string ?brown-unit "Brown"))))
               (?named-entity-unit
                --
                (HASH form ((meets ?first-name-unit ?brown-unit))))))

(def-fcg-cxn group-Elsevier-cxn
             ((?named-entity-unit-2
               (subunits (?group-unit ?first-name-unit))
               (referent ?g)
               (syn-cat (phrase-type noun-phrase)
                        (syn-function nominal)
                        (inverse-arg0 +))
               (boundaries (rightmost-unit ?first-name-rightmost-unit)
                           (leftmost-unit ?group-leftmost-unit))
               (meaning ((:name ?g ?n)))
                (sem-cat (sem-class person))
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)))
              <-
              (?first-name-unit
               --
               (referent ?n)
               (syn-cat (phrase-type noun-phrase)
                        (named-entity-type group))
               (boundaries (leftmost-unit ?first-name-leftmost-unit)
                           (rightmost-unit ?first-name-rightmost-unit)))
               (?group-unit
                --
                (referent ?g)
                (syn-cat (phrase-type nominal))
                (boundaries (leftmost-unit ?group-leftmost-unit)
                            (rightmost-unit ?group-rightmost-unit)))
               (?named-entity-unit-2
                --
                (HASH form ((meets ?group-rightmost-unit ?first-name-leftmost-unit))))))

(def-fcg-cxn NV-Elsevier-cxn
             ((?named-entity-unit
               (subunits (?NV-unit ?first-name-unit))
               (referent ?n)
               (syn-cat (phrase-type noun-phrase)
                        (syn-function nominal)
                        (named-entity-type group))
               (boundaries (leftmost-unit ?first-name-unit)
                           (rightmost-unit ?NV-unit)))
              (?NV-unit
               (referent ?n)
               (meaning ((:op2 ?n "N.V.")))
                (sem-cat (sem-class person))
                (syn-cat (lex-class proper-noun)
                         (syn-function nominal)))
              <-
              (?first-name-unit
               --
               (referent ?g)
               (sem-valence (name ?n))
               (sem-cat (sem-class person)))
               (?NV-unit
                --
                (HASH form ((string ?NV-unit "N.V."))))
               (?named-entity-unit
                --
                (HASH form ((meets ?first-name-unit ?NV-unit))))))
;; ---------------------------------------------------------------------------------------------------
;; Pronouns
;; ---------------------------------------------------------------------------------------------------

(def-fcg-cxn what-thing-cxn 
             ((?what-thing-unit
               (referent ?t)
               (meaning ((thing ?t)))
               (syn-cat (lex-class pronoun)
                        (number ?numb)
                        (syn-function nominal)
                        (interrogative -))
               (sem-cat (sem-role object)))
              <-
              (?what-thing-unit
               --
               (HASH form ((string ?what-thing-unit "what"))))))

(def-fcg-cxn what-interrogative-cxn 
             ((?what-interrogative-unit
               (referent ?a)
               (meaning ((amr-unknown ?a)))
               (syn-cat (lex-class pronoun)
                        (syn-function interrogative)
                         (interrogative +))
               (boundaries (leftmost-unit ?what-leftmost-unit)
                           (rightmost-unit ?what-rightmost-unit)))
              <-
              (?what-interrogative-unit
               --
               (HASH form ((string ?what-interrogative-unit "what"))))))

(def-fcg-cxn where-cxn 
             ((?where-unit
               (referent ?a)
               (meaning ((amr-unknown ?a)))
               (syn-cat (lex-class pronoun)
                        (syn-function interrogative)
                        (interrogative +)
                        (location +)))
              <-
              (?where-unit
               --
               (HASH form ((string ?where-unit "where"))))))

(def-fcg-cxn who-cxn 
             ((?who-unit
               (referent ?b)
               (syn-cat (lex-class pronoun)
                        (relative +)))
              <-
              (?who-unit
               --
               (HASH form ((string ?who-unit "who"))))))

(def-fcg-cxn whose-interrogative-cxn 
             ((?whose-interrogative-unit
               (referent ?a)
               (meaning ((amr-unknown ?a)))
               (syn-cat (lex-class pronoun)
                        (syn-function interrogative)))
              <-
              (?whose-interrogative-unit
               --
               (HASH form ((string ?whose-interrogative-unit "whose"))))))

;; ---------------------------------------------------------------------------------------------------
;; Verbs
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn adjusted-cxn
             ((?adjusted-unit
               (referent ?a)
               (meaning ((adjust-01 ?a)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
               <-
               (?adjusted-unit
                --
                (HASH form ((string ?adjusted-unit "adjusted"))))))

(def-fcg-cxn are-cxn
             ((?are-unit
               (referent ?are)
               (syn-cat (lex-class verb)
                        (is-copular +)))
              <-
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))))

(def-fcg-cxn cannot-cxn
             ((?cannot-unit
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (positive -)
                         (modal +)
                         (domain +))
                (meaning ((possible ?p)
                          (:polarity ?p -))))
              <-
              (?cannot-unit
               --
               (HASH form ((string ?cannot-unit "cannot"))))))

(def-fcg-cxn defaulted-cxn
             ((?defaulted-unit
               (referent ?d)
               (meaning ((default-01 ?d)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative +)
                        (transitive +)))
               <-
               (?defaulted-unit
                --
                (HASH form ((string ?defaulted-unit "defaulted"))))))

(def-fcg-cxn described-cxn
             ((?described-unit
               (referent ?d)
               (meaning ((describe-01 ?d)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
              <-
              (?described-unit
               --
               (HASH form ((string ?described-unit "described"))))))

(def-fcg-cxn destroyed-cxn
             ((?destroyed-unit
               (referent ?d)
               (meaning ((destroy-01 ?d)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
              <-
              (?destroyed-unit
               --
               (HASH form ((string ?destroyed-unit "destroyed"))))))

(def-fcg-cxn did-cxn
             ((?did-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)))
              <-
              (?did-unit
               --
               (HASH form ((string ?did-unit "did"))))))

(def-fcg-cxn doesnt-cxn
             ((?doesnt-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (positive +)))
              <-
              (?doesnt-unit
               --
               (HASH form ((string ?doesnt-unit "doesn't"))))))
             
(def-fcg-cxn feared-cxn
             ((?feared-unit
               (referent ?f)
               (meaning ((fear-01 ?f)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal -)
                        (ergative -)
                        (transitive +)
                        (simple-past +))
               (boundaries (leftmost-unit ?feared-leftmost-unit)
                           (rightmost-unit ?feared-rightmost-unit)))
               <-
               (?feared-unit
                --
                (HASH form ((string ?feared-unit "feared"))))))

(def-fcg-cxn find-cxn
             ((?find-unit
               (referent ?f)
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative -)
                        (transitive +))
               (meaning ((find-01 ?f))))
               <-
              (?find-unit
               --
               (HASH form ((string ?find-unit "find"))))))

(def-fcg-cxn go-cxn
             ((?go-unit
               (referent ?g)
               (syn-cat (lex-class verb)
                        (infinitive +))
                (meaning ((go-01 ?g))))
              <-
              (?go-unit
                --
                (HASH form ((string ?go-unit "go"))))))

(def-fcg-cxn had-cxn
             ((?had-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (light +)))
              <-
              (?had-unit
               --
               (HASH form ((string ?had-unit "had"))))))

(def-fcg-cxn has-cxn
             ((?has-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (arg1-preposition +)
                        (light +)))
              <-
              (?has-unit
               --
               (HASH form ((string ?has-unit "has"))))))

(def-fcg-cxn have-cxn
             ((?have-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (light +)))
              <-
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))))

(def-fcg-cxn have-to-cxn
             ((?have-to-unit
               (referent ?p)
               (syn-cat ((meaning (obligate-01 ?p)))
                        (lex-class verb)
                        (finite +)
                        (modal +))
               (meaning ((obligate-01 ?p)))
               (subunits (?to-unit ?have-unit))
               (boundaries (leftmost-unit ?have-unit)
                           (rightmost-unit ?to-unit)))
              <-
              (?to-unit
              --
              (form ((string ?to-unit "to"))))
              (?have-unit
               --
               (form ((string ?have-unit "have"))))
              (?have-to-unit
               --
               (HASH form ((meets ?have-unit ?to-unit))))))

(def-fcg-cxn hummed-cxn
             ((?hummed-unit
               (referent ?s)
               (meaning ((hum-02 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)))
              <-
              (?hummed-unit
               --
               (HASH form ((string ?hummed-unit "hummed"))))))

(def-fcg-cxn increased-cxn
             ((?increased-unit
               (referent ?i)
               (meaning ((increase-01 ?i)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative +)
                        (transitive +))
               (boundaries (leftmost-unit ?increased-unit)
                           (rightmost-unit ?increased-unit)))
              <-
              (?increased-unit
               --
               (HASH form ((string ?increased-unit "increased"))))))

(def-fcg-cxn is-copular-cxn
             ((?is-copular-unit
               (referent ?is)
               (syn-cat (lex-class verb)
                        (is-copular +)
                        (light +)
                        (arg1-preposition +)))
              <-
              (?is-copular-unit
               --
               (HASH form ((string ?is-copular-unit "is"))))))

(def-fcg-cxn isnt-cxn
             ((?isnt-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (polarity -)))
              <-
              (?isnt-unit
               --
               (HASH form ((string ?isnt-unit "isn't"))))))

(def-fcg-cxn looked-up-cxn
             ((?looked-up-unit
               (referent ?l)
               (meaning ((look-05 ?l)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
               <-
               (?looked-up-unit
                --
                (HASH form ((string ?looked-up-unit "looked"))))))

(def-fcg-cxn made-cxn
             ((?made-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (light +)))
              <-
              (?made-unit
               --
               (HASH form ((string ?made-unit "made"))))))

(def-fcg-cxn must-cxn
             ((?must-unit
               (referent ?p)
               (syn-cat (lex-class verb)
                        (finite +)
                        (polarity +)
                        (modal +))
               (meaning ((obligate-01 ?p)))
               (sem-valence (:arg2 ?arg2)))
              <-
              (?must-unit
               --
               (HASH form ((string ?must-unit "must"))))))

(def-fcg-cxn need-cxn
             ((?need-unit
               (referent ?p)
               (syn-cat (lex-class verb)
                        (finite +)
                        (modal +)
                        (polarity -))
               (sem-valence (:arg2 ?arg2))
               (meaning ((obligate-01 ?p))))
              <-
              (?need-unit
               --
               (HASH form ((string ?need-unit "need"))))))

(def-fcg-cxn obliged-cxn
             ((?obliged-unit
               (referent ?o)
               (syn-cat (lex-class verb)
                        (finite +)
                        (polarity -)
                        (modal +)
                        (to-infinitive +))
               (meaning ((obligate-01 ?p)))
               (sem-valence (:arg2 ?arg2)))
              <-
              (?obliged-unit
               --
               (HASH form ((string ?obliged-unit "obliged"))))))

(def-fcg-cxn obligatory-cxn
             ((?obligatory-unit
               (referent ?o)
               (syn-cat (lex-class verb)
                        (finite +)
                        (polarity +)
                        (modal +))
               (meaning ((obligate-01 ?p)))
               (sem-valence (:arg2 ?arg2)))
              <-
              (?obligatory-unit
               --
               (HASH form ((string ?obligatory-unit "obligatory"))))))

(def-fcg-cxn opined-cxn
             ((?opined-unit
               (referent ?o)
               (meaning ((opine-01 ?o)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative -)
                        (transitive +)))
               <-
               (?opined-unit
                --
                (HASH form ((string ?opined-unit "opined"))))))

(def-fcg-cxn pleasing-morph-cxn
             ((?pleasing-unit
               (referent ?p)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class verb)
                        (number ?sg)
                        (gerund +)
                        (syn-function ?nominal)
                        (transitive +)))
               <-
               (?pleasing-unit
                --
                (HASH form ((string ?pleasing-unit "pleasing"))))))

(def-fcg-cxn please-morph-cxn
             ((?please-unit
               (referent ?p)
               (meaning ((please-01 ?p)))
               (syn-cat (lex-class verb)
                        (infinitive +)
                        (transitive +)
                        (ergative +)))
               <-
               (?please-unit
                --
                (HASH form ((string ?please-unit "please"))))))

(def-fcg-cxn publishing-cxn
             ((?publishing-unit
               (referent ?p)
               (meaning ((publish-01 ?p)))
               (syn-cat (lex-class verb)
                        (finite +)))
               <-
               (?publishing-unit
                --
                (HASH form ((string ?publishing-unit "publishing"))))))

(def-fcg-cxn read-cxn
             ((?read-unit
               (referent ?r)
               (meaning ((read-01 ?r)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
               <-
               (?read-unit
                --
                (HASH form ((string ?read-unit "read"))))))

(def-fcg-cxn saw-cxn
             ((?saw-unit
               (referent ?s)
               (meaning ((see-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive +)))
               <-
               (?saw-unit
                --
                (HASH form ((string ?saw-unit "saw"))))))

(def-fcg-cxn sing-lex-cxn
             ((?sing-unit
               (referent ?s)
               (meaning ((sing-01 ?s)))
               (lex-id sing)
               (syn-cat (lex-class verb)))
               <-
               (?sing-unit
                --
               (lex-id sing))))

(def-fcg-cxn sang-morph-cxn
             ((?sang-unit
               (referent ?s)
               (lex-id sing)
               (meaning ((sing-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (simple-past +))
               (sem-valence (:arg0-of ?arg0-of)))
               <-
               (?sang-unit
                --
                (HASH form ((string ?sang-unit "sang"))))))

(def-fcg-cxn slay-lex-cxn
             ((?slay-unit
               (referent ?s)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)))
              <-
              (?slay-unit
               (lex-id slay))))

(def-fcg-cxn slew-morph-cxn
             ((?slew-unit
               (referent ?s)
               (lex-id slay)
               (meaning ((slay-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative -)
                        (transitive +)
                        (syn-function verbal)
                        (part-of-phrase +)
                        (present-participle -))
               (sem-valence (:arg0-of ?arg0-of))
               (boundaries (leftmost-unit ?slew-leftmost-unit)
                           (rightmost-unit ?slew-rightmost-unit)))
              <-
              (?slew-unit
               --
               (HASH form ((string ?slew-unit "slew"))))))

(def-fcg-cxn sued-cxn
             ((?sued-unit
               (referent ?s)
               (meaning ((sue-01 ?s)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (ergative +)
                        (past-participle +)))
               <-
               (?sued-unit
                --
                (HASH form ((string ?sued-unit "sued"))))))

(def-fcg-cxn thinks-cxn
             ((?thinks-unit
               (referent ?t-1)
               (meaning ((think-01 ?t-1)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (transitive -)))
               <-
               (?thinks-unit
                --
                (HASH form ((string ?thinks-unit "thinks"))))))

(def-fcg-cxn think-cxn
             ((?think-unit
               (referent ?t-1)
               (meaning ((think-01 ?t-1)))
               (syn-cat (lex-class verb)
                        (infinitive +)
                        (transitive -)))
               <-
               (?think-unit
                --
                (HASH form ((string ?think-unit "think"))))))

(def-fcg-cxn walked-cxn
             ((?walked-unit
               (referent ?w)
               (meaning ((walk-01 ?w)))
               (syn-cat (lex-class verb)
                        (finite +)))
              <-
              (?walked-unit
               --
               (HASH form ((string ?walked-unit "walked"))))))

(def-fcg-cxn want-lex-cxn
             ((?want-unit
               (referent ?w)
               (lex-id want)
               (syn-cat (lex-class verb))
               (meaning ((want-01 ?w))))
               <-
              (?want-unit
               --
               (lex-id want))))

(def-fcg-cxn wants-morph-cxn
             ((?wants-unit
               (referent ?w)
               (lex-id want)
               (syn-cat (lex-class verb)
                        (to-infinitive +)
                        (finite +)
                        (person 3)
                        (number sg))
               (meaning ((want-01 ?w)))
                (boundaries (leftmost-unit ?wants-leftmost-unit)
                            (rightmost-unit ?wants-rightmost-unit)))
               <-
              (?wants-unit
               --
               (HASH form ((string ?wants-unit "wants"))))))

(def-fcg-cxn was-cxn
             ((?was-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (light +)))
              <-
              (?was-unit
               --
               (HASH form ((string ?was-unit "was"))))))

(def-fcg-cxn was-located-cxn
             ((?was-located-unit
               (referent ?b)
               (meaning ((?be-located-at-91 ?b)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (light -)
                        (syn-function location)))
              <-
              (?was-located-unit
               --
               (HASH form ((string ?was-located-unit "was"))))))

(def-fcg-cxn win-cxn
             ((?win-unit
               (referent ?w)
               (meaning ((?win-01 ?w)))
               (syn-cat (lex-class verb)
                        (infinitive +)))
              <-
              (?win-unit
               --
               (HASH form ((string ?win-unit "win"))))))

(def-fcg-cxn will-cxn
             ((?will-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (positive +)))
              <-
              (?will-unit
               --
               (HASH form ((string ?will-unit "will"))))))

(def-fcg-cxn wont-cxn
             ((?wont-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (positive -)))
              <-
              (?wont-unit
               --
               (HASH form ((string ?wont-unit "won't"))))))

(def-fcg-cxn works-cxn
             ((?works-unit
               (referent ?w)
               (meaning ((work-01 ?w)))
               (syn-cat (lex-class verb)
                        (finite +)
                        (aux +)
                        (transitive -)
                        (part-of-phrase -)
                        (followed-by-an-adverb +))
               (boundaries (leftmost-unit ?works-leftmost-unit)
                           (rightmost-unit ?works-rightmost-unit)))
              <-
              (?works-unit
               --
               (HASH form ((string ?works-unit "works"))))))



;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Phrasal Constructions
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ---------------------------------------------------------------------------------------------------
;; Adjective Arg Constructions
;; ---------------------------------------------------------------------------------------------------
  
(def-fcg-cxn adjective-nominal-unit-cxn ;; arg0-of ;; match in sem quality
             ((?adjective-nominal-unit 
                (referent ?nominal)
                (meaning ((:arg0-of ?nominal ?quality)))
                (syn-cat (phrase-type nominal)
                         (syn-function nominal))
                (subunits (?adjective-unit ?nominal-unit))
                (boundaries (leftmost-unit ?adjective-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
              <-
              (?adjective-unit
               --
               (referent ?quality)
               (syn-cat (lex-class adjective)
                        (syn-function ?adjectival))
               (sem-cat (sem-class quality)))
              (?nominal-unit
               --
               (referent ?nominal)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
              (?adjective-nominal-unit 
               --
               (HASH form ((meets ?adjective-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn patient-nominal-cxn ;; arg1 of the adjective 
             ((?patient-of-nominal
               (referent ?person)
               (meaning ((:arg1 ?person ?nominal)))
               (sem-cat (sem-class ?class)
                        (sem-role patient))
               (syn-cat (phrase-type nominal-phrase)
                         (syn-function nominal)
                         (nominalisation +))
               (subunits (?second-nominal-unit ?nominal-unit))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?second-nominal-rightmost-unit)))
              <-
               (?nominal-unit
               --
               (referent ?nominal)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit))
               (sem-cat (sem-role patient)))
              (?second-nominal-unit
               --
              (referent ?person)
              (syn-cat (phrase-type nominal)
                       (syn-function nominal))
              (boundaries (leftmost-unit ?second-nominal-leftmost-unit)
                          (rightmost-unit ?second-nominal-rightmost-unit))
              (sem-cat (sem-class ?class)))
              (?patient-of-nominal
               --
               (HASH form ((meets ?second-nominal-rightmost-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn adjective-manner-cxn ;; :manner 
            ((?adjective-manner-unit
              (referent ?ref)
              (meaning ((:manner ?nominal ?type)))
               (syn-cat (syn-function ?func)
                        (nominalisation +)
                        (number ?numb))
               (subunits (?adjective-unit ?nominal-unit))
               (boundaries (leftmost-unit ?adjective-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
             <-
             (?adjective-unit
             --
             (referent ?type)
             (syn-cat (lex-class adjective)
                      (syn-function adjectival))
             (sem-cat (sem-class manner)))
             (?nominal-unit
              --
              (referent ?nominal)
              (syn-cat (phrase-type nominal)
                       (syn-function nominal))
              (boundaries (leftmost-unit ?nominal-leftmost-unit)
                          (rightmost-unit ?nominal-rightmost-unit)))
             (?adjective-manner-unit
              --
              (HASH form ((meets ?adjective-unit ?nominal-leftmost-unit))))))

 (def-fcg-cxn pertainym-adjective-nominal-cxn ;; mod
              ((?pertainym-adjective-nominal-unit
                (referent ?ref)
                (meaning ((:mod ?ref ?type)))
                (subunits (?type-unit ?nominal-unit))
                (boundaires (leftmost-unit ?type-unit)
                            (rightmost-unit ?nominal-rightmost-unit)))
                <-
                (?type-unit
                 --
                 (referent ?type)
                 (syn-cat (lex-class adjective)
                          (pertainym +)))
                (?nominal-unit
                 --
                 (referent ?ref)
                 (syn-cat (phrase-type nominal)
                          (syn-function nominal))
                 (boundaries (leftmost-unit ?nominal-leftmost-unit)
                             (rightmost-unit ?nominal-rightmost-unit)))
                (?pertainym-adjective-nominal-unit
                 --
                 (HASH form ((precedes ?type-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn pertainym-nominal-nominal-cxn ;; mod ;; not pertinym + since is a np
             ((?pertainym-nominal-nominal-unit
               (referent ?ref)
               (meaning ((:mod ?ref ?type)))
               (subunits (?type-unit ?nominal-unit))
               (boundaires (leftmost-unit ?type-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
              <-
              (?type-unit
               --
               (referent ?type)
               (syn-cat (phrase-type nominal)
                         (syn-function nominal))
               (sem-cat (sem-role pertainym))
               (boundaries (leftmost-unit ?type-leftmost-unit)
                           (rightmost-unit ?type-rightmost-unit)))
              (?nominal-unit
               --
               (referent ?ref)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
              (?pertainym-nominal-nominal-unit
               --
               (HASH form ((meets ?type-rightmost-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn arg1of-noun-cxn ;; arg1-of
             ((?arg1of-noun-unit
               (referent ?noun)
               (meaning ((:arg1-of ?noun ?adj)))
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (subunits (?adjective-unit ?nominal-unit))
               (boundaries (leftmost-unit ?adjective-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
              <-
              (?adjective-unit
               --
               (referent ?adj)
               (syn-cat (lex-class adjective)
                        (syn-function adjectival))
               (sem-cat (sem-class possibility)))
               (?nominal-unit
                --
                (referent ?noun)
                (syn-cat  (phrase-type nominal)
                          (syn-function nominal))
                (boundaries (leftmost-unit ?nominal-leftmost-unit)
                            (rightmost-unit ?nominal-rightmost-unit)))
               (?arg1of-noun-unit
                --
                (HASH form ((meets ?adjective-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn nominal-nominal-unit-cxn
             ((?nominal-nominal-unit
                (referent ?b)
                (meaning ((:arg1 ?i ?b)))
                (syn-cat (phrase-type nominal)
                         (syn-function nominal))
                (subunits (?nominal-1-unit ?nominal-2-unit))
                (boundaries (leftmost-unit ?nominal-2-unit)
                            (rightmost-unit ?nominal-1-rightmost-unit)))
              <-
              (?nominal-1-unit
               --
               (referent ?i)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-1-leftmost-unit)
                           (rightmost-unit ?nominal-1-rightmost-unit)))
              (?nominal-2-unit
               --
               (referent ?b)
               (syn-cat (lex-class adjectival-noun)
                        (pertainym -)
                        (syn-function adjectival)))
              (?nominal-nominal-unit
               --
               (HASH form ((meets ?nominal-2-unit ?nominal-1-leftmost-unit))))))

(def-fcg-cxn nominal-nominal-source-unit-cxn ;; source
             ((?nominal-nominal-source-unit 
                (referent ?b)
                (meaning ((:source ?b ?c)))
                (syn-cat (phrase-type nominal)
                         (syn-function nominal))
                (subunits (?nominal-1-unit ?nominal-2-unit))
                (boundaries (leftmost-unit ?nominal-1-unit)
                           (rightmost-unit ?nominal-2-rightmost-unit)))
              <-
              (?nominal-1-unit
               --
               (referent ?c)
               (syn-cat (lex-class adjectival-noun)
                        (syn-function adjectival))
               (sem-cat (sem-class age)))
              (?nominal-2-unit
               --
               (referent ?b)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-2-leftmost-unit)
                           (rightmost-unit ?nominal-2-rightmost-unit)))
              (?nominal-nominal-source-unit
               --
               (HASH form ((meets ?nominal-1-unit ?nominal-2-leftmost-unit))))))

(def-fcg-cxn y-of-x-cxn
             ((?y-of-x-unit
               (referent ?g)
               (meaning ((:arg0 ?o ?g)))
                (subunits (?np-x2-unit ?np-y2-unit ?of-preposition-unit))
                (boundaries (leftmost-unit ?np-x2-leftmost-unit)
                            (rightmost-unit ?np-y2-rightmost-unit)))
               <-
               (?np-x2-unit
               --
               (referent ?o)
               (syn-cat (phrase-type noun-phrase)
                        (part-of-phrase +))
               (boundaries
                (leftmost-unit ?np-x2-leftmost-unit)
                (rightmost-unit ?np-x2-rightmost-unit))
                (sem-cat (sem-role patient)))
               (?np-y2-unit
               --
               (referent ?g)
               (syn-cat (phrase-type noun-phrase))
               (sem-cat (sem-role agent))
               (boundaries
                (leftmost-unit ?np-y2-leftmost-unit)
                (rightmost-unit ?np-y2-rightmost-unit)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition))
                (form ((string ?of-preposition-unit "of"))))
               (?y-of-x-unit
                --
                (HASH form ((meets ?np-x2-rightmost-unit ?of-preposition-unit)
                            (meets ?of-preposition-unit ?np-y2-leftmost-unit))))))

(def-fcg-cxn x-of-y-possessive-cxn
             ((?x-of-y-possessive-unit
               (referent ?c)
               (syn-cat (phrase-type noun-phrase))
               (subunits (?np-unit ?proper-noun-y-unit ?of-preposition-unit))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?proper-noun-y-unit)))
                <-
               (?np-unit
               --
               (referent ?c)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?np-leftmost-unit)
                (rightmost-unit ?nprightmost-unit)))
               (?proper-noun-y-unit
               --
               (referent ?z)
               (syn-cat (lex-class proper-noun)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition))
                (form ((string ?of-preposition-unit "of"))))
               (?x-of-y-possessive-unit
                --
                (HASH form ((meets ?np-rightmost-unit ?of-preposition-unit)
                            (meets ?of-preposition-unit ?proper-noun-y-unit))))))

(def-fcg-cxn x-quantifier-of-y-cxn
             ((?x-quantifier-of-y-unit
               (referent ?n)
               (syn-cat (phrase-type noun-phrase))
               (meaning ((:quant-of ?n ?p)))
               (subunits (?np-x-unit ?np-y-unit ?of-preposition-unit))
               (boundaries (leftmost-unit ?np-x-leftmost-unit)
                            (rightmost-unit ?nominal-rightmost-unit)))
               <-
               (?np-x-unit
               --
               (referent ?n)
               (syn-cat (phrase-type noun-phrase)
                        (part-of-phrase +))
                (boundaries(leftmost-unit ?np-x-leftmost-unit)
                           (rightmost-unit ?np-x-rightmost-unit))
                (subunits (?nominal-unit)))
               (?nominal-unit
                --
                (syn-cat (phrase-type nominal))
                (subunits (?quantifier-unit)))
               (?quantifier-unit
                --
                (syn-cat (quantifier +)))
               (?np-y-unit
               --
               (referent ?p)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal)) 
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition))
                (form ((string ?of-preposition-unit "of"))))
               (?x-quantifier-of-y-unit
                --
                (HASH form ((meets ?np-x-rightmost-unit ?of-preposition-unit)
                            (meets ?of-preposition-unit ?nominal-leftmost-unit))))))
 
(def-fcg-cxn x-s-y-nominal-cxn
             ((?x-s-y-unit
               (referent ?o)
               (meaning ((:arg0 ?o ?g)))
               (syn-cat (phrase-type noun-phrase)
                         (syn-function patient)
                         (main-clause +))
               (subunits (?np-x-unit ?nominal-y-unit ?possessive-unit))
               (boundaries (leftmost-unit ?np-x-leftmost-unit)
                           (rightmost-unit ?nominal-y-rightmost-unit)))
              <-
              (?nominal-y-unit
               --
               (referent ?o)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-y-leftmost-unit)
                           (rightmost-unit ?nominal-y-leftmost-unit)))
              (?np-x-unit
               --
               (referent ?g)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?np-x-leftmost-unit)
                (rightmost-unit ?np-x-rightmost-unit)))
              (?possessive-unit
               --
               (form ((string ?possessive-unit "'s")))
               (syn-cat (syn-function possessive-form)))
              (?x-s-y-unit
               --
               (HASH form ((meets ?np-x-rightmost-unit ?possessive-unit)
                           (meets ?possessive-unit ?nominal-y-leftmost-unit))))))
             
(def-fcg-cxn y-of-x-arg1-cxn
             ((?y-of-x-arg1-unit
               (referent ?d)
               (meaning ((:arg1 ?d ?r)))
               (syn-cat (main-clause +))
               (subunits (?np-x-unit ?np-y-unit ?of-preposition-unit))
                (boundaries (rightmost-unit ?np-y-rightmost-unit)
                            (leftmost-unit ?np-x-leftmost-unit)))
               <-
               (?np-x-unit
               --
               (referent ?d)
               (syn-cat (phrase-type noun-phrase)
                        (syn-function patient))
               (boundaries
                (leftmost-unit ?np-x-leftmost-unit)
                (rightmost-unit ?np-x-rightmost-unit)))
               (?nominal-unit
               --
               (syn-cat (phrase-type nominal))
               (subunits (?noun-unit)))
               (?noun-unit
               --
               (syn-cat (lex-class noun)
                        (syn-function patient)))
               (?np-y-unit
               --
               (referent ?r)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?np-y-leftmost-unit)
                (rightmost-unit ?np-y-rightmost-unit)))
               (?of-preposition-unit
                --
               (syn-cat (lex-class preposition)
                        (syn-function specification)))
               (?y-of-x-arg1-unit
                --
                (HASH form ((meets ?np-x-rightmost-unit ?of-preposition-unit)
                            (meets ?of-preposition-unit ?np-y-leftmost-unit))))))

;; ---------------------------------------------------------------------------------------------------
;; Proper Nouns Constructions 
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn proper-noun-entity-cxn ;; President Obama
             ((?named-entity-unit
               (referent ?p)
               (meaning ((:name ?p ?n)))
               (subunits (?nominal-unit-1 ?nominal-unit-2))
               (syn-cat (phrase-type noun-phrase)
                        (named-entity-type person))
               (boundaries (leftmost-unit ?nominal-1-leftmost-unit)
                           (rightmost-unit ?nominal-unit-2)))
              <-
              (?nominal-unit-1
               --
               (referent ?p)
               (syn-cat (phrase-type nominal)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?nominal-1-leftmost-unit)
                           (rightmost-unit ?nominal-1-rightmost-unit)))
              (?nominal-unit-2
               --
               (referent ?n)
               (syn-cat (syn-function nominal)
                        (proper-noun +)))
              (?named-entity-unit
               --
               (HASH form ((meets ?nominal-1-rightmost-unit ?nominal-unit-2))))))

 (def-fcg-cxn named-entity-title-article-person-cxn ;; Obama the president 
              ((?named-entity-article-person-unit
                (referent ?p)
                (meaning ((:name ?p ?n)))
                (subunits (?nominal-unit-1 ?noun-phrase))
                (syn-cat (phrase-type noun-phrase)
                         (named-entity-type person))
                (sem-cat (sem-class ?class))
                (boundaries (leftmost-unit ?nominal-unit-1)
                            (rightmost-unit ?noun-phrase-rightmost-unit)))
               <-
               (?nominal-unit-1
                --
                (referent ?n)
                 (syn-cat (proper-noun +)
                          (syn-function nominal))
                 (sem-cat (sem-class person)))
                (?noun-phrase
                --
                (referent ?p)
                (syn-cat (phrase-type noun-phrase)
                         (part-of-phrase +))
                (boundaries (leftmost-unit ?noun-phrase-leftmost-unit)
                            (rightmost-unit ?noun-phrase-rightmost-unit)))
               (?named-entity-article-person-unit
                --
                (HASH form ((meets ?nominal-unit-1 ??noun-phrase-leftmost-unit))))))
;; ---------------------------------------------------------------------------------------------------
;; Noun Phrase Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn noun-article-phrase-cxn
             ((?noun-phrase-article-unit
               (referent ?nominal)
               (subunits (?article-unit ?nominal-unit))
               (syn-cat (phrase-type noun-phrase)
                         (part-of-phrase +)
                         (syn-function ?function))
               (boundaries (leftmost-unit ?article-unit)
                           (rightmost-unit ?nominal-rightmost-unit))
               (sem-cat (sem-role ?role)))
               <-
               (?article-unit
               --
               (syn-cat (lex-class article)
                        (definite ?def)
                        (number ?numb)))
               (?nominal-unit
               --
               (referent ?nominal)
               (syn-cat (syn-function nominal)
                        (phrase-type nominal))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
               (?noun-phrase-article-unit
                --
                (HASH form ((meets ?article-unit ?nominal-leftmost-unit))))))
                        
(def-fcg-cxn nominal-cxn
             ((?nominal-unit
               (referent ?ref)
               (syn-cat (phrase-type nominal)
                        (syn-function ?nominal))
               (sem-cat (sem-role ?role))
               (subunits (?noun-unit))
               (boundaries (rightmost-unit ?noun-unit)
                           (leftmost-unit ?noun-unit)))
               <-
                (?noun-unit
                --
                (referent ?ref)
                (syn-cat (lex-class noun)))))

;; ---------------------------------------------------------------------------------------------------
;; Prepositional Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn in-location-cxn
             ((?in-location-unit
               (referent ?m)
               (meaning ((:location ?m ?j)))
               (subunits (?np-unit ?in-unit ?location-unit))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?location-rightmost-unit)))
               <-
               (?np-unit
                --
               (referent ?m)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit)))
               (?in-unit
                --
                (syn-cat (lex-class preposition))
                (form ((string ?in-unit "in"))))
               (?location-unit
                --
                (referent ?j)
                (syn-cat (phrase-type noun-phrase)
                         (syn-function location))
                (boundaries (leftmost-unit ?location-leftmost-unit)
                           (rightmost-unit ?location-rightmost-unit)))
               (?in-location-unit
                --
                (HASH form ((meets ?np-rightmost-unit ?in-unit)
                            (meets ?in-unit ?location-leftmost-unit))))))

(def-fcg-cxn temporal-clause-cxn
             ((?temporal-clause-unit
               (referent ?d)
               (meaning ((:time ?d ?d2)))
               (subunits (?vp-unit ?in-unit ?time-unit))
               (boundaries (leftmost-unit ?vp-unit)
                           (rightmost-unit ?time-unit)))
               <-
               (?vp-unit
                --
               (referent ?d)
               (syn-cat (phrase-type verb-phrase)
                        (infinitive-clause ?inf))
               (boundaries (leftmost-unit ?vp-leftmost-unit)
                           (rightmost-unit ?vp-rightmost-unit)))
               (?in-unit
                --
                (syn-cat (lex-class preposition))
                (form ((string ?in-unit "in"))))
               (?time-unit
                --
                (referent ?d2)
                (syn-cat (phrase-type nominal)
                         (syn-function nominal))
                (sem-cat (sem-role date-entity))
                (boundaries (leftmost-unit ?time-leftmost-unit)
                           (rightmost-unit ?time-rightmost-unit)))
               (?temporal-clause-unit
                --
                (HASH form ((meets ?vp-rightmost ?in-unit)
                            (meets ?in-unit ?time-leftmost-unit))))))
  
;; ---------------------------------------------------------------------------------------------------
;; Verbal Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn verb-phrase-cxn
             ((?vp-unit
               (referent ?ref)
               (syn-cat (syn-function verbal)
                        (phrase-type verb-phrase)
                        (transitive ?trans)
                        (ergative -)
                        (passive -)
                        (part-of-phrase -)
                        (infinitive-clause -))
               (subunits (?finite-verb))
               (boundaries (rightmost-unit ?finite-verb)
                           (leftmost-unit ?finite-verb)))
               <-
                (?finite-verb
                --
                (referent ?ref)
                (syn-cat (lex-class verb)
                         (finite ?+)
                         (NOT (aux +))
                         (NOT (ergative +))
                         (NOT (modal +))
                         (NOT (light +))))))

(def-fcg-cxn modal-verb-phrase-cxn
             ((?vp-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type verb-phrase)
                        (number ?n)
                        (person ?p)
                        (syn-function verbal)
                        (part-of-phrase +)
                        (infinitive-clause -))
               (meaning ((:domain ?ref-aux ?ref-inf)))
               (subunits (?aux ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (modal +)
                         (positive +)
                         (domain +)))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((precedes ?aux ?infinitive-verb))))))

(def-fcg-cxn modal-verb-phrase-negative-cxn
             ((?vp-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type verb-phrase)
                        (number ?n)
                        (person ?p)
                        (syn-function verbal)
                        (part-of-phrase +)
                        (passive -)
                         (infinitive-clause -))
               (meaning ((:domain ?ref-aux ?ref-inf)))
               (subunits (?aux ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (modal +)
                         (positive -)
                         (domain +)))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((precedes ?aux ?infinitive-verb))))))

(def-fcg-cxn negative-aux-modal-cxn
             ((?aux-modal-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type verb-phrase)
                        (passive -)
                        (infinitive-clause -))
               (meaning ((:polarity ?p -)
                         (:arg2 ?p ?ref-inf)))
               (subunits (?aux-unit ?modal-unit ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux-unit
                --
                (referent ?ref-aux)
                (syn-cat (aux +)))
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (modal +))
                (boundaries (leftmost-unit ?modal-leftmost-unit)
                            (rightmost-unit ?modal-rightmost-unit)))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?aux-modal-unit
                --
                (HASH form ((meets ?aux ?modal-leftmost-unit)
                            (meets ?modal-rightmost-unit ?infinitive-verb))))))

(def-fcg-cxn aux-VP-positive-cxn
             ((?vp-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type verb-phrase)
                        (passive -)
                         (infinitive-clause -))
               (subunits (?aux ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (aux +)
                         (positive +)))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((meets ?aux ?infinitive-verb))))))

(def-fcg-cxn aux-VP-negative-notcontracted-cxn
             ((?aux-VP-negative-notcontracted-unit
               (referent ?w)
               (syn-cat (phrase-type verb-phrase)
                        (passive -))
               (meaning ((:polarity ?w -)))
               (subunits (?aux ?infinitive-verb))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (aux +)
                         (positive -)))
                (?infinitive-verb
                --
                (referent ?w)
                (syn-cat (infinitive +)))
                (?aux-VP-negative-notcontracted-unit
                --
                (HASH form ((meets ?aux ?infinitive-verb))))))

(def-fcg-cxn aux-VP-negative-cxn
             ((?vp-unit
               (referent ?ref-inf)
               (syn-cat (phrase-type verb-phrase)
                        (number ?n)
                        (person ?p)
                        (syn-function verbal)
                        (part-of-phrase +)
                        (passive -)
                        (infinitive-clause -))
               (meaning ((:polarity ?ref-inf -)))
               (subunits (?aux ?infinitive-verb ?not-unit))
               (boundaries (rightmost-unit ?infinitive-verb)
                           (leftmost-unit ?aux)))
               <-
               (?aux
                --
                (referent ?ref-aux)
                (syn-cat (aux +)))
               (?not-unit
                --
               (syn-cat (lex-class adverb))
               (form ((string ?not-unit "not"))))
                (?infinitive-verb
                --
                (referent ?ref-inf)
                (syn-cat (infinitive +)))
                (?vp-unit
                --
                (HASH form ((precedes ?aux ?infinitive-verb))))))

(def-fcg-cxn passive-cxn
             ((?passive-unit
               (referent ?event)
               (syn-cat (phrase-type verb-phrase)
                        (passive +))
               (subunits (?passive-aux-verb-unit ?past-participle-unit))
               (boundaries (leftmost-unit ?passive-aux-verb-unit)
                           (rightmost-unit ?past-participle-unit)))
               <-
               (?passive-aux-verb-unit
               --
               (referent ?light)
               (syn-cat (aux +)))
               (?past-participle-unit
               --
               (referent ?event)
               (syn-cat (lex-class verb)
                        (past-participle +)))
               (?passive-unit
               --
               (HASH form ((meets ?passive-aux-verb-unit ?past-participle-unit))))))

(def-fcg-cxn light-verb-cxn
             ((?vp-unit
               (referent ?event)
               (syn-cat (lex-class verb)
                         (preposition +)
                         (passive -)
                         (arg2 -))
               (subunits (?light-verb-unit ?nominal-unit))
               (boundaries (leftmost-unit ?light-verb-unit)
                           (rightmost-unit ?nominal-unit)))
               <-
               (?light-verb-unit
               --
               (referent ?light)
               (syn-cat (lex-class verb)
                        (light +)))
               (?nominal-unit
               --
               (referent ?event)
               (syn-cat (syn-function verbal)
                        (arg2 -)))
               (?vp-unit
               --
               (HASH form ((meets ?light-verb-unit ?nominal-unit))))))

(def-fcg-cxn light-verb-np-cxn
             ((?light-verb-np-unit
               (referent ?event)
               (syn-cat (lex-class verb)
                         (preposition +)
                         (passive -)
                         (arg2 -))
               (subunits (?light-verb-unit ?nominal-unit))
               (boundaries (leftmost-unit ?light-verb-unit)
                           (rightmost-unit ?np-rightmost-unit)))
               <-
               (?light-verb-unit
               --
               (referent ?light)
               (syn-cat (lex-class verb)
                        (light +)))
               (?np-unit
               --
               (referent ?event)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit)))
               (?nominal-unit
               --
               (subunits (?noun-unit)))
               (?noun-unit
               --         
               (syn-cat (syn-function verbal)
                        (arg2 -)))
               (?light-verb-np-unit
               --
               (HASH form ((meets ?light-verb-unit ?np-leftmost-unit))))))

(def-fcg-cxn vp-preposition-cxn
             ((?vp-preposition-x-unit
               (referent ?f)
                (meaning ((:arg1 ?f ?b)))
                (syn-cat (phrase-type verb-phrase)
                         (passive -))
                (sem-valence (:arg0-of ?arg0-of))
                (subunits (?vp-unit ?preposition-unit ?arg1-unit))
                (boundaries (leftmost-unit ?vp-leftmost-unit)
                            (rightmost-unit ?arg1-rightmost-unit)))
                <-
                (?vp-unit
                --
                (referent ?f)
                (syn-cat (lex-class verb)
                         (preposition +))
                (boundaries (rightmost-unit ?vp-rightmost-unit)
                            (leftmost-unit ?vp-leftmost-unit)))
                (?preposition-unit
                 --
                 (referent ?prep)
                 (syn-cat (lex-class preposition)))
                (?arg1-unit
                --
                (referent ?b)
                (syn-cat (phrase-type ?noun-phrase))
                (boundaries (rightmost-unit ?arg1-rightmost-unit)
                            (leftmost-unit ?arg1-leftmost-unit)))
                (?vp-preposition-x-unit
                --
                (HASH form ((meets ?vp-rightmost-unit ?preposition-unit)
                            (meets ?preposition-unit ?arg1-leftmost-unit))))))

(def-fcg-cxn vp-preposition-arg2-cxn
             ((?vp-preposition-arg2-unit
               (referent ?r)
                (meaning ((:arg2 ?r ?w)))
                (syn-cat (phrase-type noun-phrase)
                         (syn-function patient))
                (subunits (?nominal-unit ?preposition-unit ?arg2-unit))
                (boundaries (leftmost-unit ?nominal-leftmost-unit)
                            (rightmost-unit ?arg2-rightmost-unit)))
                <-
                (?nominal-unit
                --
                (referent ?r)
                (syn-cat (phrase-type nominal))
                (boundaries (rightmost-unit ?nominal-rightmost-unit)
                            (leftmost-unit ?nominal-leftmost-unit)))
                (?preposition-unit
                 --
                 (syn-cat (lex-class preposition))
                 (form ((string ?preposition-unit "for"))))
                (?arg2-unit
                --
                (referent ?w)
                (syn-cat (phrase-type noun-phrase))
                (boundaries (rightmost-unit ?arg2-rightmost-unit)
                            (leftmost-unit ?arg2-leftmost-unit)))
                (?vp-preposition-arg2-unit
                --
                (HASH form ((meets ?nominal-rightmost-unit ?preposition-unit)
                            (meets ?preposition-unit ?arg2-leftmost-unit))))))

(def-fcg-cxn copula-predicative-adjective-cxn
             ((?copula-predicative-adjective-unit
               (referent ?a)
               (syn-cat (transitive +)
                        (preposition +)
                        (lex-class verb)
                        (passive -))
               (sem-valence (:arg0-of ?arg0-of))
               (subunits (?copula-unit ?adjective-unit))
               (boundaries (leftmost-unit ?copula-unit)
                           (rightmost-unit ?adjective-unit)))
               <-
               (?copula-unit
               --
               (referent ?is)
               (syn-cat (lex-class verb)
                        (is-copular +)))
               (?adjective-unit
               --
               (referent ?a)
               (syn-cat (lex-class adjective)
                        (arg0-of +)))
               (?copula-predicative-adjective-unit
               --
               (HASH form ((meets ?copula-unit ?adjective-unit))))))
;; ---------------------------------------------------------------------------------------------------
;; Adverbial Phrases Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn adverbial-manner-clause-cxn
             ((?adverb-manner-clause-unit
               (meaning ((:manner ?main ?adv)))
               (subunits (?vp-unit ?adverb-unit))
               (referent ?main)
               (syn-cat (syn-function verbal)
                        (part-of-phrase +)
                        (infinitive-clause -)
                        (passive -)
                        (phrase-type verb-phrase))
               (boundaries
                 (rightmost-unit ?adverb-unit)
                 (leftmost-unit ?vp-unit-leftmost)))
               <-
               (?vp-unit
                --
                (referent ?main)
                (syn-cat (part-of-phrase -)
                         (followed-by-an-adverb +))
                (boundaries (leftmost-unit ?vp-leftmost-unit)
                            (rightmost-unit ?vp-rightmost-unit)))
               (?adverb-unit
               --
               (referent ?adv)
                (syn-cat (syn-function adverbial))
               (sem-cat (sem-class manner)))
               (?adverb-manner-clause-unit
                --
                (HASH form ((precedes ?vp-rightmost-unit ?adverb-unit))))))

(def-fcg-cxn adverb-manner-noun-cxn
           ((?adverbial-manner-unit
             (subunits (?adverb-unit ?nominal-unit))
             (referent ?w)
             (syn-cat (phrase-type nominal)
                      (syn-function nominal))
             (sem-cat (sem-class manner))
             (meaning ((:manner ?w ?h)))
             (boundaries (rightmost-unit ?nominal-rightmost-unit)
                         (leftmost-unit ?adverb-unit)))
             <-
             (?adverb-unit
             --
             (referent ?h)
             (syn-cat (lex-class adverb)))
             (?nominal-unit
             --
             (referent ?w)
             (syn-cat (phrase-type nominal))
             (boundaries (rightmost-unit ?nominal-rightmost-unit)
                         (leftmost-unit ?nominal-leftmost-unit)))
             (?adverbial-manner-unit
             --
             (HASH form ((meets ?adverb-unit ?nominal-leftmost-unit))))))

(def-fcg-cxn x-s-y-adverb-cxn
             ((?x-s-y-adverb-unit
               (referent ?m)
               (meaning ((:time ?m ?y)))
               (syn-cat (phrase-type noun-phrase))
               (subunits (?adverb-x-unit ?nominal-y-unit ?possessive-unit))
               (boundaries (leftmost-unit ?adverb-x-unit)
                           (rightmost-unit ?nominal-x-rightmost-unit)))
              <-
              (?adverb-x-unit
               --
               (referent ?y)
               (syn-cat (lex-class adverb)))
              (?nominal-y-unit
               --
               (referent ?m)
               (syn-cat (phrase-type nominal))
               (boundaries
                (leftmost-unit ?nominal-y-leftmost-unit)
                (rightmost-unit ?nominal-y-rightmost-unit)))
              (?possessive-unit
               --
               (form ((string ?possessive-unit "'s")))
               (syn-cat (syn-function possessive-form)))
              (?x-s-y-adverb-unit
               --
               (HASH form ((meets ?adverb-x-unit ?possessive-unit)
                           (meets ?possessive-unit ?nominal-y-leftmost-unit))))))
                        
;; ---------------------------------------------------------------------------------------------------
;; Arg Structure Constructions
;; ---------------------------------------------------------------------------------------------------
(def-fcg-cxn np-vp-np=arg0-finiteverb-cxn ;; active-intransitive-cxn
             ((?clause-unit
               (meaning ((:arg0 ?verb ?arg0)))
               (subunits (?vp-unit ?agent-unit))
               (referent ?verb)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?agent-leftmost-unit))
               (syn-cat (phrase-type verb-phrase)
                        (syn-function main-clause)
                        (main-clause +)))
              <-
              (?vp-unit
               --
               (referent ?verb)
               (syn-cat (phrase-type verb-phrase)
                        (passive -))
               (boundaries
                (leftmost-unit ?vp-leftmost-unit)
                (rightmost-unit ?vp-rightmost-unit)))
              (?agent-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?agent-leftmost-unit)
                (rightmost-unit ?agent-rightmost-unit)))
              (?clause-unit
               --
               (HASH form ((meets ?agent-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn np-vp-np=arg0-infinitive-clause-cxn ;; active-intransitive-cxn
             ((?clause-unit
               (meaning ((:arg0 ?verb ?arg0)))
               (subunits (?vp-infinitive-unit ?agent-unit))
               (referent ?verb)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?agent-leftmost-unit))
               (syn-cat (phrase-type verb-phrase)))
              <-
              (?vp-infinitive-unit
               --
               (referent ?verb)
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase +)
                        (infinitive-clause +))
               (meaning ((:arg0 ?go ?arg0)))
               (boundaries
                (leftmost-unit ?vp-infinitive-leftmost-unit)
                (rightmost-unit ?vp-infinitive-rightmost-unit)))
              (?agent-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type noun-phrase))
               (boundaries
                (leftmost-unit ?agent-leftmost-unit)
                (rightmost-unit ?agent-rightmost-unit))
               (sem-cat (sem-role agent)))
              (?clause-unit
               --
               (HASH form ((meets ?agent-rightmost-unit ?vp-infinitive-leftmost-unit))))))

(def-fcg-cxn AP-NP-np=arg0-cxn
             ((?adverbialclause-unit
               (meaning ((:arg0 ?main ?arg0)))
               (subunits (?vp-unit ?arg0-unit))
               (referent ?g)
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?arg0-leftmost-unit))
               (syn-cat (phrase-type verb-phrase)))
              <-
              (?ap-unit
               --
               (referent ?main)
               (syn-cat (phrase-type AP))
               (boundaries
                (leftmost-unit ?ap-leftmost-unit)
                (rightmost-unit ?ap-rightmost-unit)))
              (?arg0-unit
               --
               (referent ?arg0)
               (syn-cat (phrase-type NP)
                        (number sg)
                        (person 3)
                        (phrase-type +))
               (boundaries
                (leftmost-unit ?subject-leftmost-unit)
                (rightmost-unit ?subject-rightmost-unit)))
              (?adverbialclause-unit
               --
               (HASH form ((meets ?subject-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn arg2-modal-negativeinfinitive-cxn
             ((?arg2-modal-negativeinfinitive-unit
               (referent ?g)
               (subunits (?modal-unit ?not-unit ?infinitive-unit))
               (meaning ((:arg2 ?p ?g)
                         (:polarity ?g -)))
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase +)
                        (infinitive-clause ?inf)
                        (passive -))
               (boundaries (rightmost-unit ?infinitive-unit)
                           (leftmost-unit ?modal-unit)))
               <-
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (polarity +)
                         (modal +))
                (sem-valence (:arg2 ?arg2)))
               (?not-unit
                --
               (syn-cat (lex-class adverb))
               (form ((string ?not-unit "not"))))
               (?infinitive-unit
                --
                (referent ?g)
                (syn-cat (lex-class verb)
                         (infinitive +)))
               (?arg2-modal-negativeinfinitive-unit
                --
                (HASH form ((meets ?modal-unit ?not-unit)
                            (meets ?not-unit ?infinitive-unit))))))

(def-fcg-cxn arg2-modalnegative-infinitive-cxn
             ((?arg2-modalnegative-infinitive-unit
               (referent ?g)
               (subunits (?modal-unit ?not-unit ?infinitive-unit))
               (meaning ((:arg2 ?p ?g)
                         (:polarity ?p -)))
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase +)
                        (infinitive-clause ?inf)
                        (passive -))
               (boundaries (rightmost-unit ?infinitive-unit)
                           (leftmost-unit ?modal-unit)))
               <-
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (polarity -)
                         (modal +))
                (sem-valence (:arg2 ?arg2)))
               (?not-unit
                --
               (syn-cat (polarity -)))
               (?infinitive-unit
                --
                (referent ?g)
                (syn-cat (lex-class verb)
                         (infinitive +)))
               (?arg2-modalnegative-infinitive-unit
                --
                (HASH form ((meets ?modal-unit ?not-unit)
                            (meets ?not-unit ?infinitive-unit))))))

(def-fcg-cxn different-order-arg2-modalnegative-infinitive-cxn
             ((?different-order-arg2-modalnegative-infinitive-unit
               (referent ?g)
               (subunits (?modal-unit ?not-unit ?infinitive-unit))
               (meaning ((:arg2 ?p ?g)
                         (:polarity ?p -)))
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase +)
                        (infinitive-clause ?inf)
                        (passive -))
               (boundaries (rightmost-unit ?infinitive-unit)
                           (leftmost-unit ?not-unit)))
               <-
               (?modal-unit
                --
                (referent ?p)
                (syn-cat (lex-class verb)
                         (finite +)
                         (polarity -)
                         (modal +))
                (sem-valence (:arg2 ?arg2)))
               (?not-unit
                --
               (syn-cat (polarity -)))
               (?infinitive-unit
                --
                (referent ?g)
                (syn-cat (lex-class verb)
                         (infinitive +)))
               (?different-order-arg2-modalnegative-infinitive-unit
                --
                (HASH form ((meets ?not-unit ?modal-unit)
                            (meets ?modal-unit ?infinitive-unit))))))

(def-fcg-cxn predicative-domain-np-cxn 
             ((?predicative-clause
               (subunits (?vp-unit ?predicative-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?domain ?referring)))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?predicative-rightmost-unit))
               (referent ?referring))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?predicative-unit
               --
               (referent ?domain)
               (syn-cat (syn-function predicative))
               (boundaries (leftmost-unit ?predicative-leftmost-unit)
                           (rightmost-unit ?predicative-rightmost-unit)))
               (?nominal-unit
               --
               (syn-cat (phrase-type nominal))
               (subunits (?noun-unit)))
               --
               (?noun-unit
               --
               (syn-cat (derived-from-verb -)))
               (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (phrase-type ?noun-phrase)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?referring-noun-rightmost-unit)))
              (?predicative-clause
               --
               (HASH form ((meets ?referring-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?predicative-leftmost-unit))))))

(def-fcg-cxn predicative-domain-adjective-cxn 
             ((?predicative-domain-nominal-unit
               (subunits (?vp-unit ?predicative-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?domain ?referring)))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?predicative-unit))
               (referent ?referring))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?predicative-unit
               --
               (referent ?domain)
               (syn-cat (lex-class adjective)
                        (derived-from-verb -)
                        (syn-function predicative)))
               (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (phrase-type ?noun-phrase)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?referring-noun-rightmost-unit)))
              (?predicative-domain-nominal-unit
               --
               (HASH form ((meets ?referring-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?predicative-unit))))))


(def-fcg-cxn predicative-arg0-cxn 
             ((?predicative-arg0-clause
               (subunits (?vp-unit ?verbalised-noun-unit ?arg0-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:arg0 ?w ?b)))
               (boundaries (leftmost-unit ?arg0-leftmost-unit)
                           (rightmost-unit ?verbalised-noun-rightmost-unit))
               (referent ?b))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?verbalised-noun-unit
               --
               (referent ?w)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?verbalised-noun-leftmost-unit)
                           (rightmost-unit ?verbalised-noun-rightmost-unit)))
              (?nominal-unit
               --
               (syn-cat (phrase-type nominal))
               (subunits (?noun-unit)))
               --
               (?noun-unit
               --
               (syn-cat (lex-class noun)
                       (derived-from-verb +)))
               (?arg0-unit
               --
               (referent ?b)
               (syn-cat (phrase-type ?noun-phrase)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?arg0-leftmost-unit)
                           (rightmost-unit ?arg0-noun-rightmost-unit)))
              (?predicative-arg0-clause
               --
               (HASH form ((meets ?arg0-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?verbalised-noun-leftmost-unit))))))

(def-fcg-cxn predicative-negative-cxn 
             ((?predicative-negative-clause-unit
               (subunits (?vp-unit ?predicative-unit ?not-unit ?referring-noun-unit))
               (syn-cat (phrase-type clausal))
               (meaning ((:domain ?domain ?referring)
                         (:polarity ?domain -)))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?predicative-rightmost-unit))
               (referent ?referring))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?predicative-unit
               --
               (referent ?domain)
               (syn-cat (derived-from-verb -)
                        (lex-class adjective)))
              (?not-unit
               --
               (syn-cat (lex-class adverb))
               (form ((string ?not-unit "not"))))
               (?referring-noun-unit
               --
               (referent ?referring)
               (syn-cat (phrase-type noun-phrase)
                        (part-of-phrase +)
                        (syn-function nominal))
               (boundaries (leftmost-unit ?referring-noun-leftmost-unit)
                           (rightmost-unit ?referring-noun-rightmost-unit)))
              (?predicative-negative-clause-unit
               --
               (HASH form ((meets ?referring-noun-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?not-unit)
                           (meets ?not-unit ?predicative-leftmost-unit))))))

(def-fcg-cxn predicative-gerund-referring-entity-cxn 
             ((?predicative-gerund-referring-entity
               (subunits (?vp-unit ?adjective-predicative-unit ?gerund-unit))
               (meaning ((:domain ?adj ?ger)))
               (referent ?ger))
              <-
              (?vp-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?adjective-predicative-unit
               --
               (referent ?adj)
               (syn-cat (lex-class adjective)
                        (syn-function predicative)))
              (?gerund-unit
               --
               (referent ?ger)
               (syn-cat (gerund +)))
              (?predicative-gerund-referring-entity
               --
               (HASH form ((meets ?gerund-rightmost-unit ?vp-unit )
                           (meets ?vp-unit ?adjective-predicative-unit))))))

(def-fcg-cxn predicative-adjective-to-infinitive-cxn
             ((?predicative-adjective-to-infinitive-unit
              (meaning ((:domain ?t ?p)
                        (:arg1 ?p ?g)))
              (subunits (?patient-unit ?copula-unit ?adjective-unit ?to-unit ?infinitive-unit))
              (referent ?p)
              (boundaries
               (leftmost-unit ?patient-leftmost-unit)
               (rightmost-unit ?infinitive-unit)))
              <-
              (?patient-unit
              --
              (referent ?g)
              (syn-cat (phrase-type nominal))
              (boundaries
                (leftmost-unit ?patient-unit-leftmost-unit)
                (rightmost-unit ?patient-unit-rightmost-unit)))
              (?copula-unit
               --
               (syn-cat (lex-class verb)
                        (is-copular +)))
              (?adjective-unit
              --
              (referent ?t)
              (syn-cat (lex-class adjective)
                       (syn-function predicative)))
              (?to-unit
               --
               (syn-cat (lex-class preposition))
               (form ((string ?to-unit "to"))))
              (?infinitive-unit
              --
              (referent ?p)
              (syn-cat (lex-class verb)
                        (infinitive +)))
              (?predicative-adjective-to-infinitive-unit
              --
              (HASH form ((precedes ?patient-leftmost-unit ?copula-unit)
                          (precedes ?copula-unit ?adjective-unit)
                          (precedes ?adjective-unit ?to-unit)
                          (precedes ?to-unit ?infinitive-unit))))))

(def-fcg-cxn arg1-present-participle-cxn
             ((?arg1-present-participle-unit
               (referent ?o)
               (meaning ((:arg1 ?s ?o)))
               (syn-cat (syn-function ?arg1))
               (subunits (?present-participle-unit ?arg1-NP-unit))
               (boundaries (?arg1-NP-unit-rightmost-unit ?present-participle-unit)))
               <-
               (?present-participle-unit
                (referent ?s)
                --
                (syn-cat (lex-class verb)
                         (modal -)
                         (present-participle +)
                         (phrase-type verb-phrase))
                         (syn-function ?func))
               (?arg1-NP-unit
               (referent ?o)
                --
               (syn-cat (phrase-type noun-phrase)
                        (number sg)
                        (person 3)
                        (syn-function ?func))
               (boundaries
                (leftmost-unit ?arg1-NP-unit-leftmost-unit)
                (rightmost-unit ?arg1-NP-unit-rightmost-unit))
                (sem-cat (sem-class arg1)))
               (?arg1-present-participle-unit
                --
                (HASH form ((precedes ?arg1-NP-unit-rightmost-unit ?present-participle-unit))))))

(def-fcg-cxn active-transitive-cxn 
             ((?active-transitive-unit
               (subunits (?vp-unit ?direct-object-unit))
               (meaning ((:arg1 ?verb ?b)))
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase +)
                        (passive -)
                        (syn-function verbal)
                        (infinitive-clause ?inf))
               (boundaries (rightmost-unit ?direct-object-righmost-unit)
                           (leftmost-unit ?vp-leftmost-unit))
               (referent ?verb))
              <-
              (?vp-unit
                --
                (referent ?verb)
                (syn-cat (transitive +)
                        ;; (aux ?+)
                         (ergative -)
                         (part-of-phrase ?+)
                         (infinitive-clause ?inf)
                         (phrase-type verb-phrase))
                (boundaries (leftmost-unit ?vp-leftmost-unit)
                            (rightmost-unit ?vp-rightmost-unit))
                (subunits (?verb-unit)))
               --
               (?verb-unit
               --
               (syn-cat (transitive +)))
              (?direct-object-unit
               --
               (referent ?b)
               (syn-cat (phrase-type ?noun-phrase)
                        (syn-function nominal))
               (boundaries (rightmost-unit ?direct-object-rightmost-unit)
                           (leftmost-unit ?direct-object-leftmost-unit)))
              (?active-transitive-unit
               --
              (HASH form ((precedes ?vp-rightmost-unit ?direct-object-leftmost-unit))))))

(def-fcg-cxn active-ergative-transitive-cxn 
             ((?active-ergative-transitive-unit
               (subunits (?vp-ergative-unit ?patient-unit))
               (meaning ((:arg1 ?event ?arg1)))
               (syn-cat (phrase-type verb-phrase)
                        (infinitive-clause ?inf))
               (boundaries (leftmost-unit ?patient-leftmost-unit)
                           (rightmost-unit ?vp-ergative-unit))
               (referent ?event))
              <-
              (?vp-ergative-unit
                --
                (referent ?event)
                (syn-cat (transitive +)
                         (ergative +)))
              (?patient-unit
               --
               (referent ?arg1)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (rightmost-unit ?patient-unit-rightmost-unit)
                           (leftmost-unit ?patient-unit-leftmost-unit)))
              (?active-ergative-transitive-unit
               --
              (HASH form ((meets ?patient-unit-rightmost-unit ?vp-ergative-unit))))))

(def-fcg-cxn passive-clause-cxn 
             ((?passive-clause-unit
               (subunits (?vp-passive-unit ?patient-unit))
               (meaning ((:arg1 ?event ?arg1)))
               (syn-cat (phrase-type verb-phrase)
                        (passive +))
               (boundaries (leftmost-unit ?patient-leftmost-unit)
                           (rightmost-unit ?vp-passive-rightmost-unit))
               (referent ?event))
              <-
              (?vp-passive-unit
                --
                (referent ?event)
                (syn-cat (phrase-type verb-phrase)
                         (passive +))
                (boundaries (rightmost-unit ?vp-passive-rightmost-unit)
                           (leftmost-unit ?vp-passive-leftmost-unit)))
              (?patient-unit
               --
               (referent ?arg1)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (rightmost-unit ?patient-unit-rightmost-unit)
                           (leftmost-unit ?patient-unit-leftmost-unit)))
              (?passive-clause-unit
               --
              (HASH form ((meets ?patient-unit-rightmost-unit ?vp-passive-leftmost-unit))))))

(def-fcg-cxn gerund-phrase-cxn
             ((?gerund-phrase-unit
               (referent ?ger)
               (meaning ((:arg1 ?ger ?ref)))
               (syn-cat (syn-function nominal)
                        (gerund +))
               (subunits (?gerund-unit ?arg1-unit))
               (boundaries (leftmost-unit ?gerund-phrase-leftmost-unit)
                           (rightmost-unit ?gerund-phrase-rightmost-unit)))
               <-
               (?gerund-unit
                --
                (referent ?ger)
                (syn-cat (lex-class verb)
                         (gerund +)))
               (?arg1-unit
                --
                (referent ?ref)
                (syn-cat (phrase-type nominal))
                (boundaries (leftmost-unit ?arg1-leftmost-unit)
                            (rightmost-unit ?arg1-rightmost-unit)))
               (?gerund-phrase-unit
                --
                (HASH form ((meets ?gerund-unit ?arg1-leftmost-unit))))))

(def-fcg-cxn arg1of-before-np-cxn
             ((?clause-arg1of-unit
               (meaning ((:arg1-of ?t ?o)))
               (subunits (?object-unit ?clause-unit))
               (boundaries (leftmost-unit ?object-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
              <-
              (?object-unit
               --
               (referent ?t)
               (syn-cat (lex-class pronoun)
                        (syn-function nominal)
                        (interrogative -)))
               (?clause-unit
               --
               (referent ?o)
               (syn-cat (phrase-type verb-phrase))
               (meaning ((:arg0 ?o ?g)))
               (boundaries (leftmost-unit ?clause-leftmost-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
               (?clause-arg1of-unit
               --
               (HASH form ((meets ?object-unit ?clause-leftmost-unit))))))

(def-fcg-cxn interrogative-possessive-cxn
             ((?interrogative-possessive-unit
               (meaning ((:poss ?t ?a)))
               (subunits (?whose-unit ?possessed-unit))
               (referent ?t)
               (syn-cat (syn-function interrogative))
               (boundaries (leftmost-unit ?whose-unit)
                           (rightmost-unit ?possessed-rightmost-unit)))
               <-
               (?whose-unit
                --
                (referent ?a)
                (syn-cat (lex-class pronoun)
                         (syn-function interrogative)))
               (?possessed-unit
                --
                (referent ?t)
                (syn-cat (phrase-type nominal))
                (boundaries (leftmost-unit ?possessed-leftmost-unit)
                            (rightmost-unit ?possessed-rightmost-unit)))
               (?interrogative-possessive-unit
                --
                (HASH form ((meets ?whose-unit ?possessed-leftmost-unit))))))

(def-fcg-cxn arg0-of-nominal-cxn 
             ((?arg0-of-nominal-unit
               (meaning ((:arg0-of ?p ?s)))
               (subunits (?arg0-of-verb ?named-entity-unit))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?arg0-of-verb-rightmost-unit))
               (referent ?p))
              <-
               (?named-entity-unit
               --
               (referent ?p)
               (syn-cat (phrase-type noun-phrase)
                        (named-entity-type person))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
              (?arg0-of-verb
               --
               (referent ?s)
               (syn-cat (phrase-type noun-phrase)
                        (part-of-phrase +))
               (boundaries (leftmost-unit ?arg0-of-verb-leftmost-unit)
                           (rightmost-unit ?arg0-of-verb-rightmost-unit)))
              (?arg0-of-nominal-unit
               --
               (HASH form ((meets ?arg0-of-verb-rightmost-unit ?named-entity-leftmost-unit))))))

(def-fcg-cxn arg0-inverse-role-relative-cxn
             ((?arg0-of-relative-unit
               (meaning ((:arg0-of ?arg0-of ?verb)))
               (subunits (?vp-unit ?relative-unit ?named-entity-unit))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?vp-rightmost-unit))
               (referent ?arg0-of))
              <-
               (?named-entity-unit
               --
               (referent ?arg0-of)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?named-entity-rightmost-unit)))
                (?relative-unit
                --
                (syn-cat (lex-class pronoun)
                         (relative +)))
                (?vp-unit
                 --
                 (referent ?verb)
                 (syn-cat (phrase-type verb-phrase))
                 (boundaries (leftmost-unit ?vp-leftmost-unit)
                             (rightmost-unit ?vp-rightmost-unit)))
                (?arg0-of-relative-unit
                 --
                 (HASH form ((precedes ?named-entity-rightmost-unit ?relative-unit)
                             (precedes ?relative-unit ?vp-leftmost-unit))))))

(def-fcg-cxn arg0-inverse-clause-cxn
             ((?arg0-inverse-clause-unit
               (meaning ((:arg0-of ?arg0-of ?verb)))
               (subunits (?vp-unit ?np-unit))
               (boundaries (leftmost-unit ?named-entity-leftmost-unit)
                           (rightmost-unit ?vp-rightmost-unit))
               (referent ?arg0-of))
              <-
               (?np-unit
               --
               (referent ?arg0-of)
               (syn-cat (phrase-type ?noun-phrase)
                        (inverse-arg0 +))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit)))
                (?vp-unit
                 --
                 (referent ?verb)
                 (syn-cat (phrase-type verb-phrase)
                          (part-of-phrase ?+))
                 (boundaries (leftmost-unit ?vp-leftmost-unit)
                             (rightmost-unit ?vp-rightmost-unit)))
                (?arg0-inverse-clause-unit
                 --
                 (HASH form ((precedes ?np-rightmost-unit ?vp-leftmost-unit))))))

(def-fcg-cxn arg0-inverse-clause-nominal-cxn
             ((?arg0-inverse-clause-nominal-unit
               (meaning ((:arg0-of ?g ?p)))
               (subunits (?vp-unit ?nominal-unit))
               (boundaries (rightmost-unit ?nominal-rightmost-unit)
                           (leftmost-unit ?vp-leftmost-unit))
               (referent ?g))
              <-
               (?nominal-unit
               --
               (referent ?g)
               (syn-cat (phrase-type nominal))
               (subunits (?noun-unit))
               (boundaries (leftmost-unit ?nominal-leftmost-unit)
                           (rightmost-unit ?nominal-rightmost-unit)))
               (?noun-unit
               --
               (syn-cat (inverse-arg0 +)))
                (?vp-unit
                 --
                 (referent ?p)
                 (syn-cat (phrase-type verb-phrase)
                          (part-of-phrase ?+))
                 (boundaries (leftmost-unit ?vp-leftmost-unit)
                             (rightmost-unit ?vp-rightmost-unit)))
                (?arg0-inverse-clause-nominal-unit
                 --
                 (HASH form ((meets ?vp-rightmost-unit ?nominal-leftmost-unit ))))))

(def-fcg-cxn arg0-of-precedes-verb-cxn
             ((?arg0-of-precedes-verb-unit
               (meaning ((:arg0-of ?arg0-of ?verb)))
               (subunits (?vp-unit ?np-unit))
               (syn-cat  (phrase-type clause)
                         (syn-function mod))
               (boundaries (leftmost-unit ?vp-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit))
               (referent ?arg0-of))
              <-
               (?np-unit
               --
               (referent ?arg0-of)
               (syn-cat (phrase-type ?noun-phrase)
                        (inverse-arg0 +))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit)))
                (?vp-unit
                 --
                 (referent ?verb)
                 (syn-cat (phrase-type verb-phrase)
                          (part-of-phrase ?+))
                 (boundaries (leftmost-unit ?vp-leftmost-unit)
                             (rightmost-unit ?vp-rightmost-unit)))
                (?arg0-of-precedes-verb-unit
                 --
                 (HASH form ((precedes ?vp-rightmost-unit ?np-leftmost-unit))))))

(def-fcg-cxn V-to-infinitive-cxn
             ((?V-to-infinitive-unit
              (meaning ((:arg1 ?verb ?inf)
                        (:arg0 ?inf ?arg0)))
              (subunits (?finite-verb-unit ?to-unit ?infinitive-unit))
              (referent ?verb)
              (syn-cat (phrase-type verb-phrase)
                       (part-of-phrase +)
                        (infinitive-clause +))
              (boundaries
               (leftmost-unit ?finite-verb-leftmost-unit)
               (rightmost-unit ?infinitive-unit)))
              <-
              (?finite-verb-unit
               --
               (referent ?verb)
               (syn-cat (phrase-type verb-phrase)
                        (part-of-phrase ?+)
                        (syn-function verbal)
                        (transitive ?trans))
               (boundaries (rightmost-unit ?finite-verb-rightmost-unit)
                           (leftmost-unit ?finite-verb-leftmost-unit)))
              (?to-unit
               --
               (syn-cat (lex-class preposition))
               (form ((string ?to-unit "to"))))
              (?infinitive-unit
              --
              (referent ?inf)
              (syn-cat (lex-class verb)
                        (infinitive +)))
              (?V-to-infinitive-unit
              --
              (HASH form ((precedes ?finite-verb-rightmost-unit ?to-unit)
                          (precedes ?to-unit ?infinitive-unit))))))

(def-fcg-cxn modal-to-infinitive-cxn
             ((?V-to-infinitive-unit
              (meaning ((:arg0 ?inf ?arg0)))
              (subunits (?finite-verb-unit ?to-unit ?infinitive-unit))
              (referent ?verb)
              (syn-cat (phrase-type verb-phrase)
                       (part-of-phrase +)
                        (infinitive-clause +))
              (boundaries
               (leftmost-unit ?finite-verb-leftmost-unit)
               (rightmost-unit ?infinitive-unit)))
              <-
              (?finite-verb-unit
               --
               (referent ?verb)
               (syn-cat (modal +)
                        (to-infinitive +))
               (boundaries (rightmost-unit ?finite-verb-rightmost-unit)
                           (leftmost-unit ?finite-verb-leftmost-unit)))
              (?to-unit
               --
               (syn-cat (lex-class preposition))
               (form ((string ?to-unit "to"))))
              (?infinitive-unit
              --
              (referent ?inf)
              (syn-cat (lex-class verb)
                        (infinitive +)))
              (?V-to-infinitive-unit
              --
              (HASH form ((precedes ?finite-verb-rightmost-unit ?to-unit)
                          (precedes ?to-unit ?infinitive-unit))))))

(def-fcg-cxn temporal-clause-adverb-cxn
             ((?temporal-clause-adverb-unit
               (subunits (?time-unit ?clause-unit))
               (meaning ((:time ?d ?a)))
               (boundaries (rightmost-unit ?vp-rightmost-unit)
                           (leftmost-unit ?adverb-of-time-unit))
               (referent ?d))
               <-
               (?time-unit
                --
                (referent ?a)
                (syn-cat (syn-function adverbial-time))
                (sem-cat (sem-class time))
                (boundaries (rightmost-unit ?time-rightmost-unit)
                            (leftmost-unit ?rime-leftmost-unit)))
               (?clause-unit
                --
                (referent ?d)
                (syn-cat (phrase-type verb-phrase))
                (boundaries (rightmost-unit ?clause-rightmost-unit)
                            (leftmost-unit ?clause-leftmost-unit)))
               (?temporal-clause-adverb-unit
                --
                (HASH form ((meets ?clause-unit-rightmost-unit ?adverb-of-time-unit))))))

(def-fcg-cxn adverbial-time-clause-cxn
           ((?adverbial-time-clause-unit
             (subunits (?adverb-unit ?np-unit))
             (referent ?a)
             (meaning ((:op1 ?a ?w)))
             (syn-cat (syn-function adverbial-time))
             (sem-cat (sem-class time))
             (boundaries (rightmost-unit ?np-rightmost-unit)
                         (leftmost-unit ?adverb-unit)))
             <-
             (?adverb-unit
             --
             (referent ?a)
             (syn-cat (lex-class adverb)))
             (?np-unit
             --
             (referent ?w)
             (syn-cat (phrase-type noun-phrase))
             (boundaries (rightmost-unit ?np-rightmost-unit)
                         (leftmost-unit ?np-leftmost-unit)))
             (?adverbial-time-clause-unit
             --
             (HASH form ((meets ?adverb-unit ?np-leftmost-unit))))))

(def-fcg-cxn origin-cxn
             ((?origin-unit
               (subunits (?preposition-unit ?source-unit ?article-unit ?np-unit))
               (referent ?b)
               (syn-cat (phrase-type noun-phrase)
                        (inverse-arg0 +))
               (meaning ((:source ?b ?c)))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?source-rightmost-unit)))
              <-
              (?np-unit
               --
               (referent ?b)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (rightmost-unit ?np-rightmost-unit)
                           (leftmost-unit ?np-leftmost-unit)))
              (?preposition-unit
              --
              (syn-cat (lex-class preposition))
              (form ((string ?from-unit "from"))))
              (?article-unit
              --
              (syn-cat (lex-class article)))
              (?source-unit
              --
              (referent ?c)
              (syn-cat (syn-function adjectival))
              (sem-cat (sem-class age)))
               (?origin-unit
                --
                (HASH form ((precedes ?np-rightmost-unit ?preposition-unit)
                            (precedes ?preposition-unit ?article-unit)
                            (precedes ?article-unit ?source-unit))))))
 (def-fcg-cxn prep-in-cxn
             ((?prep-in-unit
               (referent ?s)
               (meaning ((:prep-in ?s ?c)))
               (subunits (?vp-unit ?in-unit ?location-unit))
               (boundaries (leftmost-unit ?vp-leftmost-unit)
                           (rightmost-unit ?location-rightmost-unit)))
               <-
               (?vp-unit
                --
               (referent ?s)
               (syn-cat (phrase-type verb-phrase))
               (boundaries (leftmost-unit ?vp-leftmost-unit)
                           (rightmost-unit ?vp-rightmost-unit)))
               (?in-unit
                --
                (syn-cat (lex-class preposition))
                (form ((string ?in-unit "in"))))
               (?location-unit
                --
                (referent ?c)
                (syn-cat (phrase-type noun-phrase))
                (boundaries (leftmost-unit ?location-leftmost-unit)
                           (rightmost-unit ?location-rightmost-unit)))
               (?prep-in-unit
                --
                (HASH form ((meets ?vp-rightmost-unit ?in-unit)
                            (meets ?in-unit ?location-leftmost-unit))))))         

(def-fcg-cxn interrogative-clause-transitive-vp-cxn
             ((?interrogative-clause-transitive-vp-unit
               (meaning ((:arg1 ?f ?a)))
               (referent ?f)
               (syn-cat (interrogative-clause)
                        (passive -))
               (subunits (?object-unit ?clause-unit ?auxiliar-unit ?interrogative-unit))
               (boundaries (leftmost-unit ?object-leftmost-unit)
                           (rightmost-unit ?interrogative-unit)))
              <-
              (?object-unit
               --
               (referent ?a)
               (syn-cat (syn-function interrogative))
               (boundaries (leftmost-unit ?object-leftmost-unit)
                           (rightmost-unit ?object-rightmost-unit)))
              (?auxiliar-unit
               --
               (syn-cat (lex-class verb)
                        (aux +)))
               (?clause-unit
               --
               (referent ?f)
               (syn-cat (phrase-type verb-phrase))
               (meaning ((:arg0 ?o ?g)))
               (boundaries (leftmost-unit ?clause-leftmost-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
               (?interrogative-unit
                --
                (form ((string ?interrogative-unit "?"))))
               (?interrogative-clause-transitive-vp-unit
               --
               (HASH form ((precedes ?object-rightmost-unit ?auxiliar-unit)
                           (precedes ?auxiliar-unit ?clause-leftmost-unit)
                           (precedes ?clause-rightmost-unit ?interrogative-unit))))))

(def-fcg-cxn interrogative-clause-location-vp-cxn
             ((?interrogative-clause-location-vp-unit
               (meaning ((:location ?f ?a)))
               (referent ?f)
               (syn-cat (interrogative-clause)
                        (passive -))
               (subunits (?object-unit ?clause-unit ?auxiliar-unit ?interrogative-unit))
               (boundaries (leftmost-unit ?object-unit)
                           (rightmost-unit ?interrogative-unit)))
              <-
              (?object-unit
               --
               (referent ?a)
               (syn-cat (lex-class pronoun)
                        (syn-function interrogative)
                        (interrogative +)
                        (location +)))
              (?auxiliar-unit
               --
               (syn-cat (lex-class verb)
                        (aux +)))
               (?clause-unit
               --
               (referent ?f)
               (syn-cat (phrase-type verb-phrase))
               (meaning ((:arg0 ?o ?g)))
               (boundaries (leftmost-unit ?clause-leftmost-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
               (?interrogative-unit
                --
                (form ((string ?interrogative-unit "?"))))
               (?interrogative-clause-location-vp-unit
               --
               (HASH form ((precedes ?object-unit ?auxiliar-unit)
                           (precedes ?auxiliar-unit ?clause-leftmost-unit)
                           (precedes ?clause-rightmost-unit ?interrogative-unit))))))

(def-fcg-cxn arg1-of-np-preposition-cxn
             ((?arg1-of-np-preposition-unit
               (meaning ((:arg1 ?r ?b)))
               (referent ?r)
               (subunits (?np-unit ?auxiliar-unit ?np-preposition-unit))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-preposition-rightmost-unit)))
              <-
              (?np-unit
               --
               (referent ?b)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?np-leftmost-unit)
                           (rightmost-unit ?np-rightmost-unit)))
              (?auxiliar-unit
               --
               (syn-cat (lex-class verb)
                        (arg1-preposition +)))
               (?np-preposition-unit
               --
               (referent ?r)
               (syn-cat (phrase-type noun-phrase)
                        (syn-function patient))
               (boundaries (leftmost-unit ?np-preposition-leftmost-unit)
                           (rightmost-unit ?np-preposition-rightmost-unit)))
               (?arg1-of-np-preposition-unit
               --
               (HASH form ((meets ?np-rightmost-unit ?auxiliar-unit)
                           (meets ?auxiliar-unit ?np-preposition-leftmost-unit))))))

(def-fcg-cxn arg2-cxn
         ((?comparative-unit
           (meaning ((:arg2 ?d ?d)))
            (referent ?d)
            (subunits (?conclusion-unit ?linking-unit ?clause-unit))
            (boundaries (rightmost-unit ?conclusion-rightmost-unit)
                        (leftmost-unit ?clause-leftmost-unit)))
            <-
            (?conclusion-unit
            --
            (referent ?d)
            (syn-cat (phrase-type ?nominal))
            (boundaries (leftmost-unit ?conclusion-leftmost-unit)
                        (rightmost-unit ?conclusion-rightmost-unit)))
            (?linking-unit
            --
            (syn-cat (syn-function preposition-arg2)))
            (?clause-unit
            --
            (referent ?d)
            (syn-cat (main-clause +))
            (boundaries (leftmost-unit ?clause-leftmost-unit)
                        (rightmost-unit ?clause-rightmost-unit)))
            (?comparative-unit
             --
             (HASH form ((meets ?clause-rightmost-unit ?linking-unit)
                         (meets ?linking-unit ?conclusion-leftmost-unit))))))

(def-fcg-cxn passive-nominal-cxn
            ((?passive-nominal-unit
              (meaning ((:arg0 ?d ?b)))
              (referent ?b)
              (subunits (?agent-unit ?preposition-unit ?clause-unit))
              (boundaries (rightmost-unit ?agent-rightmost-unit)
                           (leftmost-unit ?clause-leftmost-unit)))
             <-
             (?agent-unit
              --
              (referent ?b)
              (syn-cat (phrase-type noun-phrase))
              (boundaries (leftmost-unit ?agent-leftmost-unit)
                          (rightmost-unit ?agent-rightmost-unit)))
              (?preposition-unit
              --
              (syn-cat (lex-class preposition))
              (form ((string ?preposition-unit "by"))))
              (?clause-unit
              --
              (referent ?d)
              (syn-cat (phrase-type verb-phrase)
                       (passive -))
              (meaning ((:arg1 ?d ?r)))
              (boundaries (leftmost-unit ?clause-leftmost-unit)
                          (rightmost-unit ?clause-rightmost-unit)))
              (?passive-nominal-unit
               --
               (HASH form ((meets ?clause-rightmost-unit ?preposition-unit)
                           (meets ?preposition-unit ?agent-leftmost-unit))))))

(def-fcg-cxn be-located-at-cxn
             ((?be-located-at-unit
               (referent ?b)
               (syn-cat (phrase-type verb-phrase))
               (meaning ((:arg1 ?b ?m)
                         (:arg2 ?b ?j)
                         (:polarity ?b -)))
               (subunits (?patient-unit ?be-located-unit ?not-unit ?arg2-unit ?in-preposition-unit))
               (boundaries (leftmost-unit ?patient-leftmost-unit)
                           (rightmost-unit ?arg2-rightmost-unit)))
               <-
               (?patient-unit
               --
               (referent ?m)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?patient-leftmost-unit)
                           (rightmost-unit ?patient-rightmost-unit)))
               (?not-unit
                --
                (syn-cat (lex-class adverb))
                (form ((string ?not-unit "not"))))
               (?be-located-unit
                --
                (referent ?b)
                (meaning ((be-located-at-91 ?b))))
               (?in-preposition-unit
               --
               (syn-cat (lex-class preposition))
               (form ((string ?in-preposition-unit "in"))))
               (?arg2-unit
               --
               (referent ?j)
               (syn-cat (phrase-type noun-phrase))
               (boundaries (leftmost-unit ?arg2-leftmost-unit)
                           (rightmost-unit ?arg2-rightmost-unit)))
               (?be-located-at-unit
                --
                (HASH form ((meets ?patient-rightmost-unit ?be-located-unit)
                            (meets ?be-located-unit ?not-unit)
                            (meets ?not-unit ?in-preposition-unit)
                            (meets ?in-preposition-unit ?arg2-leftmost-unit))))))

(def-fcg-cxn subordinate-positive-cxn
           ((?subordinate-positive-unit
             (referent ?t-1)
             (meaning ((:arg1 ?t-1 ?w)))
             (subunits (?main-clause ?subordinate-clause))
             (boundaries (rightmost-unit ?subordinate-clause-rightmost-unit)
                         (leftmost-unit ?main-clause-leftmost-unit)))
            <-
            (?main-clause
              --
              (referent ?t-1)
              (syn-cat (phrase-type verb-phrase)
                       (syn-function main-clause))
              (boundaries (leftmost-unit ?main-clause-leftmost-unit)
                          (rightmost-unit ?main-clause-rightmost-unit)))
             (?subordinate-clause
              --
              (referent ?w)
              (syn-cat (phrase-type verb-phrase)
                       (syn-function main-clause))
              (boundaries (leftmost-unit ?subordinate-clause-leftmost-unit)
                          (rightmost-unit ?subordinate-clause-rightmost-unit)))
             (?subordinate-positive-unit
             --
             (HASH form ((meets ?main-clause-rightmost-unit ?subordinate-clause-leftmost-unit))))))


(def-fcg-cxn mod-of-a-clause-cxn 
             ((?mod-of-a-clause-unit
               (referent ?g)
               (meaning ((:mod ?g ?c)))
               (subunits (?clause-unit ?nominal-unit))
               (boundaires (leftmost-unit ?nominal-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
              <-
              (?clause-unit
               --
               (referent ?g)
               (syn-cat (phrase-type clause)
                         (syn-function mod))
               (boundaries (leftmost-unit ?clause-leftmost-unit)
                           (rightmost-unit ?clause-rightmost-unit)))
              (?nominal-unit
               --
               (referent ?c)
               (syn-cat (lex-class proper-noun)))
              (?mod-of-a-clause-unit
               --
               (HASH form ((meets ?nominal-unit ?clause-leftmost-unit))))))

                        

)

#|

New grammar
for 75 sentences 
148 lexical-morph
70 cxn

Old grammar
for 45 sentences :
92 lexical-morph
43 phrasal|arg0

                        

|#