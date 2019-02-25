(in-package :frame-extractor)

(def-fcg-cxn result-in-verb-lex
              (
               <-
               (?result-unit
                (referent ?frame)
                (sem-cat (frame causation))
                (syn-valence (subject ?subject-unit)
                             (prep-object ?object-unit))
                (sem-valence (actor ?cause)
                             (theme ?effect))
                (meaning ((frame causation result-in ?frame) 
                          (slot cause ?frame ?cause)
                          (slot effect ?frame ?effect)))
                --
                (syn-cat (lex-class verb))
                (lex-id result-in)
                (dependents (?in-unit)))
               (?in-unit
                (lex-id result-in)
                --
                (head ?result-unit)
                (form ((string ?in-unit "in")))))
              :cxn-set lex)

(def-fcg-cxn result->results-morph
             (
              <-
              (?results-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id result-in)
               --
               (form ((string ?results-unit "results")))))
            :cxn-set morph)


(def-fcg-cxn result->resulting-morph
             (
              <-
              (?resulting-unit
               (syn-cat (lex-class verb)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id result-in)
               --
               (form ((string ?resulting-unit "resulting")))))
            :cxn-set morph)

(def-fcg-cxn result->result-morph
             (
              <-
              (?result-unit
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id result-in)
               --
               (form ((string ?result-unit "result")))))
            :cxn-set morph)

(def-fcg-cxn result->resulted-morph
             (
              <-
              (?resulted-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id result-in)
               --
               (form ((string ?resulted-unit "resulted")))))
            :cxn-set morph)