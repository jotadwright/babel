(in-package :frame-extractor)

(def-fcg-cxn give-rise-to-verb-lex
              (<-
               (?give-unit
                (referent ?frame)
                (sem-cat (frame causation))
                (syn-valence (subject ?subject-unit)
                             (prep-object ?object-unit))
                (sem-valence (actor ?cause)
                             (theme ?effect))
                (meaning ((frame causation give-rise-to ?frame) 
                          (slot cause ?frame ?cause)
                          (slot effect ?frame ?effect)))
                --
                (syn-cat (lex-class verb))
                (lex-id give-rise-to)
                (dependents (?rise-unit ?to-unit)))
               (?rise-unit
                (lex-id give-rise-to)
                --
                (head ?give-unit)
                (form ((string ?rise-unit "rise"))))
               (?to-unit
                (lex-id give-rise-to)
                --
                (head ?give-unit)
                (form ((string ?to-unit "to")))))
              :cxn-set lex)

(def-fcg-cxn give-rise-to-verb-lex-2
              (<-
               (?give-unit
                (referent ?frame)
                (sem-cat (frame causation))
                (syn-valence (subject ?subject-unit)
                             (prep-object ?object-unit))
                (sem-valence (actor ?cause)
                             (theme ?effect))
                (meaning ((frame causation give-rise-to ?frame) 
                          (slot cause ?frame ?cause)
                          (slot effect ?frame ?effect)))
                --
                (syn-cat (lex-class verb))
                (lex-id give-rise-to)
                (dependents (?rise-unit)))
               (?rise-unit
                (lex-id give-rise-to)
                --
                (head ?give-unit)
                (dependents (?to-unit))
                (form ((string ?rise-unit "rise"))))
               (?to-unit
                (lex-id give-rise-to)
                --
                (head ?rise-unit)
                (form ((string ?to-unit "to")))))
              :cxn-set lex)

(def-fcg-cxn give->gives-morph
             (
              <-
              (?gives-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id give-rise-to)
               --
               (form ((string ?gives-unit "gives")))))
            :cxn-set morph)


(def-fcg-cxn give->giving-morph
             (
              <-
              (?giving-unit
               (syn-cat (lex-class verb)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id give-rise-to)
               --
               (form ((string ?giving-unit "giveing")))))
            :cxn-set morph)

(def-fcg-cxn give->give-morph
             (
              <-
              (?give-unit
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id give-rise-to)
               --
               (form ((string ?give-unit "give")))))
            :cxn-set morph)

(def-fcg-cxn give->gave-morph
             (
              <-
              (?gave-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id give-rise-to)
               --
               (form ((string ?gave-unit "gave")))))
            :cxn-set morph)

(def-fcg-cxn give->given-morph
             (
              <-
              (?given-unit
               (lex-id give-rise-to)
               (syn-cat (lex-class verb))
               --
               (form ((string ?given-unit "given")))))
            :cxn-set morph)