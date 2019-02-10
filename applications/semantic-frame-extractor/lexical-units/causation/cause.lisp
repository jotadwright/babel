
(in-package :frame-extractor)

(def-fcg-cxn cause-verb-lex
             ((?cause-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (syn-valence (subject ?subject-unit)
                            (object ?object-unit))
               (sem-valence (actor ?cause)
                            (theme ?effect))
               (meaning ((frame causation cause ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect))))
              <-
              (?cause-unit
               --
               (syn-cat (lex-class verb))
               (lex-id cause)))
             :cxn-set lex)

(def-fcg-cxn cause->causes-morph
             (
              <-
              (?causes-unit
               (syn-cat (lex-class verb)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id cause)
               --
               ((string ?causes-unit "causes"))))
            :cxn-set morph)

(def-fcg-cxn cause->causing-morph
             (
              <-
              (?causing-unit
               (syn-cat (lex-class verb)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id cause)
               --
               (form ((string ?causing-unit "causing")))))
            :cxn-set morph)

(def-fcg-cxn cause->cause-morph
             (
              <-
              (?cause-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id cause)
               --
               (form ((string ?cause-unit "cause")))))
            :cxn-set morph)

(def-fcg-cxn cause->caused-morph
             (
              <-
              (?caused-unit
               (syn-cat (lex-class verb)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id cause)
               --
               (form ((string ?caused-unit "caused")))))
            :cxn-set morph)