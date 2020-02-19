
(in-package :frame-extractor)

(def-fcg-cxn cause-verb-lex-cxn
             ((?cause-unit
               (meaning ((frame causation cause ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               (sem-cat (frame causation))
               (syn-valence (subject ?subject)
                            (object ?object))
               (sem-valence (actor ?cause)
                            (theme ?effect))
               (referent ?frame))
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
               (syn-cat (lex-class verb))
               (lex-id cause)
               --
               (form ((string ?causes-unit "causes")))))
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

(def-fcg-cxn caused-morph-cxn
             ((?caused-unit
               (syn-cat (lex-class verb))
               (lex-id cause))
              <-
              (?caused-unit
               --
               (form ((string ?caused-unit "caused")))))
            :cxn-set morph)
