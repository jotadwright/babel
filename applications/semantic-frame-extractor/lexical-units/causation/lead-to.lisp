(in-package :frame-extractor)

(def-fcg-cxn lead-to-verb-lex
              ((?lead-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (syn-valence (subject ?subject-unit)
                            (object ?object-unit))
               (sem-valence (actor ?cause)
                            (theme ?effect)))
              <-
              (?lead-unit
               (HASH meaning ((frame causation lead ?frame) 
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (syn-cat (lex-class verb))
               (lex-id lead)
               (dependents (?to-unit)))
              (?to-unit
               --
               (head ?lead-unit)
               (form ((string ?to-unit "to")))))
            :attributes (:label (hashed-lex-id)))

(def-fcg-cxn lead->leads-morph
             ((?leads-unit
               (footprints (number morph)))
              <-
              (?leads-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id lead)
               --
               (HASH form ((string ?leads-unit "leads")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-string)))


(def-fcg-cxn lead->leading-morph
             ((?leading-unit
               (footprints (number morph)))
              <-
              (?leading-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form ing-form)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id lead)
               --
               (HASH form ((string ?leading-unit "leading")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-string)))

(def-fcg-cxn lead->lead-morph
             ((?lead-unit
               (footprints (number morph)))
              <-
              (?lead-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id lead)
               --
               (HASH form ((string ?lead-unit "lead")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string))

(def-fcg-cxn lead->led-morph
             ((?led-unit
               (footprints (number morph)))
              <-
              (?led-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form participle)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id lead)
               --
               (HASH form ((string ?led-unit "led")))))
            :disable-automatic-footprints t
            :attributes (:label hashed-string))