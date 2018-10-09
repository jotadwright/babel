
(in-package :frame-extractor)

(def-fcg-cxn cause-verb-lex
             ((?cause-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (syn-valence (subject ?subject-unit)
                            (object ?object-unit))
               (sem-valence (actor ?cause)
                            (theme ?effect)))
              <-
              (?cause-unit
               (hash meaning ((frame causation cause ?frame) 
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (syn-cat (lex-class verb))
               ;(dependency (pos-tag vb)) ;;or VBD/VBN/...
               (lex-id cause)))
            :attributes (:label (hashed-lex-id hashed-meaning)
                         :meaning cause :lex-id cause :pos ("VERB")))

(def-fcg-cxn cause->causes-morph
             ((?causes-unit
               (footprints (number morph)))
              <-
              (?causes-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (finite +)
                        (agreement (- - + -))
                        (tam (tense present)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality indicative)))
               (lex-id cause)
               --
               (HASH form ((string ?causes-unit "causes")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-lex-id hashed-string) :string "causes" :lex-id cause :pos ("VERB")))

(def-fcg-cxn cause->causing-morph
             ((?causing-unit
               (footprints (number morph)))
              <-
              (?causing-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form ing-form)
                        (finite -)
                        (agreement ?agr)
                        (tam (tense ?tense)
                             (aspect (perfect ?p)
                                     (progressive +))
                             (modality ?m)))
               (lex-id cause)
               --
               (HASH form ((string ?causing-unit "causing")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-lex-id hashed-string) :string "causing" :lex-id cause :pos ("VERB")))

(def-fcg-cxn cause->cause-morph
             ((?cause-unit
               (footprints (number morph)))
              <-
              (?cause-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form base-form)
                        (tam (tense ?tense)
                             (aspect (perfect -)
                                     (progressive -))
                             (modality ?m)))
               (lex-id cause)
               --
               (HASH form ((string ?cause-unit "cause")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-lex-id hashed-string) :string "cause" :lex-id cause :pos ("VERB")))

(def-fcg-cxn cause->caused-morph
             ((?caused-unit
               (footprints (number morph)))
              <-
              (?caused-unit
               (footprints (not number morph))
               (syn-cat (lex-class verb)
                        (verb-form participle)
                        (tam (tense ?tense)
                             (aspect ?aspect)
                             (modality ?m)))
               (lex-id cause)
               --
               (HASH form ((string ?caused-unit "caused")))))
            :disable-automatic-footprints t
            :attributes (:label (hashed-lex-id hashed-string) :string "caused" :lex-id cause :pos ("VERB")))