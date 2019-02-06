(in-package :frame-extractor)

(def-fcg-cxn because-preposition-lex
             ((?because-unit
               (referent ?frame)
               (syn-cat (lex-class preposition))
               (lex-id because)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect))))
              <-
              (?because-unit
               (hash meaning ((frame causation because ?frame) 
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (HASH form ((string ?because-unit "because")))
               ))
            :attributes (:label (hashed-string)))

(def-fcg-cxn because-initial-preposition-lex
             ((?because-unit
               (referent ?frame)
               (syn-cat (lex-class preposition))
               (lex-id because)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect))))
              <-
              (?because-unit
               (hash meaning ((frame causation because ?frame) 
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (HASH form ((string ?because-unit "Because")))
               ))
            :attributes (:label (hashed-string)))