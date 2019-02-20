(in-package :frame-extractor)

(def-fcg-cxn because-conjunction-lex
             ((?because-unit
               (referent ?frame)
               (syn-cat (lex-class conjunction))
               (lex-id because)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect)))
               (meaning ((frame causation because ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect))))
              <-
              (?because-unit
                
               --
               (form ((string ?because-unit "because")))
               (dependency (pos-tag in)
                           (edge mark))))
            :cxn-set lex)

(def-fcg-cxn because-initial-conjunction-lex
             ((?because-unit
               (referent ?frame)
               (syn-cat (lex-class conjunction))
               (lex-id because)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect)))
               (meaning ((frame causation because ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect))))
              <-
              (?because-unit
                
               --
               (form ((string ?because-unit "Because")))
               (dependency (pos-tag in)
                           (edge mark))))
            :cxn-set lex)

(def-fcg-cxn because-of-preposition-lex
             ((?because-unit
               (referent ?frame)
               (syn-cat (lex-class preposition))
               (lex-id because-of)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect)))
               (meaning ((frame causation because-of ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect))))
              <-
              (?because-unit
               --
               (form ((string ?because-unit "because")))
               (dependency (pos-tag in)
                           (edge prep))
               (dependents (?of-unit)))
              (?of-unit
               --
               (form ((string ?of-unit "of")))
               (head ?because-unit)))
            :cxn-set lex)