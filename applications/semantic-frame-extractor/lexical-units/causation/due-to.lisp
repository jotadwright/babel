(in-package :frame-extractor)

(def-fcg-cxn due-to-preposition-lex
             (
              <-
              (?due-unit
               (referent ?frame)
               (syn-cat (lex-class preposition))
               (lex-id due-to)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect)))
               (meaning ((frame causation because-of ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               --
               (form ((string ?because-unit "due")))
               (dependency (pos-tag in)
                           (edge prep))
               (dependents (?to-unit)))
              (?to-unit
               (lex-id due-to)
               --
               (form ((string ?to-unit "to")))
               (head ?due-unit)))
             :cxn-set lex)