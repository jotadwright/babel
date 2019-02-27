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
               (meaning ((frame causation due-to ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               --
               (form ((string ?because-unit "due")))
               (dependency (pos-tag in))
               (dependents (?to-unit)))
              (?to-unit
               (lex-id due-to)
               --
               (form ((string ?to-unit "to")))
               (head ?due-unit)))
             :cxn-set lex)

(def-fcg-cxn due-to-adjective-lex
             (
              <-
              (?due-unit
               (referent ?frame)
               (syn-cat (lex-class preposition))
               (lex-id due-to)
               (sem-cat (frame causation)
                        (frame-slots (cause ?cause)
                                     (effect ?effect)))
               (meaning ((frame causation due-to ?frame) 
                         (slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               --
               (form ((string ?because-unit "due")))
               (dependency (pos-tag jj))
                         ;  (edge prep))
               (dependents (?to-unit)))
              (?to-unit
               (lex-id due-to)
               --
               (form ((string ?to-unit "to")))
               (head ?due-unit)))
             :cxn-set lex)


