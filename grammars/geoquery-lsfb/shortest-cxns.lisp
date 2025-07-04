(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn shortest-?x-cxn-1 
             ((?shortest-?x-optional-signs-unit
               (form ((right-hand-articulation ?plus "")
                       (adjacent ?plus ?court)
                       (adjacent ?shortest-?x\(?x\)-boundary-right ?plus))))
              (?shortest-?x-unit
               (subunits (?shortest-?x\(?x\)-unit ?shortest-?x-optional-signs-unit))
               (unit-cat shortest-?x-cat)
               (boundaries ((left ?shortest-?x\(?x\)-boundary-left)
                            (right ?court)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?shortest-?x-unit
               (HASH meaning ((shortest ?e ?b ?f)))
               --
               (HASH form ((two-hand-articulation ?court ""))))
              (?shortest-?x\(?x\)-unit
               (unit-cat shortest-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?f)))
               --
               (unit-cat shortest-?x\(?x\)-cat)
               (boundaries ((right ?shortest-?x\(?x\)-boundary-right)
                            (left ?shortest-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn shortest-?x-cxn-2
             ((?shortest-?x-unit
               (subunits (?shortest-?x\(?x\)-unit))
               (unit-cat shortest-?x-cat)
               (boundaries ((left ?shortest-?x\(?x\)-boundary-left)
                            (right ?court)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?shortest-?x-unit
               (HASH meaning ((shortest ?e ?b ?f)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (two-hand-articulation ?court "")
                           (adjacent ?plus ?court)
                           (adjacent ?shortest-?x\(?x\)-boundary-right ?plus))))
              (?shortest-?x\(?x\)-unit
               (unit-cat shortest-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?f)))
               --
               (unit-cat shortest-?x\(?x\)-cat)
               (boundaries ((right ?shortest-?x\(?x\)-boundary-right)
                            (left ?shortest-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)