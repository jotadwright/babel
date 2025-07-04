(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn largest-of-?x-cxn-1
             ((?largest-of-?x-optional-signs-unit
               (form ((adjacent ?largest-of-?x\(?x\)-boundary-right ?un)
                      (adjacent ?un ?grand))))
              (?largest-of-?x-unit
               (subunits (?largest-of-?x\(?x\)-unit ?largest-of-?x-optional-signs-unit))
               (unit-cat largest-of-?x-cat)
               (args ((target ?a)
                      (scope ?e)))
               (boundaries ((left ?largest-of-?x\(?x\)-boundary-left)
                            (right ?grand))))
              <-
              (?largest-of-?x-unit
               (HASH meaning ((largest ?e ?a ?f)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (two-hand-articulation ?grand ""))))
              (?largest-of-?x\(?x\)-unit
               (unit-cat largest-of-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat largest-of-?x\(?x\)-cat)
               (boundaries ((right ?largest-of-?x\(?x\)-boundary-right)
                            (left ?largest-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)




              
                           
                           