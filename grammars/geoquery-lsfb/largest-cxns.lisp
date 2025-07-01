(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn largest-of-?x-cxn-1
             ((?largest-of-?x-unit
               (subunits (?largest-of-?x\(?x\)-unit))
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
                           (right-hand-articulation ?beaucoup "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?largest-of-?x\(?x\)-boundary-right ?un)
                           (adjacent ?un ?beaucoup)
                           (adjacent ?beaucoup ?grand))))
              (?largest-of-?x\(?x\)-unit
               (unit-cat largest-of-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat largest-of-?x\(?x\)-cat)
               (boundaries ((right ?largest-of-?x\(?x\)-boundary-right)
                            (left ?largest-of-?x\(?x\)-boundary-left)))
               ))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn largest-cxn-1
             ((?largest-unit
               (unit-cat largest-cat)
               (args ((scope ?e)
                      (target ?b)
                      (source ?f)))
               (boundaries ((left ?plus)
                            (right ?plus))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?e ?b ?f)))
               --
               (HASH form ((right-hand-articulation ?plus "")))))
             :cxn-inventory *geoquery-lsfb*)


              
                           
                           