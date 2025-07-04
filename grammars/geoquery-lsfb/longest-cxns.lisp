(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn longest-?x-cxn-1 
             ((?longest-?x-optional-signs-unit
               (form ((adjacent ?longest-?x\(?x\)-boundary-right ?long))))
              (?longest-?x-unit
               (subunits (?longest-?x\(?x\)-unit ?longest-?x-optional-signs-unit))
               (unit-cat longest-?x-cat)
               (boundaries ((left longest-?x\(?x\)-boundary-left)
                            (right ?long)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?longest-?x-unit
               (HASH meaning ((longest ?d ?a ?e)))
               --
               (HASH form ((two-hand-articulation ?long ""))))
              (?longest-?x\(?x\)-unit
               (unit-cat longest-?x\(?x\)-cat)
               (args ((target ?a)
                      (scope ?e)))
               --
               (unit-cat longest-?x\(?x\)-cat)
               (boundaries ((right ?longest-?x\(?x\)-boundary-right)
                            (left ?longest-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn longest-?x-cxn-2
             ((?longest-?x-optional-signs-unit
               (form ((adjacent ?longest-?x\(?x\)-boundary-right ?un)
                      (adjacent ?gros ?long))))
              (?longest-?x-unit
               (subunits (?longest-?x\(?x\)-unit ?longest-?x-optional-signs-unit))
               (unit-cat longest-?x-cat)
               (boundaries ((left longest-?x\(?x\)-boundary-left)
                            (right ?long)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?longest-?x-unit
               (HASH meaning ((longest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (two-hand-articulation ?grand "")
                           (two-hand-articulation ?gros "")
                           (two-hand-articulation ?long "")
                           (adjacent ?un ?grand)
                           (adjacent ?grand ?gros))))
              (?longest-?x\(?x\)-unit
               (unit-cat longest-?x\(?x\)-cat)
               (args ((target ?a)
                      (scope ?e)))
               --
               (unit-cat longest-?x\(?x\)-cat)
               (boundaries ((right ?longest-?x\(?x\)-boundary-right)
                            (left ?longest-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)