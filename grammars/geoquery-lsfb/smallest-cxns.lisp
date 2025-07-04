(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn smallest-of-?x-cxn-1
             ((?smallest-of-?x-optional-signs-unit
               (form ((adjacent ?smallest-of-?x\(?x\)-boundary-right ?plus))))
              (?smallest-of-?x-unit
               (subunits (?smallest-of-?x\(?x\)-unit ?smallest-of-?x-optional-signs-unit))
               (unit-cat smallest-of-?x-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?smallest-of-?x\(?x\)-boundary-left)
                            (right ?petit))))
              <-
              (?smallest-of-?x-unit
               (HASH meaning ((smallest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (right-hand-articulation ?petit "")
                           (adjacent ?plus ?petit))))
              (?smallest-of-?x\(?x\)-unit
               (unit-cat smallest-of-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat smallest-of-?x\(?x\)-cat)
               (boundaries ((right ?smallest-of-?x\(?x\)-boundary-right)
                            (left ?smallest-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn smallest-of-?x-cxn-2
             ((?smallest-of-?x-optional-signs-unit
               (form ((adjacent ?smallest-of-?x\(?x\)-boundary-right ?plus))))
              (?smallest-of-?x-unit
               (subunits (?smallest-of-?x\(?x\)-unit ?smallest-of-?x-optional-signs-unit))
               (unit-cat smallest-of-?x-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?smallest-of-?x\(?x\)-boundary-left)
                            (right ?petit))))
              <-
              (?smallest-of-?x-unit
               (HASH meaning ((smallest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (two-hand-articulation ?petit "")
                           (adjacent ?plus ?petit))))
              (?smallest-of-?x\(?x\)-unit
               (unit-cat smallest-of-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat smallest-of-?x\(?x\)-cat)
               (boundaries ((right ?smallest-of-?x\(?x\)-boundary-right)
                            (left ?smallest-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)