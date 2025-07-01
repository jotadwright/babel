(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn answer-?x-cxn-1
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?quoi))))
              (?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?quoi))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?quoi ""))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn answer-?x-cxn-2
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?combien))))
              (?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?combien))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?e ?a ?f)))
               --
               (HASH form ((two-hand-articulation ?combien ""))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat-2)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat-2)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

#|
(def-fcg-cxn answer-?x-cxn-2
             ((?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?quoi))))
              (?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?nom))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?quoi "")
                           (two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?quoi ?palm-up)
                           (adjacent ?palm-up ?nom))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))
               )
              )
             :cxn-inventory *geoquery-lsfb*)



(def-fcg-cxn answer-?x-cxn-3
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?quoi))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((two-hand-articulation ?nom "")
                           (right-hand-articulation ?quoi "")
                           (adjacent ?answer-?x\(?x\)-boundary-right ?nom)
                           (adjacent ?nom ?quoi))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

|#