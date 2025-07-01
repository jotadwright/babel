(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn ?x-population-of-?y-cxn-1
             ((?x-population-of-?y-unit
               (subunits (?x-population-of-?y\(?x\)-unit ?x-population-of-?y\(?y\)-unit))
               (unit-cat x-population-of-?y-cat)
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?x-population-of-?y\(?y\)-left-boundary)
                            (right ?pt-states-2))))
              <-
              (?x-population-of-?y-unit
               (HASH meaning ((population ?f ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?quelle "")
                           (right-hand-articulation ?quoi-1 "")
                           (right-hand-articulation ?pt-states-1 "")
                           (two-hand-articulation ?habiter "")
                           (right-hand-articulation ?quoi-2 "")
                           (right-hand-articulation ?pt-states-2 "")
                           (left-hand-articulation ?states-ds "")
                           (adjacent ?x-population-of-?y\(?y\)-boundary-right ?quelle)
                           (adjacent ?quelle ?quoi-1)
                           (adjacent ?quoi-1 ?pt-states-1)
                           (adjacent ?pt-states-1 ?x-population-of-?y\(?x\)-boundary-left)
                           (adjacent ?x-population-of-?y\(?x\)-boundary-right ?habiter)
                           (adjacent ?habiter ?quoi-2)
                           (adjacent ?quoi-2 ?pt-states-2)
                           (during ?quoi-2 ?states-ds)
                           (during ?pt-states-2 ?states-ds)
                           )))
              (?x-population-of-?y\(?x\)-unit
               (unit-cat x-population-of-?y\(?x\)-cat)
               (args ((scope ?e)
                      (target ?b)
                      (source ?f)))
               --
               (unit-cat x-population-of-?y\(?x\)-cat)
               (boundaries ((right ?x-population-of-?y\(?x\)-boundary-right)
                            (left ?x-population-of-?y\(?x\)-boundary-left)))
               )
              (?x-population-of-?y\(?y\)-unit
               (unit-cat x-population-of-?y\(?y\)-cat)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat x-population-of-?y\(?y\)-cat)
               (boundaries ((right ?x-population-of-?y\(?y\)-boundary-right)
                            (left ?x-population-of-?y\(?y\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn ?x-population-of-?y-cxn-2
             ((?x-population-of-?y-unit
               (subunits (?x-population-of-?y\(?x\)-unit ?x-population-of-?y\(?y\)-unit ?x-population-of-?y-optional-signs-unit))
               (unit-cat x-population-of-?y-cat)
               (args ((scope ?f)
                      (target ?a)))
               (boundaries ((left ?x-population-of-?y\(?y\)-left-boundary)
                            (right ?pt-states-1))))
              (?x-population-of-?y-optional-signs-unit
               (form ((adjacent ?x-population-of-?y\(?x\)-boundary-right ?personne))))
              <-
              (?x-population-of-?y-unit
               (HASH meaning ((population ?g ?c ?b)))
               --
               (HASH form ((two-hand-articulation ?personne "")
                           (right-hand-articulation ?pt-states-1 "")
                           (adjacent ?personne ?pt-states-1)
                           (adjacent ?x-population-of-?y\(?y\)-boundary-right ?x-population-of-?y\(?x\)-boundary-left))))
              (?x-population-of-?y\(?x\)-unit
               (unit-cat x-population-of-?y\(?x\)-cat)
               (args ((scope ?f)
                      (target ?a)
                      (source ?b)
                      (source-2 ?g)))
               --
               (unit-cat x-population-of-?y\(?x\)-cat)
               (boundaries ((right ?x-population-of-?y\(?x\)-boundary-right)
                            (left ?x-population-of-?y\(?x\)-boundary-left)))
               )
              (?x-population-of-?y\(?y\)-unit
               (unit-cat x-population-of-?y\(?y\)-cat)
               (args ((scope ?g)
                      (target ?c)))
               --
               (unit-cat x-population-of-?y\(?y\)-cat)
               (boundaries ((right ?x-population-of-?y\(?y\)-boundary-right)
                            (left ?x-population-of-?y\(?y\)-boundary-left))))
              )
             :cxn-inventory *geoquery-lsfb*)

