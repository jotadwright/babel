(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn has-largest-population-of-?x-cxn-1
             ((?has-largest-population-of-?x-unit
               (subunits (?has-largest-population-of-?x\(?x\)-unit))
               (unit-cat has-largest-population-of-?x-cat)
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?has-largest-population-of-?x\(?x\)-left-boundary)
                            (right ?pt-states-2))))
              (?has-largest-population-of-?x-optional-signs-unit
               (form ((adjacent ?quoi-1 ?plus))))
              <-
              (?has-largest-population-of-?x-unit
               (HASH meaning ((largest ?e ?b ?f)
                              (population ?f ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?quelle "")
                           (right-hand-articulation ?quoi-1 "")
                           (right-hand-articulation ?plus "")
                           (two-hand-articulation ?habiter "")
                           (right-hand-articulation ?quoi-2 "")
                           (adjacent ?has-largest-population-of-?x\(?x\)-boundary-right ?quelle)
                           (adjacent ?quelle ?quoi-1)
                           (adjacent ?plus ?habiter)
                           (adjacent ?habiter ?quoi-2))))
              (?has-largest-population-of-?x\(?x\)-unit
               (unit-cat has-largest-population-of-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat has-largest-population-of-?x\(?x\)-cat)
               (boundaries ((right ?has-largest-population-of-?x\(?x\)-boundary-right)
                            (left ?has-largest-population-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn has-largest-population-of-?x-cxn-2
             ((?has-largest-population-of-?x-unit
               (subunits (?has-largest-population-of-?x\(?x\)-unit ?has-largest-population-of-?x-optional-signs-unit))
               (unit-cat has-largest-population-of-?x-cat)
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?has-largest-population-of-?x\(?x\)-left-boundary)
                            (right ?habiter))))
              (?has-largest-population-of-?x-optional-signs-unit
               (form ((adjacent ?has-largest-population-of-?x\(?x\)-boundary-right ?beaucoup-1))))
              <-
              (?has-largest-population-of-?x-unit
               (HASH meaning ((largest ?e ?b ?f)
                              (population ?f ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?beaucoup-1 "")
                           (right-hand-articulation ?beaucoup-2 "")
                           (two-hand-articulation ?personne "")
                           (two-hand-articulation ?habiter "")
                           (adjacent ?beaucoup-1 ?beaucoup-2)
                           (adjacent ?beaucoup-2 ?personne)
                           (adjacent ?personne ?habiter))))
              (?has-largest-population-of-?x\(?x\)-unit
               (unit-cat has-largest-population-of-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat has-largest-population-of-?x\(?x\)-cat)
               (boundaries ((right ?has-largest-population-of-?x\(?x\)-boundary-right)
                            (left ?has-largest-population-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)



(def-fcg-cxn ?summed-population-of-?x-cxn-2
             ((?summed-population-of-?x-unit
               (subunits (?summed-population-of-?x\(?x\)-unit ?summed-population-of-?x-optional-signs-unit))
               (unit-cat summed-population-of-?x-cat)
               (args ((scope ?f)
                      (target ?a)))
               (boundaries ((left ?summed-population-of-?x\(?x\)-left-boundary)
                            (right ?pt-states-1))))
              (?summed-population-of-?x-optional-signs-unit
               (form ((adjacent ?summed-population-of-?x\(?x\)-boundary-right ?total)
                      (adjacent ?total ?personne))))
              <-
              (?summed-population-of-?x-unit
               (HASH meaning ((sum ?f ?b ?g ?a) 
                              (population ?g ?c ?b)))
               --
               (HASH form ((two-hand-articulation ?total "")
                           (two-hand-articulation ?personne "")
                           (right-hand-articulation ?pt-states-1 "")
                           (adjacent ?personne ?pt-states-1))))
              (?summed-population-of-?x\(?x\)-unit
               (unit-cat summed-population-of-?x\(?x\)-cat)
               (args ((scope ?g)
                      (target ?c)))
               --
               (unit-cat summed-population-of-?x\(?x\)-cat)
               (boundaries ((right ?summed-population-of-?x\(?x\)-boundary-right)
                            (left ?summed-population-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn population-of-?x-cxn-2
             ((?population-of-?x-unit
               (subunits (?population-of-?x\(?x\)-unit ?population-of-?x-optional-signs-unit))
               (unit-cat population-of-?x-cat)
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?population-of-?x\(?x\)-left-boundary)
                            (right ?personne))))
              (?population-of-?x-optional-signs-unit
               (form ((adjacent ?population-of-?x\(?x\)-boundary-right ?combien))))
              <-
              (?population-of-?x-unit
               (HASH meaning ((population ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?combien "")
                           (two-hand-articulation ?personne "")
                           (adjacent ?combien ?personne))))
              (?population-of-?x\(?x\)-unit
               (unit-cat population-of-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?b)))
               --
               (unit-cat population-of-?x\(?x\)-cat)
               (boundaries ((right ?population-of-?x\(?x\)-boundary-right)
                            (left ?population-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)



