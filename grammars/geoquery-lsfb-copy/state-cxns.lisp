(in-package :geoquery-lsfb-grammar-copy)

#|
(def-fcg-cxn ?x-state-cxn-1
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?usa "")
                           (right-hand-articulation ?pt-usa "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?pt-usa)
                           (adjacent ?pt-usa ?pt-state-1)
                           (adjacent ?pt-state-1 ?il-y-a)
                           (adjacent ?il-y-a ?un)
                           (adjacent ?un ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pays)
                           (adjacent ?pays ?pt-state-2)
                           (during ?pt-usa ?ds-map)
                           (during ?pt-state-1 ?ds-map)
                           (during ?il-y-a ?ds-map)
                           (during ?un ?ds-map)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-4
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?ds-states-1 ?ds-map-1)
                           (during ?un ?ds-map-1)
                           (during ?pt-state-1 ?ds-map-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-2
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?pt-usa "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (left-hand-articulation ?ds-map-2 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?pt-usa)
                           (adjacent ?pt-usa ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?pt-usa ?ds-map-1)
                           (during ?ds-states-1 ?ds-map-2)
                           (during ?un ?ds-map-2)
                           (during ?pt-state-1 ?ds-map-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-3
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?ds-states-1 ?ds-map-1)
                           (during ?un ?ds-map-1)
                           (during ?pt-state-1 ?ds-map-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-5
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?ds-states-1 ?ds-map-1)
                           (during ?un ?ds-map-1)
                           (during ?pt-state-1 ?ds-map-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-6
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?x-state\(?x\)-boundary-right))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (left-hand-articulation ?ds-map-2 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?pt-state-1)
                           (adjacent ?pt-state-1 ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-2)
                           (adjacent ?pt-state-2 ?x-state\(?x\)-boundary-left)
                           (during ?pt-state-1 ?ds-map-1)
                           (during ?il-y-a ?ds-map-1)
                           (during ?ds-states-1 ?ds-map-2)
                           (during ?un ?ds-map-2)
                           (during ?pt-state-2 ?ds-map-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-7
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?x-state\(?x\)-boundary-right))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?ds-states-1 ?ds-map-1)
                           (during ?un ?ds-map-1)
                           (during ?pt-state-1 ?ds-map-1)
                           (during ?beaucoup ?ds-map-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ?x-state-cxn-8
             ((?x-state-unit
               (unit-cat x-state-cat)
               (subunits (?x-state\(?x\)-unit))
               (args ((target ?a)
                      (scope ?c)))
               (boundaries ((left ?dans)
                            (right ?x-state\(?x\)-boundary-right))))
              <-
              (?x-state\(?x\)-unit
               (unit-cat x-state\(?x\)-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               --
               (boundaries ((left ?x-state\(?x\)-boundary-left)
                            (right ?x-state\(?x\)-boundary-right)))
               (unit-cat x-state\(?x\)-cat))
              (?x-state-unit
               (HASH meaning ((state ?d ?a)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?plusieurs "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-map-1 "")
                           (adjacent ?il-y-a ?plusieurs)
                           (adjacent ?plusieurs ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?x-state\(?x\)-boundary-left)
                           (adjacent ?x-state\(?x\)-boundary-right ?pt-state-2)
                           (during ?ds-states-1 ?ds-map-1)
                           (during ?un ?ds-map-1)
                           (during ?pt-state-1 ?ds-map-1)
                           (during ?beaucoup-1 ?ds-map-1)
                           (during ?beaucoup-2 ?ds-map-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn largest-cxn-1
             ((?largest-unit
               (unit-cat largest-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               (boundaries ((left ?grand)
                            (right ?grand))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?c ?a ?d)))
               --
               (HASH form ((two-hand-articulation ?grand "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn largest-area-cxn-1
             ((?largest-area-unit
               (unit-cat largest-area-cat)
               (args ((target ?b)
                      (source ?g)
                      (scope ?f)))
               (boundaries ((left ?grand)
                            (right ?grand))))
              <-
              (?largest-area-unit
               (HASH meaning ((largest ?f ?c ?g)
                              (area ?g ?b ?c)))
               --
               (HASH form ((two-hand-articulation ?grand "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn largest-population-cxn-1
             ((?largest-population-unit
               (unit-cat largest-population-cat)
               (args ((target ?b)
                      (source ?g)
                      (scope ?f)))
               (boundaries ((left ?beaucoup)
                            (right ?habiter))))
              <-
              (?largest-population-unit
               (HASH meaning ((largest ?f ?c ?g)
                              (population ?g ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?beaucoup "")
                           (two-hand-articulation ?habiter "")
                           (adjacent ?beaucoup ?habiter)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn largest-population-cxn-2
             ((?largest-population-unit
               (unit-cat largest-population-cat)
               (args ((target ?b)
                      (source ?g)
                      (scope ?f)))
               (boundaries ((left ?beaucoup-1)
                            (right ?beaucoup-3))))
              <-
              (?largest-population-unit
               (HASH meaning ((largest ?f ?c ?g)
                              (population ?g ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?beaucoup-1 "")
                           (right-hand-articulation ?beaucoup-2 "")
                           (two-hand-articulation ?personne "")
                           (right-hand-articulation ?beaucoup-3 "")
                           (adjacent ?beaucoup-1 ?beaucoup-2)
                           (adjacent ?beaucoup-2 ?personne)
                           (adjacent ?personne ?beaucoup-3)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn smallest-area-cxn-1
             ((?smallest-area-unit
               (unit-cat smallest-area-cat)
               (args ((target ?b)
                      (source ?g)
                      (scope ?f)))
               (boundaries ((left ?plus)
                            (right ?metre-carre))))
              <-
              (?smallest-area-unit
               (HASH meaning ((smallest ?f ?c ?g)
                              (area ?g ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (two-hand-articulation ?petit "")
                           (right-hand-articulation ?peu "")
                           (right-hand-articulation ?metre-carre "")
                           (adjacent ?plus ?petit)
                           (adjacent ?petit ?peu)
                           (adjacent ?peu ?metre-carre)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn smallest-cxn-1
             ((?smallest-unit
               (unit-cat smallest-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               (boundaries ((left ?petit)
                            (right ?petit))))
              <-
              (?smallest-unit
               (HASH meaning ((smallest ?c ?a ?d)))
               --
               (HASH form ((two-hand-articulation ?petit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn smallest-cxn-2
             ((?smallest-unit
               (unit-cat smallest-cat)
               (args ((target ?a)
                      (source ?d)
                      (scope ?c)))
               (boundaries ((left ?plus)
                            (right ?petit-2))))
              <-
              (?smallest-unit
               (HASH meaning ((smallest ?c ?a ?d)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (right-hand-articulation ?petit-1 "")
                           (two-hand-articulation ?petit-2 "")
                           (adjacent ?plus ?petit-1)
                           (adjacent ?petit-1 ?petit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

                     

(def-fcg-cxn state-with-smallest-population-cxn-1
             ((?state-with-smallest-population-unit
               (unit-cat state-with-smallest-population-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?dans)
                            (right ?pt-state-2))))
              <-
              (?state-with-smallest-population-unit
               (HASH meaning ((smallest ?d ?b ?e)
                              (population ?e ?a ?b)
                              (state ?e ?a)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-2 "")
                           (right-hand-articulation ?peu "")
                           (two-hand-articulation ?personne "")
                           (left-hand-articulation ?ds-map-1 "")
                           (left-hand-articulation ?ds-map-2 "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?pt-state-1)
                           (adjacent ?pt-state-1 ?il-y-a)
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?ds-states-1)
                           (adjacent ?ds-states-1 ?un)
                           (adjacent ?un ?pt-state-2)
                           (adjacent ?pt-state-2 ?peu)
                           (adjacent ?peu ?personne)
                           (during ?pt-state-1 ?ds-map-1)
                           (during ?il-y-a ?ds-map-1)
                           (during ?ds-states-1 ?ds-map-2)
                           (during ?un ?ds-map-2)
                           (during ?pt-state-2 ?ds-map-2)
                           (during ?peu ?ds-map-2)))))
             :cxn-inventory *geoquery-lsfb-copy*
             :cxn-set level-1)


(def-fcg-cxn state-with-most-neighbouring-states-cxn-1
             ((?state-with-most-neighbouring-states-unit
               (unit-cat state-with-most-neighbouring-states-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?il-y-a-1)
                            (right ?pt-state-1))))
              <-
              (?state-with-most-neighbouring-states-unit
               (HASH meaning ((most ?d ?a ?b ?e)
                              (state ?e ?a)
                              (next_to ?e ?a ?b)
                              (state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a-1 "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?il-y-a-2 "")
                           (right-hand-articulation ?beaucoup "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?pt-state-1 "")
                           (left-hand-articulation ?state-ds "")
                           (adjacent ?il-y-a-1 ?un)
                           (adjacent ?un ?pays)
                           (adjacent ?pays ?state-ds)
                           (adjacent ?il-y-a-2 ?beaucoup)
                           (adjacent ?beaucoup ?ds-states-1)
                           (adjacent ?ds-states-1 ?pt-state-1)
                           (during ?il-y-a-2 ?state-ds)
                           (during ?beaucoup ?state-ds)
                           (during ?ds-states-1 ?state-ds)
                           (during ?pt-state-1 ?state-ds)))))
             :cxn-inventory *geoquery-lsfb-copy*
             :cxn-set level-1)


(def-fcg-cxn state-with-most-neighbouring-states-cxn-2
             ((?state-with-most-neighbouring-states-unit
               (unit-cat state-with-most-neighbouring-states-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?il-y-a-1)
                            (right ?pt-state-1))))
              <-
              (?state-with-most-neighbouring-states-unit
               (HASH meaning ((most ?d ?a ?b ?e)
                              (state ?e ?a)
                              (next_to ?e ?a ?b)
                              (state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a-1 "")
                           (two-hand-articulation ?pays-1 "")
                           (right-hand-articulation ?il-y-a-2 "")
                           (right-hand-articulation ?plus "")
                           (two-hand-articulation ?different-1 "")
                           (two-hand-articulation ?pays-2 "")
                           (two-hand-articulation ?different-2 "")
                           (right-hand-articulation ?ds-states-1 "")
                           (right-hand-articulation ?pt-state-1 "")
                           (left-hand-articulation ?state-ds-1 "")
                           (left-hand-articulation ?state-ds-2 "")
                           (adjacent ?il-y-a-1 ?pays-1)
                           (adjacent ?pays-1 ?state-ds-1)
                           (adjacent ?il-y-a-2 ?plus)
                           (adjacent ?plus ?different-1)
                           (adjacent ?different-1 ?pays-2)
                           (adjacent ?pays-2 ?different-2)
                           (adjacent ?different-2 ?ds-states-1)
                           (adjacent ?ds-states-1 ?pt-state-1)
                           (during ?il-y-a-2 ?state-ds-1)
                           (during ?plus ?state-ds-1)
                           (during ?ds-states-1 ?state-ds-2)
                           (during ?pt-state-1 ?state-ds-2)))))
             :cxn-inventory *geoquery-lsfb-copy*
             :cxn-set level-1)

|#