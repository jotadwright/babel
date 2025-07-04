(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn highest-points-of-?x-cxn-1
             ((?highest-points-of-?x-unit
               (subunits (?highest-points-of-?x\(?x\)))
               (unit-cat highest-points-of-?x-cat)
               (args ((target ?a)
                      (scope ?e)))
               (boundaries ((left ?highest-points-of-?x\(?x\)-boundary-left)
                            (right ?haut))))
              <-
              (?highest-points-of-?x-unit
               (HASH meaning ((high_point ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?haut "")
                           (adjacent ?highest-points-of-?x\(?x\)-boundary-right ?haut))))
              (?highest-points-of-?x\(?x\)
               (unit-cat highest-points-of-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?b)))
               --
               (unit-cat highest-points-of-?x\(?x\)-cat)
               (boundaries ((right ?highest-points-of-?x\(?x\)-boundary-right)
                            (left ?highest-points-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn highest-place-of-?x-cxn-1
             ((?highest-place-of-?x-unit
               (subunits (?highest-place-of-?x\(?x\)))
               (unit-cat highest-place-of-?x-cat)
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?highest-place-of-?x\(?x\)-boundary-left)
                            (right ?haut))))
              <-
              (?highest-place-of-?x-unit
               (HASH meaning ((highest ?e ?a ?f)
                              (place ?f ?a)
                              (loc ?f ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (right-hand-articulation ?haut "")
                           (adjacent ?plus ?haut)
                           (adjacent ?highest-place-of-?x\(?x\)-boundary-right ?plus))))
              (?highest-place-of-?x\(?x\)
               (unit-cat highest-place-of-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?b)))
               --
               (unit-cat highest-place-of-?x\(?x\)-cat)
               (boundaries ((right ?highest-place-of-?x\(?x\)-boundary-right)
                            (left ?highest-place-of-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)
