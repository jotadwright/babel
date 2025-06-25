(in-package :slp)

(def-fcg-cxn highest-points-of-cxn-1
             ((?highest-points-of-unit
               (subunits (?reference))
               (unit-cat highest-points-of-cat)
               (args ((target ?a)
                      (scope ?e)))
               (boundaries ((left ?reference-boundary-left)
                            (right ?haut))))
              <-
              (?highest-points-of-unit
               (HASH meaning ((high_point ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?haut "")
                           (adjacent ?reference-boundary-right ?haut))))
              (?reference
               (unit-cat highest-points-of-reference-cat)
               (args ((scope ?e)
                      (target ?b)))
               --
               (unit-cat highest-points-of-reference-cat)
               (boundaries ((right ?reference-boundary-right)
                            (left ?reference-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)
