(in-package :slp)

(def-fcg-cxn what-cxn-1
             ((?what-unit
               (subunits (?what-reference-unit))
               (boundaries ((left ?reference-left)
                            (right ?quoi))))
              <-
              (?what-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?quoi "")
                           (adjacent ?reference-right ?quoi))))
              (?what-reference-unit
               (unit-cat what-reference-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat what-reference-cat)
               (boundaries ((right ?reference-right)
                            (left ?reference-left)))))
             :cxn-inventory *geoquery-lsfb*)