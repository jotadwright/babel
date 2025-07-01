(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn state-cxn-1
             ((?state-unit
               (unit-cat state-cat)
               (args ((target ?b)
                      (scope ?e)))
               (boundaries ((left ?il-y-a)
                            (right ?pays))))
              <-
              (?state-unit
               (HASH meaning ((state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?il-y-a ?pays)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn state-cxn-2
             ((?state-unit
               (unit-cat state-cat)
               (args ((target ?b)
                      (scope ?e)))
               (boundaries ((left ?il-y-a)
                            (right ?un))))
              <-
              (?state-unit
               (HASH meaning ((state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?un "")
                           (adjacent ?il-y-a ?pays)
                           (adjacent ?pays ?un)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-with-capital-?x-cxn-1
             ((?state-with-capital-?x-unit
               (unit-cat state-with-capital-?x-cat)
               (subunits (?state-with-capital-?x\(?x\)-unit))
               (args ((target ?c)
                      (scope ?f)))
               (boundaries ((left ?il-y-a)
                            (right ?state-with-capital-?x\(?x\)-boundary-right))))
              <-
              (?state-with-capital-?x-unit
               (HASH meaning ((state ?f ?c)
                              (loc ?f ?d ?c)
                              (capital ?f ?d)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?state-ds-right "")
                           (left-hand-articulation ?state-ds-left "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?state-pt "")
                           (two-hand-articulation ?capitale "")
                           (adjacent ?capitale ?state-with-capital-?x\(?x\)-boundary-left)
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?state-ds-right)
                           (adjacent ?state-ds-right ?un)
                           (adjacent ?un ?state-pt)
                           (adjacent ?state-pt ?capitale)
                           (during ?state-ds-right ?state-ds-left)
                           (during ?un ?state-ds-left)
                           (during ?state-pt ?state-ds-left)
                           )))
              
              (?state-with-capital-?x\(?x\)-unit
               (args ((scope ?f)
                      (target ?d)))
               ;(unit-cat state-with-capital-?x\(?x\)-cat)
               --
               ;(unit-cat state-with-capital-?x\(?x\)-cat)
               (boundaries ((right ?state-with-capital-?x\(?x\)-boundary-right)
                            (left ?state-with-capital-?x\(?x\)-boundary-left))))
              )
             :cxn-inventory *geoquery-lsfb*)