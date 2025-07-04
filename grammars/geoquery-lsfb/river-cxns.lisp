(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn rivers-traversing-?x-cxn-1 
             ((?rivers-traversing-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (two-hand-articulation ?different "")
                      (right-hand-articulation ?eau "")
                      (left-hand-articulation ?ds-riviere-1-left "")
                      (right-hand-articulation ?ds-riviere-2 "")
                      (right-hand-articulation ?ds-riviere-3 "")
                      (right-hand-articulation ?pt-states "")
                      (left-hand-articulation ?ds-state "")
                      (right-hand-articulation ?ds-riviere-4-right "")
                      (left-hand-articulation ?ds-riviere-4-left "")
                      (right-hand-articulation ?ds-riviere-5-right "")
                      (left-hand-articulation ?ds-riviere-5-left "")
                      (adjacent ?rivers-traversing-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?different)
                      (adjacent ?different ?eau)
                      (adjacent ?eau ?riviere)
                      (adjacent ?riviere ?ds-riviere-1-right)
                      (during ?ds-riviere-1-right ?ds-riviere-1-left)
                      (adjacent ?ds-riviere-1-right ?ds-riviere-2)
                      (adjacent ?ds-riviere-2 ?ds-riviere-3)
                      (during ?ds-riviere-2 ?ds-riviere-1-left)
                      (adjacent ?ds-riviere-3 ?pt-states)
                      (during ?ds-riviere-3 ?ds-state)
                      (during ?pt-states ?ds-state)
                      (adjacent ?pt-states ?ds-riviere-4-right)
                      (during ?ds-riviere-4-right ?ds-riviere-4-left)
                      (adjacent ?ds-riviere-4-right ?ds-riviere-5-right)
                      (during ?ds-riviere-5-right ?ds-riviere-5-left))))
              (?rivers-traversing-?x-unit
               (subunits (?rivers-traversing-?x\(?x\)-unit ?rivers-traversing-?x-optional-signs-unit))
               (unit-cat rivers-traversing-?x-cat)
               (boundaries ((left rivers-traversing-?x\(?x\)-boundary-left)
                            (right ?ds-riviere-5-right)))
               (args ((target ?a)
                      (scope ?f))))
              <-
              (?rivers-traversing-?x-unit
               (HASH meaning ((river ?f ?a)
                              (traverse ?f ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?riviere "")
                           (right-hand-articulation ?ds-riviere-1-right ""))))
              (?rivers-traversing-?x\(?x\)-unit
               (unit-cat rivers-traversing-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?f)))
               --
               (unit-cat rivers-traversing-?x\(?x\)-cat)
               (boundaries ((right ?rivers-traversing-?x\(?x\)-boundary-right)
                            (left ?rivers-traversing-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn rivers-traversing-?x-cxn-2
             ((?rivers-traversing-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (two-hand-articulation ?different "")
                      (right-hand-articulation ?ds-riviere-2 "")
                      (right-hand-articulation ?ds-riviere-3 "")
                      (right-hand-articulation ?pt-states "")
                      (left-hand-articulation ?ds-state "")
                      (adjacent ?rivers-traversing-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?different)
                      (adjacent ?different ?riviere-1)
                      (adjacent ?riviere-1 ?ds-riviere-1-right)
                      (during ?ds-riviere-1-right ?ds-riviere-1-left)
                      (adjacent ?ds-riviere-1-right ?ds-riviere-2)
                      (adjacent ?ds-riviere-2 ?ds-riviere-3)
                      (during ?ds-riviere-2 ?ds-riviere-1-left)
                      (adjacent ?ds-riviere-3 ?pt-states)
                      (adjacent ?pt-states ?riviere-2)
                      (during ?ds-riviere-3 ?ds-state)
                      (during ?pt-states ?ds-state))))
              (?rivers-traversing-?x-unit
               (subunits (?rivers-traversing-?x\(?x\)-unit ?rivers-traversing-?x-optional-signs-unit))
               (unit-cat rivers-traversing-?x-cat)
               (boundaries ((left rivers-traversing-?x\(?x\)-boundary-left)
                            (right ?riviere-2)))
               (args ((target ?a)
                      (scope ?f))))
              <-
              (?rivers-traversing-?x-unit
               (HASH meaning ((river ?f ?a)
                              (traverse ?f ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?riviere-1 "")
                           (right-hand-articulation ?ds-riviere-1-right "")
                           (two-hand-articulation ?riviere-2 ""))))
              (?rivers-traversing-?x\(?x\)-unit
               (unit-cat rivers-traversing-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?f)))
               --
               (unit-cat rivers-traversing-?x\(?x\)-cat)
               (boundaries ((right ?rivers-traversing-?x\(?x\)-boundary-right)
                            (left ?rivers-traversing-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn rivers-in-?x-cxn-1 
             ((?rivers-in-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (adjacent ?rivers-in-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?riviere))))
              (?rivers-in-?x-unit
               (subunits (?rivers-in-?x\(?x\)-unit ?rivers-in-?x-optional-signs-unit))
               (unit-cat rivers-in-?x-cat)
               (boundaries ((left ?rivers-in-?x\(?x\)-boundary-left)
                            (right ?riviere)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?rivers-in-?x-unit
               (HASH meaning ((river ?d ?a)
                              (loc ?d ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?riviere ""))))
              (?rivers-in-?x\(?x\)-unit
               (unit-cat rivers-in-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?d)))
               --
               (unit-cat rivers-in-?x\(?x\)-cat)
               (boundaries ((right ?rivers-in-?x\(?x\)-boundary-right)
                            (left ?rivers-in-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn rivers-in-?x-cxn-2 
             ((?rivers-in-?x-unit
               (subunits (?rivers-in-?x\(?x\)-unit))
               (unit-cat rivers-in-?x-cat)
               (boundaries ((left ?rivers-in-?x\(?x\)-boundary-left)
                            (right ?riviere)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?rivers-in-?x-unit
               (HASH meaning ((river ?d ?a)
                              (loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?eau "")
                           (two-hand-articulation ?riviere "")
                           (adjacent ?rivers-in-?x\(?x\)-boundary-right ?il-y-a)
                           (adjacent ?il-y-a ?eau)
                           (adjacent ?eau ?riviere))))
              (?rivers-in-?x\(?x\)-unit
               (unit-cat rivers-in-?x\(?x\)-cat)
               (args ((target ?b)
                      (scope ?d)))
               --
               (unit-cat rivers-in-?x\(?x\)-cat)
               (boundaries ((right ?rivers-in-?x\(?x\)-boundary-right)
                            (left ?rivers-in-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

