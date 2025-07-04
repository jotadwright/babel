(in-package :geoquery-lsfb-grammar)

;; example 1_0_1
(def-fcg-cxn states-next-to-?x-cxn-1 
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (adjacent ?il-y-a ?neighbouring-states-ds)
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                      (adjacent ?neighbouring-states-pt ?reference-state-ds)
                      (left-hand-articulation ?reference-state-ds "")
                      (during ?neighbouring-states-ds ?reference-state-ds)
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?il-y-a))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?neighbouring-states-pt)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds ""))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn states-next-to-?x-cxn-2
             ((?states-next-to-?x-optional-signs-unit
               (form ((two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (left-hand-articulation ?reference-state-ds "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?different)
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                      (during ?neighbouring-states-ds ?reference-state-ds))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?neighbouring-states-pt)))
               (args ((target ?c)
                      (scope ?g))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?g ?c)
                              (next_to ?g ?d ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           )))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?d)
                      (scope ?g)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn states-next-to-?x-cxn-3
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (right-hand-articulation ?reference-state-pt "")
                      (two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (left-hand-articulation ?reference-state-ds-1 "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (left-hand-articulation ?reference-state-ds-2 "")
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?reference-state-pt)
                      (adjacent ?reference-state-pt ?different)
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                      (during ?il-y-a ?reference-state-ds-1)
                      (during ?reference-state-pt ?reference-state-ds-1)
                      (during ?neighbouring-states-ds ?reference-state-ds-2)
                      (during ?neighbouring-states-pt ?reference-state-ds-2))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?neighbouring-states-pt)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           )))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn states-next-to-?x-cxn-4
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?a-cote "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (left-hand-articulation ?reference-state-ds "")
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?a-cote)
                      (adjacent ?a-cote ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                      (during ?a-cote ?reference-state-ds)
                      (during ?neighbouring-states-ds ?reference-state-ds)
                      (during ?neighbouring-states-pt ?reference-state-ds))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?neighbouring-states-pt)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds ""))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn states-next-to-?x-cxn-5
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?reference-state-pt "")
                      (right-hand-articulation ?il-y-a "")
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?reference-state-pt)
                      (adjacent ?reference-state-pt ?il-y-a)
                      (adjacent ?il-y-a ?a-cote-1)
                      (adjacent ?a-cote-1 ?a-cote-2))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?a-cote-2)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?a-cote-1 "")
                           (right-hand-articulation ?a-cote-2 ""))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn states-next-to-?x-cxn-6
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?reference-state-pt "")
                      (left-hand-articulation ?reference-state-ds-1 "")
                      (right-hand-articulation ?il-y-a "")
                      (two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (left-hand-articulation ?reference-state-ds-2 "")
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?reference-state-pt)
                      (adjacent ?reference-state-pt ?il-y-a)
                      (during ?reference-state-pt ?reference-state-ds-1)
                      (adjacent ?il-y-a ?different)
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (during ?neighbouring-states-ds ?reference-state-ds-2))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x\(?x\)-unit ?states-next-to-?x-optional-signs-unit))
               (unit-cat states-next-to-?x-cat)
               (boundaries ((left ?states-next-to-?x\(?x\)-boundary-left)
                            (right ?neighbouring-states-ds)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next-to-?x-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds ""))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn states-next-to-cxn-1
             ((?states-next-to-optional-signs-unit
               (form ((left-hand-articulation ?reference-state-ds "")
                      (during ?neighbouring-states-ds ?reference-state-ds))))
              (?states-next-to-unit
               (subunits (?states-next-to-optional-signs-unit))
               (unit-cat states-next-to-cat)
               (boundaries ((left ?neighbouring-states-ds)
                            (right ?neighbouring-states-ds)))
               (args ((target ?a)
                      (scope ?e)
                      (source ?b))))
              <-
              (?states-next-to-unit
               (HASH meaning ((state ?e ?a)
                              (next_to ?e ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           ))))
             :cxn-inventory *geoquery-lsfb*)

