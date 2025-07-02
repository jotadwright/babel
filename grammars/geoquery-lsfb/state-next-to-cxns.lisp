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
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
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
                            (right ?a-cote-2)))
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

#|
;; example 48_0_414
(def-fcg-cxn states-next-to-?x-cxn-2
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (right-hand-articulation ?autre "")
                      (two-hand-articulation ?pays "")
                      (right-hand-articulation ?un "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (during ?neighbouring-states-pt ?reference-state)
                      (during ?un ?reference-state)
                      (adjacent ?il-y-a ?autre)
                      (adjacent ?autre ?pays)
                      (adjacent ?states-next-to-?x\(?x\)-boundary-right ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?un)
                      (adjacent ?un ?neighbouring-states-pt))))
              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x-optional-signs-unit ?states-next-to-?x\(?x\)-unit))
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
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)



;; example 57_0_485
(def-fcg-cxn states-next-to-?x-cxn-4
             ((?states-next-to-?x-optional-signs-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (right-hand-articulation ?a-cote "")
                      (adjacent ?il-y-a ?a-cote)
                      (adjacent ?a-cote ?neighbouring-states-ds))))

              (?states-next-to-?x-unit
               (subunits (?states-next-to-?x-optional-signs-unit ?states-next-to-?x\(?x\)-unit))
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
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (adjacent ?states-next-to-?x\(?x\)-boundary-right ?il-y-a))))
              (?states-next-to-?x\(?x\)-unit
               (unit-cat states-next-to-?x\(?x\)-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next-to-?x\(?x\)-cat)
               (boundaries ((right ?states-next-to-?x\(?x\)-boundary-right)
                            (left ?states-next-to-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)
|#
#|

;; example 43_0_386
(def-fcg-cxn state-next_to-cxn-2
             ((?extra-signs
               (form ((two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (during ?neighbouring-states-pt ?reference-state)
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)))))
             :cxn-inventory *geoquery-lsfb*)






;; example 71_0_597
(def-fcg-cxn state-next_to-cxn-5
             ((?extra-signs
               (form ((two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           ))))
             :cxn-inventory *geoquery-lsfb*)

;; example 75_0_633
(def-fcg-cxn state-next_to-cxn-6
             ((?extra-signs
               (form ((two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 76_0_637
(def-fcg-cxn state-next_to-cxn-7
             ((?extra-signs
               (form ((right-hand-articulation ?reference-state-pt "")
                      (two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (during ?neighbouring-states-pt ?reference-state)
                      (adjacent ?reference-state-pt ?different)
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 76_1_754
(def-fcg-cxn state-next_to-cxn-8
             ((?extra-signs
               (form ((right-hand-articulation ?a-cote "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (adjacent ?a-cote ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                      (during ?a-cote ?reference-state-ds)
                      (during ?neighbouring-states-pt ?reference-state-ds))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 87_0_801
(def-fcg-cxn state-next_to-cxn-9
             ((?extra-signs
               (form ((two-hand-articulation ?different "")
                      (two-hand-articulation ?pays "")
                      (adjacent ?different ?pays)
                      (adjacent ?pays ?neighbouring-states-ds))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((left-hand-articulation ?reference-state-ds "")
                           (right-hand-articulation ?neighbouring-states-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 76_2_806
(def-fcg-cxn state-next_to-cxn-10
             ((?extra-signs
               (form ((right-hand-articulation ?reference-state-pt "")
                      (right-hand-articulation ?il-y-a "")
                      (adjacent ?reference-state-ds-1 ?reference-state-pt)
                      (adjacent ?reference-state-pt ?il-y-a)
                      (adjacent ?il-y-a ?a-cote-1)
                      (during ?reference-state-pt ?reference-state-ds-2)
                      (during ?il-y-a ?reference-state-ds-2))))
              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?reference-state-ds-1 "")
                           (left-hand-articulation ?reference-state-ds-2 "")
                           (right-hand-articulation ?a-cote-1 "")
                           (right-hand-articulation ?a-cote-2 "")
                           (adjacent ?a-cote-1 ?a-cote-2)
                           (during ?reference-state-ds-1 ?reference-state-ds-2)
                           (during ?a-cote-1 ?reference-state-ds-2)
                           (during ?a-cote-2 ?reference-state-ds-2)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 88_0_809
(def-fcg-cxn state-next_to-cxn-11
             ((?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds-1 "") ))))
             :cxn-inventory *geoquery-lsfb*)

;; example 88_1_810
(def-fcg-cxn state-next_to-cxn-12
             ((?extra-signs
               (form ((two-hand-articulation ?different "")
                      (adjacent ?different ?neighbouring-states-ds))))
              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)
                           ))))
             :cxn-inventory *geoquery-lsfb*)

;; example 76_3_852
(def-fcg-cxn state-next_to-cxn-13
             (
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?reference-state-ds-1 "")
                           (left-hand-articulation ?reference-state-ds-2 "")
                           (right-hand-articulation ?a-cote-1 "")
                           (right-hand-articulation ?a-cote-2 "")
                           (adjacent ?reference-state-ds-1 ?a-cote-1)
                           (adjacent ?a-cote-1 ?a-cote-2)
                           (during ?reference-state-ds-1 ?reference-state-ds-2)
                           (during ?a-cote-1 ?reference-state-ds-2)
                           (during ?a-cote-2 ?reference-state-ds-2)))))
             :cxn-inventory *geoquery-lsfb*)


 (def-fcg-cxn state-next_to-cxn-1 
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (during ?neighbouring-states-pt ?reference-state)
                           (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-2
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (during ?neighbouring-states-pt ?reference-state)
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?neighbouring-states-ds)
                           (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn state-next_to-cxn-3
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (right-hand-articulation ?autre "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (during ?neighbouring-states-pt ?reference-state)
                           (during ?un ?reference-state)
                           (adjacent ?autre ?pays)
                           (adjacent ?pays ?neighbouring-states-ds)
                           (adjacent ?neighbouring-states-ds ?un)
                           (adjacent ?un ?neighbouring-states-pt)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-4
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (right-hand-articulation ?a-cote "")
                           (during ?a-cote ?reference-state)
                           (adjacent ?a-cote ?neighbouring-states-ds)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-5
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?neighbouring-states-ds)
                           (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                           ))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-6
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
                (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                            (left-hand-articulation ?reference-state "")
                            (during ?neighbouring-states-ds ?reference-state)
                            (two-hand-articulation ?different "")
                            (two-hand-articulation ?pays "")
                            (adjacent ?different ?pays)
                            (adjacent ?pays ?neighbouring-states-ds)))))
              :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-7
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           (right-hand-articulation ?reference-state-pt "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (during ?neighbouring-states-pt ?reference-state)
                           (adjacent ?reference-state-pt ?different)
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?neighbouring-states-ds)
                           (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-8
              (<-
               (?state-next_to-unit
                (HASH meaning ((state ?e ?b)
                               (next_to ?e ?b ?c)))
                --
                (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                            (left-hand-articulation ?reference-state-ds "")
                            (during ?neighbouring-states-ds ?reference-state-ds)
                            (right-hand-articulation ?a-cote "")
                       (right-hand-articulation ?neighbouring-states-pt "")
                       (adjacent ?a-cote ?neighbouring-states-ds)
                       (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                       (during ?a-cote ?reference-state-ds)
                       (during ?neighbouring-states-pt ?reference-state-ds)))))
              :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-9
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((left-hand-articulation ?reference-state-ds "")
                           (right-hand-articulation ?neighbouring-states-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?neighbouring-states-ds)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-10
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?reference-state-ds-1 "")
                           (left-hand-articulation ?reference-state-ds-2 "")
                           (right-hand-articulation ?a-cote-1 "")
                           (right-hand-articulation ?a-cote-2 "")
                           (adjacent ?a-cote-1 ?a-cote-2)
                           (during ?reference-state-ds-1 ?reference-state-ds-2)
                           (during ?a-cote-1 ?reference-state-ds-2)
                           (during ?a-cote-2 ?reference-state-ds-2)
                           (right-hand-articulation ?reference-state-pt "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?reference-state-ds-1 ?reference-state-pt)
                           (adjacent ?reference-state-pt ?il-y-a)
                           (adjacent ?il-y-a ?a-cote-1)
                           (during ?reference-state-pt ?reference-state-ds-2)
                           (during ?il-y-a ?reference-state-ds-2)))))
             :cxn-inventory *geoquery-lsfb*)



(def-fcg-cxn state-next_to-cxn-11
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds-1 "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (adjacent ?neighbouring-states-ds-1 ?neighbouring-states-pt)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-12
             (<-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)
                           (two-hand-articulation ?different "")
                           (right-hand-articulation ?neighbouring-states-pt "")
                           (adjacent ?different ?neighbouring-states-ds)
                           (adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                           (during ?neighbouring-states-pt ?reference-state-ds)
                           ))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-next_to-cxn-13
              (<-
               (?state-next_to-unit
                (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?reference-state-ds-1 "")
                           (left-hand-articulation ?reference-state-ds-2 "")
                           (right-hand-articulation ?a-cote-1 "")
                           (right-hand-articulation ?a-cote-2 "")
                           (adjacent ?reference-state-ds-1 ?a-cote-1)
                           (adjacent ?a-cote-1 ?a-cote-2)
                           (during ?reference-state-ds-1 ?reference-state-ds-2)
                           (during ?a-cote-1 ?reference-state-ds-2)
                           (during ?a-cote-2 ?reference-state-ds-2)))))
              :cxn-inventory *geoquery-lsfb*)





(def-fcg-cxn state-next_to-cxn-14
             (<-
              (?state-next-to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           ;(right-hand-articulation ?neighbouring-states-pt "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)
                           ;(during ?neighbouring-states-pt ?reference-state)
                           ;(adjacent ?neighbouring-states-ds ?neighbouring-states-pt)
                           ))))
             :cxn-inventory *geoquery-lsfb*)



(def-fcg-cxn state-next_to-cxn-15
             (<-
              (?reference-state-unit
               --
               (boundaries ((right ?reference-state)))
               (sign-cat reference-state-cat)
               (sign-plane ?plane))
              (?neighbouring-states-unit
               (HASH meaning ((state ?e ?b)))
               --
               (sign-cat neighbouring-states-cat)
               (sign-plane ?plane)
               (boundaries ((left ?neighbouring-states-ds))))
              (?neighbouring-states-next-to-reference-state-unit
               (HASH meaning ((next_to ?e ?b ?c)))
               --
               (HASH form ((during ?neighbouring-states-ds ?reference-state)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-1
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane horizontal))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-2
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-3
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-4
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-5
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-6
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn neighbouring-states-cxn-7
             ((?neighbouring-states-unit
               (footprints (complete))
               (boundaries ((left ?neighbouring-states)))
               (sign-plane vertical))
              <-
              (?neighbouring-states-unit
               (sign-cat neighbouring-states-cat)
               (footprints (not complete))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-1
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane horizontal))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-2
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane horizontal))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-3
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane horizontal))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-4
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane vertical))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-5
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane vertical))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-6
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane vertical))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn reference-state-cxn-7
             ((?reference-state-unit
               (footprints (complete))
               (boundaries ((right ?reference-state)))
               (sign-plane vertical))
              <-
              (?reference-state-unit
               (sign-cat reference-state-cat)
               (footprints (not complete))
               --
               (HASH form ((left-hand-articulation ?reference-state "")))))
             :cxn-inventory *geoquery-lsfb*)
|#

#|

(def-fcg-cxn fs_mississippi-cxn-1
             ((?fs_mississippi-unit
               (sign-cat fs_mississippi-1-cat)
               (sem-class state-entity-cat)
               (number sg)
               (args ((target ?f)
                      (source ?g)))
               (location neutral))
              <-
              (?fs_mississippi-unit
               (HASH meaning ((mississippi ?g)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?fs_mississippi-unit ""))))))
|#