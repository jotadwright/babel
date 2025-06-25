(in-package :slp)

;; example 1_0_1
(def-fcg-cxn states-next_to-state-cxn-1 
             ((?neighbouring-states-pointing-unit
               (form ((right-hand-articulation ?neighbouring-states-pt "")
                      (during ?neighbouring-states-pt ?reference-state)
                      (adjacent ?neighbouring-states-ds ?neighbouring-states-pt))))
              (?there-is-unit
               (form ((right-hand-articulation ?il-y-a "")
                      (adjacent ?il-y-a ?neighbouring-states-ds))))

              (?states-next_to-state-unit
               (subunits (?there-is-unit ?neighbouring-states-pointing-unit ?reference-state-unit))
               (unit-cat states-next_to-state-cat)
               (boundaries ((left ?reference-state-left)
                            (right ?neighbouring-states-pt)))
               (args ((target ?b)
                      (scope ?e))))
              <-
              (?states-next_to-state-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state-ds "")
                           (during ?neighbouring-states-ds ?reference-state-ds)
                           (adjacent ?reference-state-right ?il-y-a))))
              (?reference-state-unit
               (unit-cat states-next_to-reference-cat)
               (args ((target ?c)
                      (scope ?e)))
               --
               (unit-cat states-next_to-reference-cat)
               (boundaries ((right ?reference-state-right)
                            (left ?reference-state-left)))))
             :cxn-inventory *geoquery-lsfb*)

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

;; example 48_0_414
(def-fcg-cxn state-next_to-cxn-3
             ((?extra-signs
               (form ((right-hand-articulation ?autre "")
                      (two-hand-articulation ?pays "")
                      (right-hand-articulation ?un "")
                      (right-hand-articulation ?neighbouring-states-pt "")
                      (during ?neighbouring-states-pt ?reference-state)
                      (during ?un ?reference-state)
                      (adjacent ?autre ?pays)
                      (adjacent ?pays ?neighbouring-states-ds)
                      (adjacent ?neighbouring-states-ds ?un)
                      (adjacent ?un ?neighbouring-states-pt))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
                           (during ?neighbouring-states-ds ?reference-state)))))
             :cxn-inventory *geoquery-lsfb*)

;; example 57_0_485
(def-fcg-cxn state-next_to-cxn-4
             ((?extra-signs
               (form ((right-hand-articulation ?a-cote "")
                      (during ?a-cote ?reference-state)
                      (adjacent ?a-cote ?neighbouring-states-ds))))

              (?state-next_to-unit
               (subunits (?extra-signs)))
              <-
              (?state-next_to-unit
               (HASH meaning ((state ?e ?b)
                              (next_to ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?neighbouring-states-ds "")
                           (left-hand-articulation ?reference-state "")
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