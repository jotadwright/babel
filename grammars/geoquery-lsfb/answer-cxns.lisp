(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn answer-?x-cxn-1
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?quoi))))
              (?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?quoi))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?quoi ""))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat)
               (args ((scope ?e)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*
             :score 0.1)

(def-fcg-cxn answer-?x-cxn-2
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?combien))))
              (?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?combien))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?e ?a ?f)))
               --
               (HASH form ((two-hand-articulation ?combien ""))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat-2)
               (args ((scope ?f)
                      (target ?a)))
               --
               (unit-cat answer-?x\(?x\)-cat-2)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*
             :score 0.1)

(def-fcg-cxn answer-?x-cxn-4
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?reference-pt))))
              (?answer-?x-optional-signs-unit
               (form ((adjacent ?combien ?reference-pt)
                      (adjacent ?answer-?x\(?x\)-boundary-right ?combien)
                      (right-hand-articulation ?reference-pt ""))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)
                              (count ?e ?b ?f ?a)))
               --
               (HASH form ((right-hand-articulation ?combien ""))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat-4)
               (args ((scope ?f)
                      (target ?b)))
               --
               (unit-cat answer-?x\(?x\)-cat-4)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*
             :score 0.1)

(def-fcg-cxn answer-?x-cxn-3
             ((?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?kilometre))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)
                              (len ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?distance-ds "")
                           (right-hand-articulation ?combien "")
                           (right-hand-articulation ?kilometre "")
                           (adjacent ?answer-?x\(?x\)-boundary-right ?distance-ds)
                           (adjacent ?distance-ds ?combien)
                           (adjacent ?combien ?kilometre))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat-3)
               (args ((scope ?e)
                      (target ?b)))
               --
               (unit-cat answer-?x\(?x\)-cat-3)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*
             :score 0.1)

(def-fcg-cxn answer-?x-cxn-5
             ((?answer-?x-optional-signs-unit
               (form ((adjacent ?answer-?x\(?x\)-boundary-right ?combien))))
              (?answer-?x-unit
               (subunits (?answer-?x\(?x\)-unit ?answer-?x-optional-signs-unit))
               (boundaries ((left ?answer-?x\(?x\)-boundary-left)
                            (right ?taille))))
              <-
              (?answer-?x-unit
               (HASH meaning ((answer ?d ?a ?e)
                              (size ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?combien "")
                           (right-hand-articulation ?metre-carre "")
                           (two-hand-articulation ?taille "")
                           (adjacent ?combien ?metre-carre)
                           (adjacent ?metre-carre ?taille))))
              (?answer-?x\(?x\)-unit
               (unit-cat answer-?x\(?x\)-cat-5)
               (args ((scope ?e)
                      (target ?b)))
               --
               (unit-cat answer-?x\(?x\)-cat-5)
               (boundaries ((right ?answer-?x\(?x\)-boundary-right)
                            (left ?answer-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*
             :score 0.1)

