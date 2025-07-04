(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn ?x-has-most-?y-cxn-1
             ((?x-has-most-?y-optional-signs-unit
               (form ((adjacent ?x-has-most-?y\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?beaucoup ?x-has-most-?y\(?y\)-boundary-left))))
              (?x-has-most-?y-unit
               (subunits (?x-has-most-?y\(?x\)-unit ?x-has-most-?y\(?y\)-unit ?x-has-most-?y-optional-signs-unit))
               (unit-cat x-has-most-?y-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?x-has-most-?y\(?x\)-boundary-left)
                            (right ?reference-state-pt))))
              <-
              (?x-has-most-?y-unit
               (HASH meaning ((most ?d ?a ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?beaucoup "")
                           (right-hand-articulation ?reference-state-pt "")
                           (adjacent ?il-y-a ?beaucoup)
                           (adjacent ?x-has-most-?y\(?y\)-boundary-right ?reference-state-pt)
                           )))
              (?x-has-most-?y\(?x\)-unit
               (unit-cat x-has-most-?y\(?x\)-cat)
               (args ((target ?b)
                      (scope ?e)))
               --
               (unit-cat x-has-most-?y\(?x\)-cat)
               (boundaries ((right ?x-has-most-?y\(?x\)-boundary-right)
                            (left ?x-has-most-?y\(?x\)-boundary-left)))
               )
              (?x-has-most-?y\(?y\)-unit
               (unit-cat x-has-most-?y\(?y\)-cat)
               (args ((target ?a)
                      (scope ?e)
                      (source ?b)))
               --
               (unit-cat x-has-most-?y\(?y\)-cat)
               (boundaries ((right ?x-has-most-?y\(?y\)-boundary-right)
                            (left ?x-has-most-?y\(?y\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn ?x-has-most-?y-cxn-2
             ((?x-has-most-?y-optional-signs-unit
               (form ((adjacent ?x-has-most-?y\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?different ?x-has-most-?y\(?y\)-boundary-left))))
              (?x-has-most-?y-unit
               (subunits (?x-has-most-?y\(?x\)-unit ?x-has-most-?y\(?y\)-unit ?x-has-most-?y-optional-signs-unit))
               (unit-cat x-has-most-?y-cat)
               (args ((target ?a)
                      (scope ?d)))
               (boundaries ((left ?x-has-most-?y\(?x\)-boundary-left)
                            (right ?reference-state-pt))))
              <-
              (?x-has-most-?y-unit
               (HASH meaning ((most ?d ?a ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?plus "")
                           (two-hand-articulation ?different "")
                           (right-hand-articulation ?reference-state-pt "")
                           (adjacent ?il-y-a ?plus)
                           (adjacent ?plus ?different)
                           (adjacent ?x-has-most-?y\(?y\)-boundary-right ?reference-state-pt))))
              (?x-has-most-?y\(?x\)-unit
               (unit-cat x-has-most-?y\(?x\)-cat)
               (args ((target ?b)
                      (scope ?e)))
               --
               (unit-cat x-has-most-?y\(?x\)-cat)
               (boundaries ((right ?x-has-most-?y\(?x\)-boundary-right)
                            (left ?x-has-most-?y\(?x\)-boundary-left)))
               )
              (?x-has-most-?y\(?y\)-unit
               (unit-cat x-has-most-?y\(?y\)-cat)
               (args ((target ?a)
                      (scope ?e)
                      (source ?b)))
               --
               (unit-cat x-has-most-?y\(?y\)-cat)
               (boundaries ((right ?x-has-most-?y\(?y\)-boundary-right)
                            (left ?x-has-most-?y\(?y\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)
