(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn major-?x-cxn-1 
             ((?major-?x-optional-signs-unit
               (form ((adjacent ?major-?x\(?x\)-boundary-right ?important))))
              (?major-?x-unit
               (subunits (?major-?x\(?x\)-unit ?major-?x-optional-signs-unit))
               (unit-cat major-?x-cat)
               (boundaries ((left major-?x\(?x\)-boundary-left)
                            (right ?important)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?major-?x-unit
               (HASH meaning ((major ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?important ""))))
              (?major-?x\(?x\)-unit
               (unit-cat major-?x\(?x\)-cat)
               (args ((target ?a)
                      (scope ?d)))
               --
               (unit-cat major-?x\(?x\)-cat)
               (boundaries ((right ?major-?x\(?x\)-boundary-right)
                            (left ?major-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn major-?x-cxn-2 
             ((?major-?x-optional-signs-unit
               (form ((adjacent ?major-?x\(?x\)-boundary-right ?grand))))
              (?major-?x-unit
               (subunits (?major-?x\(?x\)-unit ?major-?x-optional-signs-unit))
               (unit-cat major-?x-cat)
               (boundaries ((left major-?x\(?x\)-boundary-left)
                            (right ?grand)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?major-?x-unit
               (HASH meaning ((major ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?grand ""))))
              (?major-?x\(?x\)-unit
               (unit-cat major-?x\(?x\)-cat)
               (args ((target ?a)
                      (scope ?d)))
               --
               (unit-cat major-?x\(?x\)-cat)
               (boundaries ((right ?major-?x\(?x\)-boundary-right)
                            (left ?major-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn major-?x-cxn-3 
             ((?major-?x-optional-signs-unit
               (form ((adjacent ?major-?x\(?x\)-boundary-right ?reconnu))))
              (?major-?x-unit
               (subunits (?major-?x\(?x\)-unit ?major-?x-optional-signs-unit))
               (unit-cat major-?x-cat)
               (boundaries ((left major-?x\(?x\)-boundary-left)
                            (right ?reconnu)))
               (args ((target ?a)
                      (scope ?d))))
              <-
              (?major-?x-unit
               (HASH meaning ((major ?d ?a)))
               --
               (HASH form ((right-hand-articulation ?reconnu ""))))
              (?major-?x\(?x\)-unit
               (unit-cat major-?x\(?x\)-cat)
               (args ((target ?a)
                      (scope ?d)))
               --
               (unit-cat major-?x\(?x\)-cat)
               (boundaries ((right ?major-?x\(?x\)-boundary-right)
                            (left ?major-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)