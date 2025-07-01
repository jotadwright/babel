(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn sum-cxn-1
             ((?sum-unit
               (unit-cat sum-cat)
               (args ((scope ?f)
                      (target ?a)
                      (source ?b)
                      (source-2 ?g)))
               (boundaries ((left ?total)
                            (right ?total))))
              <-
              (?sum-unit
               (HASH meaning ((SUM ?f ?b ?g ?a)))
               --
               (HASH form ((two-hand-articulation ?total "")))))
             :cxn-inventory *geoquery-lsfb*)