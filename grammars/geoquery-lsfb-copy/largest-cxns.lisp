(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn largest-cxn\(grand\)
             ((?pt-unit
               (args ((source ?d)
                      (target ?a)
                      (scope ?c)))
               (boundaries ((left ?grand)
                            (right ?grand))))
              <-
              (?pt-unit
               (syn-cat adjective)
               (HASH meaning ((largest ?c ?a ?d)))
               --
               (HASH form ((two-hand-articulation ?grand "")))))
             :cxn-inventory *geoquery-lsfb-copy*)