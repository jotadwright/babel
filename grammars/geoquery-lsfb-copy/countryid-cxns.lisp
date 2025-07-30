(in-package :geoquery-lsfb-grammar-copy)

#|
(def-fcg-cxn countryid-usa-cxn\(dans\,usa\,ds\,pt\,pt\)
             ((?state-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?ds-unit ?pt-unit))
               (args ((scope ?f)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?state-unit
               (HASH meaning ((const ?f ?c ?g)
                              (countryid ?g ?h)
                              (usa ?h)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?usa "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?ds-left)
                           (adjacent ?ds-right ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?pt-unit-2)))))
              :cxn-inventory *geoquery-lsfb-copy*)
|#