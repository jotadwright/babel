(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn usa-country-cxn-1
             ((?usa-country-unit
               (unit-cat usa-country-cat)
               (subunits (?usa-country-optional-signs-unit))
               (args ((target ?c)
                      (scope ?f)))
               (boundaries ((left ?dans)
                            (right ?usa-pt))))
              (?usa-country-optional-signs-unit
               (form ((two-hand-articulation ?dans "")
                      (right-hand-articulation ?usa-pt "")
                      (adjacent ?dans ?usa))))
              <-
              (?usa-country-unit
               (HASH meaning ((const ?f ?c ?g)
                              (countryid ?g ?h)
                              (usa ?h)))
               --
               (HASH form ((right-hand-articulation ?usa "")
                           (left-hand-articulation ?map-ds "")
                           (adjacent ?usa ?usa-pt)
                           (during ?usa-pt ?map-ds)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn usa-country-cxn-2
             ((?usa-country-unit
               (unit-cat usa-country-cat)
               (subunits (?usa-country-optional-signs-unit))
               (args ((target ?c)
                      (scope ?f)))
               (boundaries ((left ?dans)
                            (right ?usa-pt))))
              (?usa-country-optional-signs-unit
               (form ((right-hand-articulation ?usa-pt "")
                      (left-hand-articulation ?map-ds ""))))
              <-
              (?usa-country-unit
               (HASH meaning ((const ?f ?c ?g)
                              (countryid ?g ?h)
                              (usa ?h)))
               --
               (HASH form ((two-hand-articulation ?usa "")
                           (adjacent ?usa ?usa-pt)
                           (during ?usa-pt ?map-ds)))))
             :cxn-inventory *geoquery-lsfb*)