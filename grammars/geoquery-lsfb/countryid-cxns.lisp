(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn usa-cxn-1
             ((?usa-unit
               (syn-cat noun)
               (sem-cat (identified-category country)
                        (identified yes))
               (footprints (identified))
               (meaning-pred-type full-name)
               (args ((scope ?f)
                      (target ?g)))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (HASH meaning ((usa ?g)
                              (countryid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn usa-cxn-2
             ((?usa-unit
               (syn-cat noun)
               (sem-cat (identified-category country)
                        (identified yes))
               (footprints (identified))
               (meaning-pred-type full-name)
               (args ((scope ?f)
                      (target ?g)))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (HASH meaning ((usa ?g)
                              (countryid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn usa-cxn-3
             ((?usa-unit
               (syn-cat noun)
               (meaning-pred-type none)
               (footprints (identified))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (lex-id amerique)
               --
               (HASH form ((right-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn usa-cxn-4
             ((?usa-unit
               (syn-cat noun)
               (footprints (identified))
               (meaning-pred-type none)
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (lex-id amerique)
               --
               (HASH form ((two-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn const-?country-cxn\(dans\,?country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (sem-cat (referent-class country))
               (subunits (?country-unit ?pt-unit-1 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-1))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-1
               --
               (phonetic-components
                (handshape /)
                (finger-orientation )
                (palm-orientation )
                (location /))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit-1)))))
             :cxn-inventory *geoquery-lsfb*)

; country: 9
; ds: _ | _ | _ | _ | _ --> done
; handshape-right: 
; finger-orientation-right: /
; palm-orientation-right: /
; location-right: /
; movement-right: /
; handshape-left: 
; finger-orientation-left: /
; palm-orientation-left: /
; location-left: 
; movement-left: 


(def-fcg-cxn const-?country-cxn\(dans\,?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (sem-cat (referent-class country))
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (palm-orientation /))
                (non-dominant-hand
                 (handshape )
                 (palm-orientation /)
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb*)

; country: 2
; ds: _ | _ --> done
; handshape-right: 
; extended-finger-direction-right: /
; palm-orientation-right: 
; location-right: 
; movement-right: /
; handshape-left: 
; extended-finger-direction-left: /
; palm-orientation-left: 
; location-left: 
; movement-left: 

(def-fcg-cxn const-?country-cxn\(?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (sem-cat (referent-class country))
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?country-left-boundary)
                            (right ?ds-right))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement /))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb*)

;country: 2
; ds: _ | _ --> done
; handshape-right: 
; extended-finger-direction-right: /
; palm-orientation-right: 
; location-right: /
; movement-right: |
; handshape-left: 
; extended-finger-direction-left: /
; palm-orientation-left: 
; location-left: 

(def-fcg-cxn const-?country-cxn\(palm-up\,pays\,?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (sem-cat (referent-class country))
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?ds-right))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location /)
                 (movement /))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb*)

;country: 1
; ds: _ --> done
; pt:  --> done

(def-fcg-cxn const-?country-cxn\(?country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (sem-cat (referent-class country))
               (subunits (?country-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?country-left-boundary)
                            (right ?pt-unit))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
               (?pt-unit
               --
               (phonetic-components
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb*)
