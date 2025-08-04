(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn riverid-cxn
             ((?river-unit
               (footprints (identified))
               (args ((target ?g)
                      (scope ?f))))
              <-
              (?river-unit
               (args ((target ?g)))
               (HASH meaning ((riverid ?f ?g)))
               (sem-cat (identified-category river)
                        (identified yes))
               --
               (footprints (not identified))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(riviere\,?pt\,?riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?pt-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?riviere)
                            (right ?riviere-right-boundary))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((two-hand-articulation ?riviere "")
                           (adjacent ?riviere ?pt-unit)
                           (adjacent ?pt-unit ?river-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(riviere\,?riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?riviere)
                            (right ?riviere-right-boundary))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((two-hand-articulation ?riviere "")
                           (adjacent ?riviere ?river-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(riviere\,?riviere\,riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?riviere-1)
                            (right ?riviere-right-boundary))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((two-hand-articulation ?riviere-1 "")
                           (two-hand-articulation ?riviere-2 "")
                           (adjacent ?riviere ?river-left-boundary)
                           (adjacent ?river-right-boundary ?riviere-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(eau\,riviere\,?ds\,?riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?ds-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?eau)
                            (right ?riviere-right-unit))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?eau "")
                           (two-hand-articulation ?riviere "")
                           (adjacent ?eau ?riviere)
                           (adjacent ?riviere ?ds-left)
                           (adjacent ?ds-right ?river-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(il-y-a\,riviere\,?riviere\,riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?il-y-a)
                            (right ?riviere-2))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?riviere-1 "")
                           (two-hand-articulation ?riviere-2 "")
                           (adjacent ?il-y-a ?riviere-1)
                           (adjacent ?riviere-1 ?river-left-boundary)
                           (adjacent ?river-right-boundary ?riviere-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn const-?river-cxn\(eau\,riviere\,?pt\,?riviere\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?eau)
                            (right ?pt-unit-2))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?eau "")
                           (two-hand-articulation ?riviere "")
                           (adjacent ?eau ?riviere)
                           (adjacent ?riviere ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?river-left-boundary)
                           (adjacent ?river-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn const-?river-cxn\(eau\,riviere\,?pt\,?riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?pt-unit-1))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?eau)
                            (right ?river-right-boundary))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?eau "")
                           (two-hand-articulation ?riviere "")
                           (adjacent ?eau ?riviere)
                           (adjacent ?riviere ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?river-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?river-cxn\(il-y-a\,riviere\,eau\,?pt\,riviere\,?pt\,nom\,?riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?il-y-a)
                            (right ?river-right-boundary))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?riviere-1 "")
                           (right-hand-articulation ?eau "")
                           (two-hand-articulation ?riviere-2 "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?il-y-a ?riviere-1)
                           (adjacent ?riviere-1 ?eau)
                           (adjacent ?eau ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?riviere-2)
                           (adjacent ?riviere-2 ?pt-unit-2)
                           (adjacent ?pt-unit-2 ?nom)
                           (adjacent ?nom ?river-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn const-?river-cxn\(riviere\,nom\,?riviere\,?pt\,riviere\)
             ((?const-unit
               (syn-cat np)
               (subunits (?riviere-unit ?pt-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?riviere-1)
                            (right ?riviere-2))))
              (?riviere-unit
                (footprints (const)))
              <-
              (?riviere-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (sem-cat (identified-category river)
                        (identified yes))
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?river-left-boundary)
                            (right ?river-right-boundary))))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((two-hand-articulation ?riviere-1 "")
                           (two-hand-articulation ?nom "")
                           (two-hand-articulation ?riviere-2 "")
                           (adjacent ?riviere-1 ?nom)
                           (adjacent ?nom ?river-left-boundary)
                           (adjacent ?river-right-boundary ?pt-unit)
                           (adjacent ?pt-unit ?riviere-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)