(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn stateid-cxn
             ((?entity-unit
               (footprints (identified)))
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (subunits (?entity-unit))
               (meaning-pred-type full-name)
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (args ((target ?g)
                      (scope ?f)))
               (syn-cat noun))
              <-
              (?entity-unit
               (args ((target ?g)))
               (HASH meaning ((stateid ?f ?g)))
               (sem-cat (identified-category state)
                        (identified no))
               --
               (sem-cat (possible-categories (state))
                        (identified no))
               (footprints (not identified))
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn cityid-cxn
             ((?entity-unit
               (footprints (identified)))
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (subunits (?entity-unit))
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (args ((source ?g)
                      (scope ?e)))
               (syn-cat noun))
              <-
              (?entity-unit
               (args ((target ?f)))
               (HASH meaning ((cityid ?e ?f ?g)))
               (sem-cat (identified-category city)
                        (identified no))
               --
               (sem-cat (identified-category city)
                        (identified no))
               (footprints (not identified))
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn riverid-cxn
             ((?entity-unit
               (footprints (identified)))
              (?river-unit
               (sem-cat (identified-category river)
                        (identified yes))
               (subunits (?entity-unit))
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (args ((target ?g)
                      (scope ?f)))
               (syn-cat noun))
              <-
              (?river-unit
               (args ((target ?g)))
               (HASH meaning ((riverid ?f ?g)))
               (sem-cat (identified-category river)
                        (identified no))
               --
               (sem-cat (identified-category river)
                        (identified no))
               (footprints (not identified))
               (boundaries ((left ?entity-left)
                            (right ?entity-right)))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb*)