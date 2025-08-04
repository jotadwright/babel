(in-package :geoquery-lsfb-grammar-copy)

(loop for city in *geoquery-cities*
      for city-meaning = (replace-spaces (cdr (assoc :name city)) :replacer "_")
      for hamnosys = (make-fingerspelling city-meaning)
      for construction-name = (read-from-string (format nil "~a-cxn-1" city-meaning))
      for city-fcg-tag = (read-from-string (format nil "?fs-~a" city-meaning))
      for city-unit-name = (read-from-string (format nil "?~a-unit" city-meaning))
      for sem-categories = (append '(city)(determine-additional-categories city-meaning :type 'city))
      for possible-category-len = (length sem-categories)
      when (not (member 'state sem-categories))
        do (eval
            `(def-fcg-cxn ,construction-name
                          ((,city-unit-name
                            (syn-cat noun)
                            (meaning-pred-type full-name)
                            ,(if (eql possible-category-len 1)
                               `(sem-cat
                                 (identified-category city)
                                 (identified yes))
                               `(sem-cat
                                 (possible-categories ,sem-categories)))
                            (footprints
                             ,(if (eql possible-category-len 1)
                                `(identified)
                                `()))
                            (args
                             ,(if (eql possible-category-len 1)
                                `((target ?f)
                                  (scope ?e)
                                  (source ?g))
                                `((target ?f))))
                            (boundaries ((left ,city-fcg-tag)
                                         (right ,city-fcg-tag))))
                           <-
                           (,city-unit-name
                            (HASH meaning
                                  ,(if (eql possible-category-len 1)
                                     `((,(read-from-string city-meaning) ?f)
                                       (cityid ?e ?f ?g))
                                     `((,(read-from-string city-meaning) ?f))))
                            --
                            (HASH form ((right-hand-articulation ,city-fcg-tag ,hamnosys)))))
                          :cxn-inventory *geoquery-lsfb-copy*)))

(def-fcg-cxn des-moines-cxn-2
             ((?des-moines-unit
               (syn-cat noun)
               (sem-cat (identified-category city)
                        (identified yes))
               (meaning-pred-type full-name)
               (footprints (identified))
               (args ((scope ?e)
                      (target ?f)
                      (source ?g)))
               (boundaries ((left ?des)
                            (right ?moine))))
              <-
              (?des-moines-unit
               (HASH meaning ((des_moines ?f)
                              (cityid ?e ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?des "")
                           (two-hand-articulation ?moine "")
                           (adjacent ?des ?moine)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn des-moines-cxn-3
             ((?des-moines-unit
               (syn-cat noun)
               (sem-cat (identified-category city)
                        (identified yes))
               (meaning-pred-type full-name)
               (footprints (identified))
               (args ((scope ?e)
                      (target ?f)
                      (source ?g)))
               (boundaries ((left ?moine)
                            (right ?moine))))
              <-
              (?des-moines-unit
               (HASH meaning ((des_moines ?f)
                              (cityid ?e ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?moine "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn cityid-cxn
             ((?city-unit
               (footprints (identified))
               (args ((source ?g)
                      (scope ?e))))
              <-
              (?city-unit
               (args ((target ?f)))
               (HASH meaning ((cityid ?e ?f ?g)))
               (sem-cat (identified-category city)
                        (identified yes))
               --
               (footprints (not identified))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb-copy*)

; city: 1
; pt-1:  --> done
; pt-2:  --> done
(def-fcg-cxn const-?city-in-?state-cxn\(?pt\,pays\,nom\,?state\,dans\,il-y-a\,ville\,?city\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?city-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              (?state-unit
                (footprints (const)))
              (?city-unit
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
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (meaning-pred-type abbreviation)
               (args ((target ?g)))
               (footprints (not const))
               --
               (meaning-pred-type abbreviation)
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?pt-unit-2
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
               (HASH form ((two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?ville "")
                           (adjacent ?pt-unit-1 ?pays)
                           (adjacent ?pays ?nom)
                           (adjacent ?nom ?state-left-boundary)
                           (adjacent ?state-right-boundary ?dans)
                           (adjacent ?dans ?il-y-a)
                           (adjacent ?il-y-a ?ville)
                           (adjacent ?ville ?city-left-boundary)
                           (adjacent ?city-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

#|

(def-fcg-cxn capital-?city-cxn\(capitale\,nom\,city\,pt\)
             ((?capital-unit
               (syn-cat np)
               (subunits (?city-unit ?pt-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?capitale)
                            (right ?pt-unit))))
              (?city-unit
                (footprints (const)))
              <-
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?capital-unit
               (HASH meaning ((capital ?f ?c)
                              (const ?f ?c ?g)))
               --
               (HASH form ((two-hand-articulation ?capitale "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?capitale ?nom)
                           (adjacent ?nom ?city-left-boundary)
                           (adjacent ?city-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

|#

; city: 1
(def-fcg-cxn capital-?city-cxn\(capitale\,nom\,city\)
             ((?capital-unit
               (syn-cat np)
               (subunits (?city-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              (?city-unit
                (footprints (const)))
              <-
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?capital-unit
               (HASH meaning ((capital ?f ?c)
                              (const ?f ?c ?g)))
               --
               (HASH form ((two-hand-articulation ?capitale "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?capitale ?nom)
                           (adjacent ?nom ?city-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; city: 1
; ds: _ --> done

(def-fcg-cxn capital-?city-cxn\(capitale\,nom\,city\,ds\)
             ((?capital-unit
               (syn-cat np)
               (subunits (?city-unit))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?capitale)
                            (right ?ds-right))))
              (?city-unit
                (footprints (const)))
              <-
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?capital-unit
               (HASH meaning ((capital ?f ?c)
                              (const ?f ?c ?g)))
               --
               (HASH form ((two-hand-articulation ?capitale "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?capitale ?nom)
                           (adjacent ?nom ?city-left-boundary)
                           (adjacent ?city-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)


;city: 1
;ds-1: _ --> done
;ds-2: _ --> done
;pt-1:  --> done
;pt-2:  --> done

(def-fcg-cxn capital-?city-cxn\(?usa\,?ds\,il-y-a\,different\,capitale\,?ds\,un\,?pt\,nom\,?city\,?pt\)
             ((?capital-unit
               (syn-cat np)
               (subunits (?usa-unit ?ds-unit-1 ?ds-unit-2 ?pt-unit-1 ?city-unit ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?usa-left-boundary)
                            (right ?pt-unit))))
              (?city-unit
                (footprints (const)))
              <-
              (?usa-unit
               --
               (lex-id amerique)
               (meaning-pred-type none)
               (boundaries ((left ?usa-left-boundary)
                            (right ?usa-right-boundary))))
              (?ds-unit-1
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-1-left)
                            (right ?ds-1-right))))
              (?ds-unit-2
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-2-left)
                            (right ?ds-2-right))))
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
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?capital-unit
               (HASH meaning ((capital ?f ?c)
                              (const ?f ?c ?g)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?capitale "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?usa-right-boundary ?ds-1-left)
                           (adjacent ?ds-1-right ?il-y-a)
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?capitale)
                           (adjacent ?capitale ?ds-2-left)
                           (adjacent ?ds-2-right ?un)
                           (adjacent ?un ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?nom)
                           (adjacent ?nom ?city-left-boundary)
                           (adjacent ?city-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;city: 1
;pt-1:  --> done
;pt-2:  --> done

(def-fcg-cxn const-?city-cxn\(il-y-a\,different\,ville\,un\,?pt\,nom\,?city\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?city-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-2))))
              (?city-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?ville "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?ville)
                           (adjacent ?ville ?un)
                           (adjacent ?un ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?nom)
                           (adjacent ?nom ?city-left-boundary)
                           (adjacent ?city-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; city: 1
; pt-1: 
; pt-2: 

(def-fcg-cxn const-?city-cxn\(?pt\,ville\,?city\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?city-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              (?city-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?city-unit
               (sem-cat (identified-category city)
                        (identified yes))
               (args ((scope ?e)
                      (source ?g)))
               (footprints (not const))
               --
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?city-left-boundary)
                            (right ?city-right-boundary))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?d ?b ?e)))
               --
               (HASH form ((two-hand-articulation ?ville "")
                           (adjacent ?pt-unit-1 ?ville)
                           (adjacent ?ville ?city-left-boundary)
                           (adjacent ?city-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)