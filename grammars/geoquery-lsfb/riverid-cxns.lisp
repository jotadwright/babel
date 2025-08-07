(in-package :geoquery-lsfb-grammar-copy)

(loop for river in *geoquery-rivers*
      for river-meaning = (replace-spaces (cdr (assoc :name river)) :replacer "_")
      for hamnosys = (make-fingerspelling river-meaning)
      for construction-name = (read-from-string (format nil "~a-cxn-1" river-meaning))
      for river-fcg-tag = (read-from-string (format nil "?fs-~a" river-meaning))
      for river-unit-name = (read-from-string (format nil "?~a-unit" river-meaning))
      for sem-categories = (append '(river)(determine-additional-categories river-meaning :type 'river))
      for possible-category-len = (length sem-categories)
      when (not (member 'river sem-categories))
        do (eval
            `(def-fcg-cxn ,construction-name
                          ((,river-unit-name
                            (syn-cat noun)
                            ,(if (eql possible-category-len 1)
                               `(sem-cat
                                 (identified-category river)
                                 (identified yes))
                               `(sem-cat
                                 (possible-categories ,sem-categories)
                                 (identified no)))
                            (footprints
                             ,(if (eql possible-category-len 1)
                                `(identified)
                                `()))
                            (args
                             ,(if (eql possible-category-len 1)
                                `((target ?f)
                                  (scope ?e))
                                `((target ?f))))
                            (boundaries ((left ,river-fcg-tag)
                                         (right ,river-fcg-tag))))
                           <-
                           (,river-unit-name
                            (HASH meaning
                                  ,(if (eql possible-category-len 1)
                                     `((,(read-from-string river-meaning) ?f)
                                       (riverid ?e ?f))
                                     `((,(read-from-string river-meaning) ?f))))
                            --
                            (HASH form ((right-hand-articulation ,river-fcg-tag ,hamnosys)))))
                          :cxn-inventory *geoquery-lsfb*)))


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
             :cxn-inventory *geoquery-lsfb*)

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
             :cxn-inventory *geoquery-lsfb*)

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
             :cxn-inventory *geoquery-lsfb*)

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
             :cxn-inventory *geoquery-lsfb*)

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
             :cxn-inventory *geoquery-lsfb*)


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
             :cxn-inventory *geoquery-lsfb*)


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
             :cxn-inventory *geoquery-lsfb*)

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
             :cxn-inventory *geoquery-lsfb*)


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
             :cxn-inventory *geoquery-lsfb*)
