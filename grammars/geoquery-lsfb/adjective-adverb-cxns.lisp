(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn largest-cxn\(un\,me\,dire\,quoi\,grand\)
             ((?largest-unit
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?un)
                            (right ?grand))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (right-hand-articulation ?me-dire "")
                           (right-hand-articulation ?quoi "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?un ?me-dire)
                           (adjacent ?me-dire ?quoi)
                           (adjacent ?quoi ?grand)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn largest-cxn\(un\,beaucoup\,grand\)
             ((?largest-unit
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?un)
                            (right ?grand))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (right-hand-articulation ?beaucoup "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?un ?beaucoup)
                           (adjacent ?beaucoup ?grand)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn largest-cxn\(un\,?pt\,beaucoup\,grand\)
             ((?largest-unit
               (subunits (?pt-unit))
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?un)
                            (right ?grand))))
              <-
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (right-hand-articulation ?beaucoup "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?beaucoup)
                           (adjacent ?beaucoup ?grand)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn largest-cxn\(plus\,grand\)
             ((?largest-unit
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?plus)
                            (right ?grand))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?plus ?grand)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn smallest-cxn\(plus\,petit-1h\)
             ((?smallest-unit
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?plus)
                            (right ?petit))))
              <-
              (?smallest-unit
               (HASH meaning ((smallest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (right-hand-articulation ?petit "")
                           (adjacent ?plus ?petit)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn smallest-cxn\(plus\,petit-2h\)
             ((?smallest-unit
               (syn-cat comparative-adj)
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?plus)
                            (right ?petit))))
              <-
              (?smallest-unit
               (HASH meaning ((smallest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?plus "")
                           (two-hand-articulation ?petit "")
                           (adjacent ?plus ?petit)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn largest-cxn\(un\,?ds\,grand\)
             ((?largest-unit
               (syn-cat comparative-adj)
               (subunits (?ds-unit))
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?un)
                            (right ?grand))))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?un ?ds-left)
                           (adjacent ?ds-right ?grand)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn largest-cxn\(un\,grand\)
             ((?largest-unit
               (syn-cat comparative-adj)
               (subunits (?ds-unit))
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               (boundaries ((left ?un)
                            (right ?grand))))
              <-
              (?largest-unit
               (HASH meaning ((largest ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (two-hand-articulation ?grand "")
                           (adjacent ?un ?grand)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn count-cxn\(combien\)
             ((?combien-unit
               (args ((scope ?e)
                      (target ?b)
                      (source-1 ?f)
                      (source-2 ?a)))
               (syn-cat adv)
               (boundaries ((left ?combien)
                            (right ?combien))))
              <-
              (?combien-unit
               (HASH meaning ((count ?e ?b ?f ?a)))
               --
               (HASH form ((right-hand-articulation ?combien "")))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn major-cxn\(grand\)
             ((?major-unit
               (args ((scope ?f)
                      (target ?b)))
               (syn-cat adj)
               (boundaries ((left ?grand)
                            (right ?grand))))
              <-
              (?major-unit
               (HASH meaning ((major ?f ?b)))
               --
               (HASH form ((two-hand-articulation ?grand "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn major-cxn\(reconnu\)
             ((?major-unit
               (args ((scope ?f)
                      (target ?b)))
               (syn-cat adj)
               (boundaries ((left ?reconnu)
                            (right ?reconnu))))
              <-
              (?major-unit
               (HASH meaning ((major ?f ?b)))
               --
               (HASH form ((right-hand-articulation ?reconnu "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn major-cxn\(wow\,important\)
             ((?major-unit
               (args ((scope ?f)
                      (target ?b)))
               (syn-cat adj)
               (boundaries ((left ?wow)
                            (right ?important))))
              <-
              (?major-unit
               (HASH meaning ((major ?f ?b)))
               --
               
               (HASH form ((two-hand-articulation ?wow "")
                           (two-hand-articulation ?important "")
                           (adjacent ?wow ?important)))))
             :cxn-inventory *geoquery-lsfb*)
