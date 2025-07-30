(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn ds-cxn\(landmark-left\,pt\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-out\,pt-flat-sweep\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-left\,pt-flat-sweep\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn ds-cxn\(landmark\,pt-flat-sweep\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(placement\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-out\,pt\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-up\,pt\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(hand-landmark-left\,pt\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(hand-landmark\,pt\)
             ((?ds-unit
               (boundaries ((left ?landmark)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn ds-cxn\(curved-landmark\,placement\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement "")
                           (left-hand-articulation ?landmark "")
                           (during ?placement ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(hand-landmark\,placement\,pt\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement "")
                           (right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (adjacent ?placement ?pt)
                           (during ?placement ?landmark)
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn ds-cxn\(landmark\,placement-multiple\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement "")
                           (left-hand-articulation ?landmark "")
                           (during ?placement ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn ds-cxn\(placement-left-hand\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((left-hand-articulation ?placement "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(c-landmark\,pt\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(placement-both-hands\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement-right "")
                           (left-hand-articulation ?placement-left "")
                           (during ?placement-right ?placement-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(placement-both-hands-c\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement-right "")
                           (left-hand-articulation ?placement-left "")
                           (during ?placement-right ?placement-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-hand\,placement-multiple\)
             ((?ds-unit
               (boundaries ((left ?placement)
                            (right ?placement)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?placement "")
                           (left-hand-articulation ?landmark "")
                           (during ?placement ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark-bent\,pt\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn ds-cxn\(landmark\,pt-flat-sweep\)
             ((?ds-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (footprints (ds)))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?pt "")
                           (left-hand-articulation ?landmark "")
                           (during ?pt ?landmark)))))
             :cxn-inventory *geoquery-lsfb-copy*)


#|

(def-fcg-cxn ds-phrase-cxn\(landmark\,pt\,pt\)
             ((?ds-phrase-unit
               (boundaries ((left ?pt)
                            (right ?pt)))
               (unit-type phrase)
               (footprints (ds-phrase)))
              <-
              (?landmark-unit
               --
               (syn-cat depicting-sign)
               (sem-cat landmark))
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?ds-phrase-unit
               (syn-cat depicting-phrase)
               (footprints (not ds-phrase))
               --
               (HASH form ((during ?pt-unit-1 ?landmark-unit)
                           (during ?pt-unit-2 ?landmark-unit)
                           (adjacent ?pt-unit-1 ?pt-unit-2)))))


(def-fcg-cxn ds-cxn\(bent5\,left\)
             ((?ds-unit
               (footprints (ds))
               (boundaries ((left ?map-ds)
                            (right ?map-ds))))
              <-
              (?ds-unit
               (syn-cat depicting-sign)
               (sem-cat landmark)
               (footprints (not ds))
               --
               (HASH form ((right-hand-articulation ?map-ds "")))))
             :cxn-inventory *geoquery-lsfb-copy*)
|#