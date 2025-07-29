(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn pt-cxn\(index\,left\,out\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index\,out\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index\,left\,multiple-movement\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn pt-cxn\(index\,left\,out\,movement\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn pt-cxn\(index-bent\,left\,out\,movement\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn pt-cxn\(index\,out\,repeated-movement\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index\,out-left\,movement-multiple\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index\,out-left\,movement-out\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn pt-cxn\(index\,out\,movement-down\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index\,out\,movement-out\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn pt-cxn\(index-bent\,out\,movement-out-multiple\)
             ((?pt-unit
               (footprints (pt)))
              <-
              (?pt-unit
               (syn-cat pointing-sign)
               (footprints (not pt))
               --
               (HASH form ((right-hand-articulation ?pt-unit "")))))
             :cxn-inventory *geoquery-lsfb-copy*)




