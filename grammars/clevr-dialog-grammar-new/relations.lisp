(in-package :clevr-dialog-grammar)


;; relations mnist dialog
;; above, below, at the right, at the left

(def-fcg-cxn above-cxn
             ((?above-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?above-lex-unit)
                        (rightmost-unit ?above-lex-unit)))
               <-
               (?above-unit
                (HASH meaning ((bind 2D-relation-category ?relation above)))
                --
                (HASH form ((string ?above-unit "above")))))
             :attributes (:meaning above
                          :string "above")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn below-lex-cxn
             ((?below-lex-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?below-lex-unit)
                        (rightmost-unit ?below-lex-unit)))
               <-
               (?below-lex-unit
                (HASH meaning ((bind 2D-relation-category ?relation below)))
                --
                (HASH form ((string ?below-lex-unit "below")))))
             :attributes (:meaning below
                          :string "below")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn at-the-right-lex-cxn
             ((?at-the-right-lex-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?at-unit ?the-unit ?right-unit ?of-unit)))
               <-
               (?at-the-right-lex-unit
                (HASH meaning ((bind 2D-relation-category ?relation 2D-right)))
                --
                (HASH form ((meets ?at-unit ?the-unit)
                            (meets ?the-unit ?right-unit)
                            (meets ?right-unit ?of-unit))))
               (?at-unit
                --
                (HASH form ((string ?at-unit "at"))))
               (?the-unit
                --
                (HASH form ((string ?the-unit "the"))))
               (?right-unit
                --
                (HASH form ((string ?right-unit "right"))))
               (?of-unit
                --
                (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning 2D-right
                          :string "right")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn at-the-left-lex-cxn
             ((?at-the-left-lex-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?at-unit ?the-unit ?left-unit ?of-unit)))
               <-
               (?at-the-left-lex-unit
                (HASH meaning ((bind 2D-relation-category ?relation 2D-left)))
                --
                (HASH form ((meets ?at-unit ?the-unit)
                            (meets ?the-unit ?left-unit)
                            (meets ?left-unit ?of-unit))))
               (?at-unit
                --
                (HASH form ((string ?at-unit "at"))))
               (?the-unit
                --
                (HASH form ((string ?the-unit "the"))))
               (?left-unit
                --
                (HASH form ((string ?left-unit "left"))))
               (?of-unit
                --
                (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning 2D-left
                          :string "left")
             :cxn-inventory *clevr-dialog*)

;; RELATIONS
;; constructions that can be found in dataset: 
;; left & right --> left/right of, to the left/right of, on the left/right side of
;; in front of
;; behind

;;(aslo: right extreme, far right, left extreme, far left)

(def-fcg-cxn behind-lex-cxn
             ((?behind-lex-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?behind-lex-unit)
                        (rightmost-unit ?behind-lex-unit)))
              <-
              (?behind-lex-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)))
               --
               (HASH form ((string ?behind-lex-unit "behind")))))
             :attributes (:meaning behind
                          :string "behind")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn in-front-of-lex-cxn
             ((?in-front-of-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?in-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?in-unit ?front-unit ?of-unit)))
              <-
              (?in-front-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)))
               --
               (HASH form ((meets ?in-unit ?front-unit)
                           (meets ?front-unit ?of-unit))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?front-unit
               --
               (HASH form ((string ?front-unit "front"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning front
                          :string "front")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn right-of-cxn
             ((?right-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?right-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?right-unit ?of-unit)))
              <-
              (?right-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)))
               --
               (HASH form ((meets ?right-unit ?of-unit))))
              (?right-unit
               --
               (HASH form ((string ?right-unit "right"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning right
                          :string "right")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn left-of-cxn
             ((?left-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?left-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?left-unit ?of-unit)))
              <-
              (?left-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)))
               --
               
               (HASH form ((meets ?left-unit ?of-unit))))
              (?left-unit
               --
               (HASH form ((string ?left-unit "left"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning left
                          :string "left")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn to-the-right-of-cxn
             ((?to-the-right-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?to-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?to-unit ?the-unit ?right-unit ?of-unit))) 
              <-
              (?to-the-right-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)))
               --
               (HASH form ((meets ?to-unit ?the-unit)
                           (meets ?the-unit ?right-unit)
                           (meets ?right-unit ?of-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?right-unit
               --
               (HASH form ((string ?right-unit "right"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning right
                          :string "right")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn to-the-left-of-cxn
             ((?to-the-left-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?to-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?to-unit ?the-unit ?left-unit ?of-unit))) 
              <-
              (?to-the-left-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)))
               --
               (HASH form ((meets ?to-unit ?the-unit)
                           (meets ?the-unit ?left-unit)
                           (meets ?left-unit ?of-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?left-unit
               --
               (HASH form ((string ?left-unit "left"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning left
                          :string "left")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn to-the-back-of-cxn
             ((?to-the-back-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?to-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?to-unit ?the-unit ?back-unit ?of-unit)))
              <-
              (?to-the-back-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)))
               --
               (HASH form ((meets ?to-unit ?the-unit)
                           (meets ?the-unit ?back-unit)
                           (meets ?back-unit ?of-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?back-unit
               --
               (HASH form ((string ?back-unit "back"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning behind
                          :string "back")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn to-the-front-of-cxn
             ((?to-the-front-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?to-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?to-unit ?the-unit ?front-unit ?of-unit))) 
              <-
              (?to-the-front-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)))
               --

               (HASH form ((meets ?to-unit ?the-unit)
                           (meets ?the-unit ?front-unit)
                           (meets ?front-unit ?of-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?front-unit
               --
               (HASH form ((string ?front-unit "front"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning front
                          :string "front")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn center-morph-cxn
             ((?center-morph-unit
               (footprints (center)))
              <-
              (?center-morph-unit
               (lex-id center)
               (number singular)
               (footprints (NOT center))
               --
               (HASH form ((string ?center-morph-unit "center")))))
             :attributes (:string "center")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn middle-morph-cxn
             ((?middle-morph-unit
               (footprints (center)))
              <-
              (?middle-morph-unit
               (lex-id center)
               (number singular)
               (footprints (NOT center))
               --
               (HASH form ((string ?middle-morph-unit "middle")))))
             :attributes (:string "middle")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn at-the-center-of-cxn
             ((?at-the-center-of-unit
               (args ((target ?relation))) 
               (sem-cat (sem-class relation)) 
               (syn-cat (syn-class adjective) 
                        (leftmost-unit ?at-unit) 
                        (rightmost-unit ?of-unit))
               (subunits (?center-unit))) 
              <-
              (?at-the-center-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation center)))
               --
               (HASH form ((string ?at-unit "at")
                           (string ?the-unit "the")
                           (string ?of-unit "of")
                           (meets ?at-unit ?the-unit)
                           (meets ?the-unit ?center-unit)
                           (meets ?center-unit ?of-unit))))
              (?center-unit
               --
               (number singular)
               (lex-id center)))
             :attributes (:meaning center
                          :string "at")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn in-the-center-cxn
             ((?in-the-center-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?in-unit)
                        (rightmost-unit ?center-unit))
               (subunits (?in-unit ?the-unit ?center-unit)))
              <-
              (?in-the-center-unit
               (HASH meaning ((bind spatial-relation-category ?relation center)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((string ?in-unit "in")
                           (string ?the-unit "the")
                           (meets ?at-unit ?the-unit)
                           (meets ?the-unit ?center-unit))))
              (?center-unit
               --
               (number singular)
               (lex-id center)))
             :attributes (:meaning center
                          :string "in")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn on-the-right-side-of-cxn
             ((?on-the-right-side-of-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?on-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?on-unit ?the-unit ?right-unit ?side-unit ?of-unit)))
              <-
              (?on-the-right-side-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)))
               --
               (HASH form ((meets ?on-unit ?the-unit)
                           (meets ?the-unit ?right-unit)
                           (meets ?right-unit ?side-unit)
                           (meets ?side-unit ?of-unit))))
              (?on-unit
               --
               (HASH form ((string ?on-unit "on"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?right-unit
               --
               (HASH form ((string ?right-unit "right"))))
              (?side-unit
               --
               (HASH form ((string ?side-unit "side"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning right
                          :string "right")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn on-the-left-side-of-cxn
             ((?on-the-left-side-of-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?on-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?on-unit ?the-unit ?left-unit ?side-unit ?of-unit)))
              <-
              (?on-the-left-side-of-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)))
               --
               (HASH form ((meets ?on-unit ?the-unit)
                           (meets ?the-unit ?left-unit)
                           (meets ?left-unit ?side-unit)
                           (meets ?side-unit ?of-unit))))
              (?on-unit
               --
               (HASH form ((string ?on-unit "on"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?left-unit
               --
               (HASH form ((string ?left-unit "left"))))
              (?side-unit
               --
               (HASH form ((string ?side-unit "side"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :attributes (:meaning left
                          :string "left")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn R-of-all-things
             ((?r-of-all-things-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?things-unit))
               (subunits (?relation-unit ?all-unit ?things-unit)))
              <-
              (?r-of-all-things-unit
               (HASH meaning ((extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?rightmost-relation-unit ?all-unit)
                           (meets ?all-unit ?things-unit))))
              (?relation-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?all-unit
               --
               (HASH form ((string ?all-unit "all"))))
              (?things-unit
               --
               (HASH form ((string ?things-unit "things")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn R-of-all-objects
             ((?r-of-all-things-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?things-unit))
               (subunits (?relation-unit ?all-unit ?things-unit)))
              <-
              (?r-of-all-things-unit
               (HASH meaning ((extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?rightmost-relation-unit ?all-unit)
                           (meets ?all-unit ?things-unit))))
              (?relation-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?all-unit
               --
               (HASH form ((string ?all-unit "all"))))
              (?things-unit
               --
               (HASH form ((string ?things-unit "objects")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn at-the-extreme-right-cxn
             ((?at-the-extreme-right-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?right-unit))
               (subunits (?at-unit ?the-unit ?extreme-unit ?right-unit)))
              <-
              (?at-the-extreme-right-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?at-unit ?the-unit)
                           (meets ?the-unit ?extreme-unit)
                           (meets ?extreme-unit ?right-unit))))
              (?at-unit
               --
               (HASH form ((string ?at-unit "at"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?extreme-unit
               --
               (HASH form ((string ?extreme-unit "extreme"))))
              (?right-unit
               --
               (HASH form ((string ?right-unit "right")))))
             :attributes (:meaning right
                          :string "right")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn at-the-extreme-back-cxn
             ((?at-the-extreme-back-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?back-unit))
               (subunits (?at-unit ?the-unit ?extreme-unit ?back-unit)))
              <-
              (?at-the-extreme-back-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?at-unit ?the-unit)
                           (meets ?the-unit ?extreme-unit)
                           (meets ?extreme-unit ?back-unit))))
              (?at-unit
               --
               (HASH form ((string ?at-unit "at"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?extreme-unit
               --
               (HASH form ((string ?extreme-unit "extreme"))))
              (?back-unit
               --
               (HASH form ((string ?back-unit "back")))))
             :attributes (:meaning behind
                          :string "back")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn at-the-extreme-front-cxn
             ((?at-the-extreme-front-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?front-unit))
               (subunits (?at-unit ?the-unit ?extreme-unit ?front-unit)))
              <-
              (?at-the-extreme-front-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?at-unit ?the-unit)
                           (meets ?the-unit ?extreme-unit)
                           (meets ?extreme-unit ?front-unit))))
              (?at-unit
               --
               (HASH form ((string ?at-unit "at"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?extreme-unit
               --
               (HASH form ((string ?extreme-unit "extreme"))))
              (?front-unit
               --
               (HASH form ((string ?front-unit "front")))))
             :attributes (:meaning front
                          :string "front")
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn at-the-extreme-left-cxn
             ((?at-the-extreme-left-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?at-unit)
                        (rightmost-unit ?right-unit))
               (subunits (?at-unit ?the-unit ?extreme-unit ?left-unit)))
              <-
              (?at-the-extreme-left-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?at-unit ?the-unit)
                           (meets ?the-unit ?extreme-unit)
                           (meets ?extreme-unit ?left-unit))))
              (?at-unit
               --
               (HASH form ((string ?at-unit "at"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?extreme-unit
               --
               (HASH form ((string ?extreme-unit "extreme"))))
              (?left-unit
               --
               (HASH form ((string ?left-unit "left")))))
             :attributes (:meaning left
                          :string "left")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-rightmost-thing-cxn
             ((?the-rightmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?rightmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-rightmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?rightmost-unit)
                           (meets ?rightmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?rightmost-unit
               --
               (HASH form ((string ?rightmost-unit "rightmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "thing")))))
             :attributes (:meaning right
                          :string "rightmost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-rearmost-thing-cxn
             ((?the-rearmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?rearmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-rearmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?rearmost-unit)
                           (meets ?rearmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?rearmost-unit
               --
               (HASH form ((string ?rearmost-unit "rearmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "thing")))))
             :attributes (:meaning behind
                          :string "rearmost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-foremost-thing-cxn
             ((?the-foremost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?foremost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-foremost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?foremost-unit)
                           (meets ?foremost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?foremost-unit
               --
               (HASH form ((string ?foremost-unit "foremost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "thing")))))
             :attributes (:meaning front
                          :string "foremost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-leftmost-thing-cxn
             ((?the-leftmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?leftmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-leftmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?leftmost-unit)
                           (meets ?leftmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?leftmost-unit
               --
               (HASH form ((string ?leftmost-unit "leftmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "thing")))))
             :attributes (:meaning left
                          :string "leftmost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-rightmost-object-cxn
             ((?the-rightmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?rightmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-rightmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?rightmost-unit)
                           (meets ?rightmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?rightmost-unit
               --
               (HASH form ((string ?rightmost-unit "rightmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "object")))))
             :attributes (:meaning right
                          :string "rightmost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-rearmost-object-cxn
             ((?the-rearmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?rearmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-rearmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?rearmost-unit)
                           (meets ?rearmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?rearmost-unit
               --
               (HASH form ((string ?rearmost-unit "rearmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "object")))))
             :attributes (:meaning behind
                          :string "rearmost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-foremost-object-cxn
             ((?the-foremost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?foremost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-foremost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?foremost-unit)
                           (meets ?foremost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?foremost-unit
               --
               (HASH form ((string ?foremost-unit "foremost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "object")))))
             :attributes (:meaning front
                          :string "foremost")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-leftmost-object-cxn
             ((?the-leftmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?leftmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-leftmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?leftmost-unit)
                           (meets ?leftmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?leftmost-unit
               --
               (HASH form ((string ?leftmost-unit "leftmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "object")))))
             :attributes (:meaning left
                          :string "leftmost")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn the-midmost-object-cxn
             ((?the-midmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?leftmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-midmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation center)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?midmost-unit) (meets ?midmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?midmost-unit
               --
               (HASH form ((string ?midmost-unit "midmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "object")))))
             :attributes (:meaning center
                          :string "midmost")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn the-midmost-thing-cxn
             ((?the-midmost-thing-unit
               (args ((target ?target) (source ?source)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               (subunits (?the-unit ?leftmost-unit ?thing-unit))
               (footprints (xmost-thing)))
              <-
              (?the-midmost-thing-unit
               (HASH meaning ((bind spatial-relation-category ?relation center)
                              (extreme-relate ?target ?source ?relation)))
               --
               (HASH form ((meets ?the-unit ?midmost-unit) (meets ?midmost-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?midmost-unit
               --
               (HASH form ((string ?midmost-unit "midmost"))))
              (?thing-unit
               --
               (HASH form ((string ?thing-unit "thing")))))
             :attributes (:meaning center
                          :string "midmost")
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn right-cxn
             ((?right-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?right-unit)
                        (rightmost-unit ?right-unit))
               (sem-cat (sem-class relation)))
              <-
              (?right-unit
               (HASH meaning ((bind spatial-relation-category ?relation right)))
               --
               (HASH form ((string ?right-unit "right")))))
             :attributes (:meaning right
                          :string "right")
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn left-cxn
             ((?left-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?left-unit)
                        (rightmost-unit ?left-unit))
               (sem-cat (sem-class relation)))
              <-
              (?left-unit
               (HASH meaning ((bind spatial-relation-category ?relation left)))
               --
               (HASH form ((string ?left-unit "left")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn front-cxn
             ((?front-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?front-unit)
                        (rightmost-unit ?front-unit))
               (sem-cat (sem-class relation)))
              <-
              (?front-unit
               (HASH meaning ((bind spatial-relation-category ?relation front)))
               --
               (HASH form ((string ?front-unit "front")))))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn behind-cxn
             ((?behind-unit
               (args ((target ?relation)))
               (syn-cat (syn-class noun))
               (sem-cat (sem-class relation)))
              <-
              (?behind-unit
               (HASH meaning ((bind spatial-relation-category ?relation behind)))
               --
               (HASH form ((string ?behind-unit "behind")))))
             :attributes (:meaning behind
                          :string "behind")
             :cxn-inventory *clevr-dialog*)|#



(def-fcg-cxn to-its-R-cxn
             ((?to-its-R-unit
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?to-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (mentioned-object nil)
               (subunits (?to-unit ?possessor-unit ?relation-unit)))
              <-
              (?to-its-R-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation)
                              (find-in-context ?object-set ?segmented-scene ?object)
                              (unique ?unique ?object-set)
                              (get-last-topic ?object ?memory)))
               --
               (HASH form ((meets ?to-unit ?possessor-unit) (meets ?possessor-unit ?leftmost-rel-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?possessor-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?relation-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation))
               --
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn to-its-R-mentioned-object-cxn
             ((?to-its-R-unit
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?to-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (mentioned-object t)
               (subunits (?to-unit ?possessor-unit ?relation-unit)))
              <-
              (?to-its-R-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation)
                              (find-in-context ?object-set ?segmented-scene ?object)
                              (unique ?unique ?object-set)
                              (get-penultimate-topic ?object ?memory)
                              ;(get-memory ?history)
                              ))
               --
               (HASH form ((meets ?to-unit ?possessor-unit) (meets ?possessor-unit ?leftmost-rel-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?possessor-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?relation-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation))
               --
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn to-R-itself-cxn
             ((?to-r-of-itself-unit
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?to-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?to-unit ?relation-unit ?itself-unit)))
              <-
              (?to-r-of-itself-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?to-unit ?leftmost-relation-unit) (meets ?rightmost-relation-unit ?itself-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?relation-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              
              (?itself-unit
               (args ((target ?unique) (source ?segmented-scene) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?itself-unit)
                        (rightmost-unit ?itself-unit)
                        (meaning +))
              --
              
               (args ((target ?unique) (source ?segmented-scene) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?itself-unit)
                        (rightmost-unit ?itself-unit)
                        (meaning +)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn R-of-itself-cxn
             ((?to-R-of-itself-unit
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?to-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?to-unit ?relation-unit ?of-unit ?anaphoric-unit)))
              <-
              (?to-r-of-itself-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation )))
               --
               (HASH form ((meets ?to-unit ?relation-unit) (meets ?relation-unit ?of-unit) (meets ?of-unit ?leftmost-unit))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?relation-unit
               (args ((target ?relation)))
               (syn-cat (syn-class noun))
               (sem-cat (sem-class relation))
               --
               (args ((target ?relation)))
               (syn-cat (syn-class noun))
               (sem-cat (sem-class relation)))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?anaphoric-unit
               (args ((target ?unique) (source ?segmented-scene) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?unique) (source ?segmented-scene) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


;;; RELATION CXN

(def-fcg-cxn R-of-Y-cxn
             ((?R-of-Y-unit
               (args ((target ?target)
                      ;(original-source ?original-input)
                      (source ?source)
                      (relation ?relation)
                     ; (unique ?second-np-target)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-np-unit))
               (multiple-relate t)
               (subunits (?relation-unit ?Y-unit)))
              <-
              (?R-of-Y-unit
               ;(HASH meaning ((relate ?target ?second-np-target ?relation)))
               --
               (HASH form ((meets ?rightmost-relation-unit ?leftmost-unit))))
              (?relation-unit
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?relation)))
               (sem-cat (sem-class relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?Y-unit
               (args ((target ?target) (source ?source)
                     ; (original-source ?original-input)
                      ))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-of-np-unit))
               --
               (args ((target ?target) (source ?source)
                      ;(original-source ?original-input)
                      ))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-of-np-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn X-R-cxn
             ((?X-R-unit
               (args ((target ?first-target)
                      (source ?second-source)
                      ;(original-source ?segmented-scene)
                      ))
               (sem-cat (sem-class immediate-relation)
                        (grammar ?grammar)
                        (relation t))
               (syn-cat (number ?number)
                        (syn-class np)
                        (leftmost-unit ?determiner-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)
                        ;(definiteness ?def)
                        )
               (subunits (?X-unit ?relation-unit))
               )
              <-
              (?X-R-unit
               (HASH meaning ((immediate-relate ?first-source ?second-target ?second-source ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-unit ?leftmost-relation-unit))))
              (?X-unit
               (args ((target ?first-target)
                      (source ?first-source)
                     ; (original-source ?target)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
                        )
               --
               (args ((target ?first-target)
                      (source ?first-source)
                      ;(original-source ?target)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
))
              (?relation-unit
               (args ((target ?second-target)
                     ; (original-source ?segmented-scene)
                      (source ?second-source)
                      (relation ?relation)
                     ; (unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit))
               ;(immediate-relate t)
               --
               ;(immediate-relate t)
               (args ((target ?second-target)
                     ; (original-source ?segmented-scene)
                      (source ?second-source)
                      (relation ?relation)
                      ;(unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


#|(def-fcg-cxn X-plural-R-cxn
             ((?X-R-unit
               (args ((target ?first-np-target) (source ?segmented-scene) (original-source ?original-input)))
               (sem-cat (sem-class immediate-relation)
                        (grammar ?grammar))
               (syn-cat (number ?number)
                        (syn-class np)
                        (leftmost-unit ?determiner-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)
                        ;(definiteness ?def)
                        )
               (subunits (?X-unit ?relation-unit)))
              <-
              (?X-R-unit
               (HASH meaning ((relate ?target ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-unit ?leftmost-relation-unit))))
              (?X-unit
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
                        )
               --
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
))
              (?relation-unit
               (args ((target ?target) (original-source ?original-input) (source ?segmented-scene) (relation ?relation) (unique ?unique)))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit))
               ;(immediate-relate t)
               --
               ;(immediate-relate t)
               (args ((target ?target) (original-source ?original-input) (source ?segmented-scene) (relation ?relation)(unique ?unique)))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)|#



(def-fcg-cxn X-plural-present-R-cxn
             ((?X-R-unit
               (args ((target ?first-np-target) (source ?segmented-scene) (original-source ?original-input)))
               (sem-cat (sem-class immediate-relation)
                        (grammar ?grammar))
               (syn-cat (number plural)
                        (syn-class np)
                        (leftmost-unit ?determiner-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)
                        ;(definiteness ?def)
                        )
               (subunits (?X-unit ?present-unit ?relation-unit )))
              <-
              (?X-R-unit
               (HASH meaning ((relate ?target ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-unit ?present-unit) (meets ?present-unit ?leftmost-relation-unit))))
              (?X-unit
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
                        )         
               --
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?relation-unit
               (args ((target ?target) (original-source ?original-input) (source ?segmented-scene) (relation ?relation)(unique ?unique)))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit))
               ;(multiple-relate t)
               --
               ;(multiple-relate t)
               (args ((target ?target) (original-source ?original-input) (source ?segmented-scene) (relation ?relation)(unique ?unique)))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn X-plural-R-cxn
             ((?X-R-unit
               (args ((target ?first-np-target) (source ?segmented-scene) (original-source ?original-input)))
               (sem-cat (sem-class immediate-relation)
                        (grammar ?grammar))
               (syn-cat (number plural)
                        (syn-class np)
                        (leftmost-unit ?determiner-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)
                        ;(definiteness ?def)
                        )
               (subunits (?X-unit ?relation-unit )))
              <-
              (?X-R-unit
               (HASH meaning ((relate ?first-np-source ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-unit ?leftmost-relation-unit))))
              (?X-unit
               (args ((target ?first-np-target) (source ?first-np-source) ;(original-source ?target)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
                        )         
               --
               (args ((target ?first-np-target) (source ?first-np-source) ;(original-source ?target)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?determiner-unit)
                        ;(definiteness ?def)
))
              (?relation-unit
               (args ((target ?unique)
                      (source ?segmented-scene)
                      (relation ?relation) ;(original-source ?original-input) (unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit))
               (multiple-relate t)
               --
               (multiple-relate t)
               (args ((target ?unique)
                      (source ?segmented-scene)
                      (relation ?relation) ;(original-source ?original-input) (unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-of-second-np-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)