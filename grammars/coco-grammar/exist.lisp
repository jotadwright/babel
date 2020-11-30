;;;; exist.lisp

(in-package :coco-grammar)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the exist function          ;;
;; ----------------------------------------------------- ;;

;; are there any [other] Xs [that are/that have/of]

(def-fcg-cxn are-other-cxn
             ((?other-exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?exist-unit ?other))
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (footprints (other))
               (qtype same-relate)
               (material-suffix ?mat))
              (?exist-unit
               (footprints (other)))
              <-
              (?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (footprints (NOT other hop))
               (material-suffix ?mat)
               --
               (material-suffix ?mat)
               (footprints (NOT other hop))
               (qtype same-relate)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit))
              (?other
               --
               (HASH form ((string ?other "other")
                           (meets ?leftmost-exist-unit ?other)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn are-there-any-X-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?are-there-any ?plural-nominal-unit))
               (leftmost-unit ?are-there-any)
               (rightmost-unit ?rightmost-nom-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (qtype ?qtype)
               (suffix-type none)
               (material-suffix +))
              (?plural-nominal-unit
               (footprints (exist)))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?are-there-any
               --
               (HASH form ((string ?are-there-any "are there any")
                           (precedes ?are-there-any ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn are-there-any-X-that-are-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?are-there-any ?plural-nominal-unit ?that-are))
               (leftmost-unit ?are-there-any)
               (rightmost-unit ?that-are)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (qtype ?type)
               (material-suffix +))
              (?plural-nominal-unit
               (footprints (exist)))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?are-there-any
               --
               (HASH form ((string ?are-there-any "are there any")
                           (precedes ?are-there-any ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-are
               --
               (HASH form ((string ?that-are "that are")
                           (meets ?rightmost-nom-unit ?that-are)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn are-there-any-X-that-have-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?are-there-any ?plural-nominal-unit ?that-have))
               (leftmost-unit ?are-there-any)
               (rightmost-unit ?that-have)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (exist)))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?are-there-any
               --
               (HASH form ((string ?are-there-any "are there any")
                           (precedes ?are-there-any ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-have
               --
               (HASH form ((string ?that-have "that have")
                           (meets ?rightmost-nom-unit ?that-have)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn are-there-any-X-of-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?are-there-any ?plural-nominal-unit ?of))
               (leftmost-unit ?are-there-any)
               (rightmost-unit ?of)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (exist)))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?are-there-any
               --
               (HASH form ((string ?are-there-any "are there any")
                           (precedes ?are-there-any ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?of
               --
               (HASH form ((string ?of "of")
                           (meets ?rightmost-nom-unit ?of)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

;; is there a X [that has/that is/of]
(def-fcg-cxn is-there-a-X-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?rightmost-nom-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype ?qtype)
               (suffix-type none)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there a")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-a-X-that-has-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-has))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-has)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there a")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-has
               --
               (HASH form ((string ?that-has "that has")
                           (meets ?rightmost-nom-unit ?that-has)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-a-X-that-is-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-is))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-is)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype ?qtype)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there a")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-a-X-of-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?of))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?of)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there a")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?of
               --
               (HASH form ((string ?of "of")
                           (meets ?rightmost-nom-unit ?of)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

;; is there another X [that has/that is/of]
(def-fcg-cxn is-there-another-X-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?rightmost-nom-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there another")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit)))
             :cxn-set cxn
             :cxn-inventory *COCO*)
     
(def-fcg-cxn is-there-another-X-that-has-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-has))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-has)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there another")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-has
               --
               (HASH form ((string ?that-has "that has")
                           (meets ?rightmost-nom-unit ?that-has)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-another-X-that-is-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-is))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-is)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there another")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-another-X-of-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?of))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?of)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there another")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?of
               --
               (HASH form ((string ?of "of")
                           (meets ?rightmost-nom-unit ?of)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

;; is there any other X [that has/that is/of]
(def-fcg-cxn is-there-any-other-X-that-has-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-has))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-has)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there any other")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-has
               --
               (HASH form ((string ?that-has "that has")
                           (meets ?rightmost-nom-unit ?that-has)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-any-other-X-that-is-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?that-is))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?that-is)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there any other")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-nom-unit ?that-is)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn is-there-any-other-X-of-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-there-unit ?singular-nominal-unit ?of))
               (leftmost-unit ?is-there-unit)
               (rightmost-unit ?of)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?exist-set)))
               --
               )
              (?is-there-unit
               --
               (HASH form ((string ?is-there-unit "is there any other")
                           (meets ?is-there-unit ?leftmost-nom-unit))))
              (?singular-nominal-unit
               (args ((sources ?source)
                      (target ?exist-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (sem-cat (sem-class object))
               (superunits nil)
               --
               (superunits nil)
               (sem-cat (sem-class object))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number singular))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?of
               --
               (HASH form ((string ?of "of")
                           (meets ?rightmost-nom-unit ?of)))))
             :cxn-set cxn
             :cxn-inventory *COCO*)

;; is there anything else [that has/that is/of]
(def-fcg-cxn is-there-anything-else-that-has-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (leftmost-unit ?existing-unit)
               (rightmost-unit ?existing-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?source)))
               --
               (HASH form ((string ?existing-unit "is there anything else that has")))))
             :cxn-set cxn
             :cxn-inventory *COCO*)
              
(def-fcg-cxn is-there-anything-else-that-is-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (leftmost-unit ?existing-unit)
               (rightmost-unit ?existing-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate)
               (material-suffix +))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?source)))
               --
               (HASH form ((string ?existing-unit "is there anything else that is")))))
             :cxn-set cxn
             :cxn-inventory *COCO*)
             
(def-fcg-cxn is-there-anything-else-of-cxn
             ((?exist-unit
               (args ((sources ?source)
                      (target ?target)))
               (leftmost-unit ?existing-unit)
               (rightmost-unit ?existing-unit)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number singular))
               (qtype same-relate))
              <-
              (?exist-unit
               (HASH meaning ((exist ?target ?source)))
               --
               (HASH form ((string ?existing-unit "is there anything else of")))))
             :cxn-set cxn
             :cxn-inventory *COCO*)