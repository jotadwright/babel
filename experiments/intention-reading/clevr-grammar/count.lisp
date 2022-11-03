;;;; count.lisp

(in-package :intention-reading)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the count function          ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn count-other-cxn
             ((?other-counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?counting-unit ?other))
               (leftmost-unit ?leftmost-count-unit)
               (rightmost-unit ?rightmost-count-unit)
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (footprints (other))
               (qtype same-relate)
               (material-suffix ?mat))
              (?counting-unit
               (superunits (?other-counting-unit))
               (footprints (other)))
              <-
              (?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (qtype same-relate)
               (footprints (NOT other hop single-and single-or))
               (material-suffix ?mat)
               --
               (material-suffix ?mat)
               (footprints (NOT other hop single-and single-or))
               (qtype same-relate)
               (sem-cat (sem-function count-referent))
               (syn-cat (number ?number))
               (leftmost-unit ?leftmost-count-unit)
               (rightmost-unit ?rightmost-count-unit))
              (?other
               --
               (HASH form ((string ?other "other")
                           (meets ?leftmost-count-unit ?other)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

;; how many [other] Xs are/are there of/have
(def-fcg-cxn how-many-X-are-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?how-many ?plural-nominal-unit ?are))
               (leftmost-unit ?how-many)
               (rightmost-unit ?are)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype ?qtype)
               (material-suffix +))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?how-many
               --
               (HASH form ((string ?how-many "how many")
                           (precedes ?how-many ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?are
               --
               (HASH form ((string ?are "are")
                           (meets ?rightmost-nom-unit ?are)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

(def-fcg-cxn how-many-X-are-there-of-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?how-many ?plural-nominal-unit ?are-there-of))
               (leftmost-unit ?how-many)
               (rightmost-unit ?are-there-of)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?how-many
               --
               (HASH form ((string ?how-many "how many")
                           (precedes ?how-many ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?are-there-of
               --
               (HASH form ((string ?are-there-of "are there of")
                           (meets ?rightmost-nom-unit ?are-there-of)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

(def-fcg-cxn how-many-X-have-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?how-many ?plural-nominal-unit ?have))
               (leftmost-unit ?how-many)
               (rightmost-unit ?have)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?how-many
               --
               (HASH form ((string ?how-many "how many")
                           (precedes ?how-many ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?have
               --
               (HASH form ((string ?have "have")
                           (meets ?rightmost-nom-unit ?have)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

;; what number of [other] Xs are/are there of/have
(def-fcg-cxn what-number-of-X-are-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what-number-of ?plural-nominal-unit ?are))
               (leftmost-unit ?what-number-of)
               (rightmost-unit ?are)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype ?qtype)
               (material-suffix +))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?what-number-of
               --
               (HASH form ((string ?what-number-of "what number of")
                           (precedes ?what-number-of ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?are
               --
               (HASH form ((string ?are "are")
                           (meets ?rightmost-nom-unit ?are)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

(def-fcg-cxn what-number-of-X-are-there-of-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what-number-of ?plural-nominal-unit ?are-there-of))
               (leftmost-unit ?what-number-of)
               (rightmost-unit ?are-there-of)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?what-number-of
               --
               (HASH form ((string ?what-number-of "what number of")
                           (precedes ?what-number-of ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?are-there-of
               --
               (HASH form ((string ?are-there-of "are there of")
                           (meets ?rightmost-nom-unit ?are-there-of)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)


(def-fcg-cxn what-number-of-X-have-cxn
             ((?counting-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what-number-of ?plural-nominal-unit ?have))
               (leftmost-unit ?what-number-of)
               (rightmost-unit ?have)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (qtype same-relate)
               (material-suffix -))
              (?plural-nominal-unit
               (footprints (count)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?counting-unit
               (HASH meaning ((count! ?target ?count-set)))
               --
               )
              (?what-number-of
               --
               (HASH form ((string ?what-number-of "what number of")
                           (precedes ?what-number-of ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?source)
                      (target ?count-set)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate))
               --
               (footprints (NOT plural-relate))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?have
               --
               (HASH form ((string ?have "have")
                           (meets ?rightmost-nom-unit ?have)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)
