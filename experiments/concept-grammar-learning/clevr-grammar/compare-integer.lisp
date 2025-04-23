;;;; compare-integer.lisp

(in-package :cgl)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the equal_integer,          ;;
;; less_than and greater_than functions                  ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn an-equal-number-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (comparison equal))
               (syn-cat (syn-class comparative-adjective)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (equal-integer ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "an equal number of")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn the-same-number-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (comparison equal))
               (syn-cat (syn-class comparative-adjective)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (equal-integer ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "the same number of")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn fewer-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (comparison less-than))
               (syn-cat (syn-class comparative-adjective)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (less-than ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "fewer")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn more-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (comparison greater-than))
               (syn-cat (syn-class comparative-adjective)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (greater-than ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "more")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn the-same-as-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class uncountable-comparator)
                        (comparison equal))
               (syn-cat (syn-class comparative-conjunction)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (equal-integer ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "the same as")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn less-than-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class uncountable-comparator)
                        (comparison less-than))
               (syn-cat (syn-class comparative-conjunction)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (less-than ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "less than")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn greater-than-cxn
             ((?comparator-unit
               (args ((sources ?src-1 ?src-2)
                      (target ?target)))
               (sem-cat (sem-class uncountable-comparator)
                        (comparison greater-than))
               (syn-cat (syn-class comparative-conjunction)))
              <-
              (?comparator-unit
               (HASH meaning ((count! ?count-1 ?src-1)
                              (count! ?count-2 ?src-2)
                              (greater-than ?target ?count-1 ?count-2)))
               --
               (HASH form ((string ?comparator-unit "greater than")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn compare-equal-countable-cxn
             ;; are-there + C + Xs + and + Ys
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?there ?comparator-unit ?plural-nominal-unit-1 ?and ?plural-nominal-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "are")
                           (meets ?compare-unit ?there))))
              ;; split up "are" and "there" to solve sentence preprocessing issue...
              (?there
               --
               (HASH form ((string ?there "there")
                           (meets ?there ?comparator-unit))))
              (?comparator-unit
               (args ((sources ?set-1 ?set-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (comparison equal))
               --
               (syn-cat (syn-class comparative-adjective))
               (sem-cat (comparison equal))
               (HASH form ((meets ?comparator-unit ?leftmost-nom-unit-1))))
              (?plural-nominal-unit-1
               (args ((sources ?context)
                      (target ?set-1)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-1))
              (?and
               --
               (HASH form ((string ?and "and")
                           (meets ?rightmost-nom-unit-1 ?and)
                           (meets ?and ?leftmost-nom-unit-2))))
              (?plural-nominal-unit-2
               (args ((sources ?context)
                      (target ?set-2)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-2)
               (rightmost-unit ?rightmost-nom-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

(def-fcg-cxn compare-unequal-countable-cxn
             ;; are-there + C + Xs + than + Ys
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?there ?comparator-unit ?plural-nominal-unit-1 ?than ?plural-nominal-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "are")
                           (meets ?compare-unit ?there))))
              ;; split up "are" and "there" to solve sentence preprocessing issue...
              (?there
               --
               (HASH form ((string ?there "there")
                           (meets ?there ?comparator-unit))))
              (?comparator-unit
               (args ((sources ?set-1 ?set-2)
                      (target ?target)))
               (sem-cat (sem-class countable-comparator)
                        (NOT (comparison equal)))
               --
               (syn-cat (syn-class comparative-adjective))
               (sem-cat (NOT (comparison equal)))
               (HASH form ((meets ?comparator-unit ?leftmost-nom-unit-1))))
              (?plural-nominal-unit-1
               (args ((sources ?context)
                      (target ?set-1)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-1))
              (?than
               --
               (HASH form ((string ?than "than")
                           (meets ?rightmost-nom-unit-1 ?than)
                           (meets ?than ?leftmost-nom-unit-2))))
              (?plural-nominal-unit-2
               (args ((sources ?context)
                      (target ?set-2)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-2)
               (rightmost-unit ?rightmost-nom-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

(def-fcg-cxn compare-uncountable-cxn
             ;; is the number of + Xs + C + the number of + Ys
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?plural-nominal-unit-1 ?comparator-unit ?number-of-unit ?plural-nominal-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is the number of")
                           (meets ?compare-unit ?leftmost-nom-unit-1))))
              (?plural-nominal-unit-1
               (args ((sources ?context)
                      (target ?set-1)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-1))
              (?comparator-unit
               (args ((sources ?set-1 ?set-2)
                      (target ?target)))
               (sem-cat (sem-class uncountable-comparator))
               --
               (syn-cat (syn-class comparative-conjunction))
               (HASH form ((meets ?rightmost-nom-unit-1 ?comparator-unit))))
              (?number-of-unit
               --
               (HASH form ((string ?number-of-unit "the number of")
                           (meets ?comparator-unit ?number-of-unit)
                           (meets ?number-of-unit ?leftmost-nom-unit-2))))
              (?plural-nominal-unit-2
               (args ((sources ?context)
                      (target ?set-2)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               --
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit-2)
               (rightmost-unit ?rightmost-nom-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))