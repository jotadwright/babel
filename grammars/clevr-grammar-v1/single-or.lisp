;;;; single-or.lisp

(in-package :clevr-grammar-v1)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the union function          ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn X-or-Y-cxn
             ((?union-unit
               (args ((sources ?source)
                      (target ?target)))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction union))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-2)
               (subunits (?plural-nominal-unit-1 ?plural-nominal-unit-2)))
              <-
              (?plural-nominal-unit-1
               (args ((sources ?source)
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
              (?union-unit
               (HASH meaning ((union! ?target ?set-1 ?set-2)))
               --
               (HASH form ((string ?union-unit "or")
                           (meets ?rightmost-nom-unit-1 ?union-unit)
                           (meets ?union-unit ?leftmost-nom-unit-2))))
              (?plural-nominal-unit-2
               (args ((sources ?source)
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
             :cxn-inventory *clevr*)
              
(def-fcg-cxn either-X-or-Y-cxn
             ((?union-unit
               (args ((sources ?source)
                      (target ?target)))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction union))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?either)
               (rightmost-unit ?rightmost-nom-unit-2)
               (subunits (?either ?plural-nominal-unit-1 ?plural-nominal-unit-2)))
              <-
              (?either
               --
               (HASH form ((string ?either "either")
                           (meets ?either ?leftmost-nom-unit-1))))
              (?plural-nominal-unit-1
               (args ((sources ?source)
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
              (?union-unit
               (HASH meaning ((union! ?target ?set-1 ?set-2)))
               --
               (HASH form ((string ?union-unit "or")
                           (meets ?rightmost-nom-unit-1 ?union-unit)
                           (meets ?union-unit ?leftmost-nom-unit-2))))
              (?plural-nominal-unit-2
               (args ((sources ?source)
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
             :cxn-inventory *clevr*)

(def-fcg-cxn union-count-cxn
             ((?count-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?counting-unit ?union-unit)))
              (?counting-unit
               (footprints (single-or)))
              <-
              (?count-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?counting-unit
               (args ((sources ?union-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (qtype single-or)
               (footprints (NOT other))
               --
               (footprints (NOT other))
               (qtype single-or)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-counting-unit)
               (rightmost-unit ?rightmost-counting-unit)
               (HASH form ((meets ?rightmost-counting-unit ?leftmost-union-unit))))
              (?union-unit
               (args ((sources ?context)
                      (target ?union-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction union))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction union))
               (leftmost-unit ?leftmost-union-unit)
               (rightmost-unit ?rightmost-union-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))