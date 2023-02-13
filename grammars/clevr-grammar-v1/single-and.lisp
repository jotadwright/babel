;;;; single-and.lisp

(in-package :clevr-grammar-v1)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the intersect function      ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn RX-and-RY-cxn
             ((?prepositional-phrase-1
               (args ((sources ?source)
                      (target ?related-set-1)))
               (syn-cat (phrase-type pp)
                        (number singular)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit-1 ?determined-noun-phrase-unit-1))
               (leftmost-unit ?spatial-relation-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?prepositional-phrase-2
               (args ((sources ?source)
                      (target ?related-set-2)))
               (syn-cat (phrase-type pp)
                        (number singular)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit-2 ?determined-noun-phrase-unit-2))
               (leftmost-unit ?spatial-relation-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))
              (?intersect-unit
               (args ((sources ?source)
                      (target ?target)))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?spatial-relation-unit-1)
               (rightmost-unit ?rightmost-np-unit-2)
               (subunits (?prepositional-phrase-1 ?prepositional-phrase-2)))
              <-
              (?prepositional-phrase-1
               (HASH meaning ((relate ?related-set-1 ?object-1 ?spatial-relation-1)))
               --
               )
              (?spatial-relation-unit-1
               (args ((target ?spatial-relation-1)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit-1 ?leftmost-np-unit-1))))
              (?determined-noun-phrase-unit-1
               (args ((sources ?source)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?intersect-unit
               (HASH meaning ((intersect ?target ?related-set-1 ?related-set-2)))
               --
               (HASH form ((string ?intersect-unit "and")
                           (meets ?rightmost-np-unit-1 ?intersect-unit)
                           (meets ?intersect-unit ?spatial-relation-unit-2))))
              (?prepositional-phrase-2
               (HASH meaning ((relate ?related-set-2 ?object-2 ?spatial-relation-2)))
               --
               )
              (?spatial-relation-unit-2
               (args ((target ?spatial-relation-2)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit-2 ?leftmost-np-unit-2))))
              (?determined-noun-phrase-unit-2
               (args ((sources ?source)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn both-RX-and-RY-cxn
             ((?prepositional-phrase-1
               (args ((sources ?source)
                      (target ?related-set-1)))
               (syn-cat (phrase-type pp)
                        (number singular)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit-1 ?determined-noun-phrase-unit-1))
               (leftmost-unit ?spatial-relation-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?prepositional-phrase-2
               (args ((sources ?source)
                      (target ?related-set-2)))
               (syn-cat (phrase-type pp)
                        (number singular)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit-2 ?determined-noun-phrase-unit-2))
               (leftmost-unit ?spatial-relation-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))
              (?intersect-unit
               (args ((sources ?source)
                      (target ?target)))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?both)
               (rightmost-unit ?rightmost-np-unit-2)
               (subunits (?both ?prepositional-phrase-1 ?prepositional-phrase-2)))
              <-
              (?both
               --
               (HASH form ((string ?both "both")
                           (meets ?both ?spatial-relation-unit-1))))
              (?prepositional-phrase-1
               (HASH meaning ((relate ?related-set-1 ?object-1 ?spatial-relation-1)))
               --
               )
              (?spatial-relation-unit-1
               (args ((target ?spatial-relation-1)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit-1 ?leftmost-np-unit-1))))
              (?determined-noun-phrase-unit-1
               (args ((sources ?source)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?intersect-unit
               (HASH meaning ((intersect ?target ?related-set-1 ?related-set-2)))
               --
               (HASH form ((string ?intersect-unit "and")
                           (meets ?rightmost-np-unit-1 ?intersect-unit)
                           (meets ?intersect-unit ?spatial-relation-unit-2))))
              (?prepositional-phrase-2
               (HASH meaning ((relate ?related-set-2 ?object-2 ?spatial-relation-2)))
               --
               )
              (?spatial-relation-unit-2
               (args ((target ?spatial-relation-2)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit-2 ?leftmost-np-unit-2))))
              (?determined-noun-phrase-unit-2
               (args ((sources ?source)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*)
               
(def-fcg-cxn intersect-count-cxn
             ((?count-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?counting-unit ?intersect-unit)))
              (?counting-unit
               (footprints (single-and)))
              <-
              (?count-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?counting-unit
               (args ((sources ?intersected-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (qtype single-and)
               (footprints (NOT other))
               --
               (footprints (NOT other))
               (qtype single-and)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-counting-unit)
               (rightmost-unit ?rightmost-counting-unit)
               (HASH form ((meets ?rightmost-counting-unit ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

(def-fcg-cxn intersect-query-property-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-noun-phrase-unit ?that-is ?intersect-unit)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?query-type-unit
               (args ((sources ?object)
                      (target ?target)))
               (property-type ?type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front))
               --
               (property-type ?type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)
               (HASH form ((meets ?rightmost-qt-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

(def-fcg-cxn intersect-query-property-reverse-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?determined-noun-phrase-unit ?that-is ?intersect-unit ?query-type-unit)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?determined-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit)
               (HASH form ((meets ?rightmost-intersect-unit ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?object)
                      (target ?target)))
               (property-type ?type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position rear))
               (footprints (NOT material))
               --
               (property-type ?type)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

(def-fcg-cxn intersect-query-property-anaphoric-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?intersect-unit ?semicolon ?query-type-unit)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-intersect-unit ?semicolon)
                           (meets ?semicolon ?leftmost-qt-unit))))
              (?query-type-unit
               (args ((sources ?object)
                      (target ?target)))
               (property-type ?type)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric +))
               --
               (property-type ?type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; EXCEPTIONS
;; how big is X
(def-fcg-cxn intersect-how-big-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?determined-noun-phrase-unit ?that-is ?intersect-unit)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute size)))
               --
               (HASH form ((string ?query-unit "how big is")
                           (meets ?query-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; there is a X; how big is it
(def-fcg-cxn intersect-how-big-anaphoric-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?intersect-unit ?semicolon)))
              <-
              (?declared-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-intersect-unit ?semicolon)
                           (meets ?semicolon ?query-unit))))
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute size)))
               --
               (HASH form ((string ?query-unit "how big is it")))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; what is the X made of
(def-fcg-cxn intersect-made-of-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?what-is ?determined-noun-phrase-unit ?that-is ?intersect-unit)))
              <-
              (?what-is
               --
               (HASH form ((string ?what-is "what is")
                           (meets ?what-is ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               (HASH form ((string ?query-unit "made of")
                           (meets ?rightmost-intersect-unit ?query-unit)))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; the X is made of what material
(def-fcg-cxn intersect-made-of-reverse-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?determined-noun-phrase-unit ?that-is ?intersect-unit ?type-unit)))
              <-
              (?determined-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)))
               --
               (HASH form ((string ?query-unit "is made of what")
                           (meets ?rightmost-intersect-unit ?query-unit)
                           (meets ?query-unit ?type-unit))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type material)
               --
               (property-type material)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; there is a X; what [material] is it made of
(def-fcg-cxn intersect-made-of-anaphoric-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?intersect-unit ?semicolon)))
              <-
              (?declared-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-intersect-unit ?semicolon)
                           (meets ?semicolon ?query-unit))))
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               (HASH form ((string ?query-unit "what is it made of")))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

(def-fcg-cxn intersect-made-of-anaphoric-extended-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?that-is ?intersect-unit ?semicolon ?what ?type-unit)))
              <-
              (?declared-noun-phrase-unit
               (args ((sources ?intersected-set)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?that-is
               --
               (HASH form ((string ?that-is "that is")
                           (meets ?rightmost-np-unit ?that-is)
                           (meets ?that-is ?leftmost-intersect-unit))))
              (?intersect-unit
               (args ((sources ?context)
                      (target ?intersected-set)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               --
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction intersect))
               (leftmost-unit ?leftmost-intersect-unit)
               (rightmost-unit ?rightmost-intersect-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-intersect-unit ?semicolon)
                           (meets ?semicolon ?what))))
              (?what
               --
               (HASH form ((string ?what "what")
                           (meets ?what ?type-unit))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type material)
               --
               (property-type material)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute)))
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)))
               --
               (HASH form ((string ?query-unit "is it made of")
                           (meets ?type-unit ?query-unit)))))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
