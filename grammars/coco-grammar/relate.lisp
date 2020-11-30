;;;; relate.lisp

(in-package :coco-grammar)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions to       ;;
;; recursively construct the relate function             ;;
;; ----------------------------------------------------- ;;

;; the *-hop question family shows a recursive pattern in question types 3 -> 6:
;; zero-hop:  what shape is the A                         / there is a A; what shape is it?
;; one-hop:   what shape is the A R the B                 / there is a A R the B; what shape is it?
;; two-hop:   what shape is the A R the B R the C         / there is a A R the B R the C; what shape is it?
;; three-hop: what shape is the A R the B R the C R the D / there is a A R the B R the C R the D; what shape is it?

;; base-relate-cxn takes a determined/declared singular nominal,
;; a spatial relation and a determined singular nominal and creates
;; a 'related noun phrase'. It also adds the 'relate' predicate.

;; IMPROVE!!
;; For the moment, many syn-cat and sem-cat features are repeated in both locks
;; to differentiate determined noun phrases with relate noun phrase.
;; The addition of some extra features for the relate-noun-phrase would be nice
;; What semantic and syntactic properties discriminate this from a regular noun phrase?
(def-fcg-cxn base-relate-cxn
             ((?relate-noun-phrase-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-ref-unit)
               (rightmost-unit ?rightmost-np-unit)
               (subunits (?referent-noun-phrase-unit ?spatial-relation-unit ?determined-noun-phrase-unit))
               (superunits nil)
               (relate +)
               (footprints (base-relate)))
              (?referent-noun-phrase-unit
               (superunits (?relate-noun-phrase-unit))
               (footprints (relate)))
              (?spatial-relation-unit
               (footprints (relate)))
              (?determined-noun-phrase-unit
               (superunits (?relate-noun-phrase-unit))
               (footprints (relate)))
              <-
              (?relate-noun-phrase-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?referent-noun-phrase-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-ref-unit)
               (rightmost-unit ?rightmost-ref-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-ref-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *COCO*
             :cxn-set cxn)

;; relate-cxn takes a 'related noun phrase', a spatial relation
;; and a determined singular nominal and creates a super related np.
;; This cxn can apply recursively. It also adds a 'relate' predicate.
(def-fcg-cxn relate-cxn
             ((?super-relate-noun-phrase-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-relate-unit)
               (rightmost-unit ?rightmost-np-unit)
               (subunits (?relate-noun-phrase-unit ?spatial-relation-unit ?determined-noun-phrase-unit))
               (superunits nil)
               (relate +)
               (footprints (base-relate)))
              (?relate-noun-phrase-unit
               (superunits (?super-relate-noun-phrase-unit))
               (footprints (relate)))
              (?spatial-relation-unit
               (footprints (relate)))
              (?determined-noun-phrase-unit
               (superunits (?super-relate-noun-phrase-unit))
               (footprints (relate)))
              <-
              (?super-relate-noun-phrase-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?relate-noun-phrase-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (footprints (NOT relate))
               (relate +)
               --
               (relate +)
               (footprints (NOT relate))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?leftmost-relate-unit)
               (rightmost-unit ?rightmost-relate-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-relate-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *COCO*
             :cxn-set cxn)
              
;; we need 2 (base-)relate cxns, since at any level the string 'that is' can be between
;; a nominal and a spatial relation, e.g. what shape is the A R the B that is R the C
(def-fcg-cxn base-relate-that-is-cxn
             ((?relate-noun-phrase-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-ref-unit)
               (rightmost-unit ?rightmost-np-unit)
               (subunits (?referent-noun-phrase-unit ?that-is-unit ?spatial-relation-unit ?determined-noun-phrase-unit))
               (superunits nil)
               (relate +)
               (footprints (base-relate)))
              (?referent-noun-phrase-unit
               (superunits (?relate-noun-phrase-unit))
               (footprints (relate)))
              (?spatial-relation-unit
               (footprints (relate)))
              (?determined-noun-phrase-unit
               (superunits (?relate-noun-phrase-unit))
               (footprints (relate)))
              <-
              (?relate-noun-phrase-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?referent-noun-phrase-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-ref-unit)
               (rightmost-unit ?rightmost-ref-unit))
              (?that-is-unit
               --
               (HASH form ((string ?that-is-unit "that is")
                           (meets ?rightmost-ref-unit ?that-is-unit)
                           (meets ?that-is-unit ?spatial-relation-unit))))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *COCO*
             :cxn-set cxn)

(def-fcg-cxn relate-that-is-cxn
             ((?super-relate-noun-phrase-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (leftmost-unit ?leftmost-rel-unit)
               (rightmost-unit ?rightmost-np-unit)
               (subunits (?relate-noun-phrase-unit ?that-is-unit ?spatial-relation-unit ?determined-noun-phrase-unit))
               (superunits nil)
               (relate +)
               (footprints (base-relate)))
              (?relate-noun-phrase-unit
               (superunits (?super-relate-noun-phrase-unit))
               (footprints (relate)))
              (?spatial-relation-unit
               (footprints (relate)))
              (?determined-noun-phrase-unit
               (superunits (?super-relate-noun-phrase-unit))
               (footprints (relate)))
              <-
              (?super-relate-noun-phrase-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?relate-noun-phrase-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (footprints (NOT relate))
               (relate +)
               --
               (relate +)
               (footprints (NOT relate))
               (syn-cat (phrase-type np)
                        (number ?number)
                        (definite ?def))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?leftmost-rel-unit)
               (rightmost-unit ?rightmost-rel-unit))
              (?that-is-unit
               --
               (HASH form ((string ?that-is-unit "that is")
                           (meets ?rightmost-rel-unit ?that-is-unit)
                           (meets ?that-is-unit ?spatial-relation-unit))))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *COCO*
             :cxn-set cxn)


;; the single-or question family requires a relate-cxn with a plural nominal
;; as the referent and determined noun phrase as the modifier
;; Example; Xs R the Y; Xs that are R the Y.
(def-fcg-cxn plural-relate-cxn
             ((?relate-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (lex-class np)
                        (syn-function nominal)
                        (number plural))
               (subunits (?plural-nominal-unit ?prepositional-phrase-unit))
               (superunits nil)
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-np-unit)
               (footprints (plural-relate)))
              (?plural-nominal-unit
               (superunits (?relate-unit))
               (footprints (plural-relate)))
              (?prepositional-phrase-unit
               (args ((sources ?input-set)
                      (target ?region-set)))
               (syn-cat (phrase-type pp)
                        (number plural)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit ?determined-noun-phrase-unit))
               (leftmost-unit ?spatial-relation-unit)
               (rightmost-unit ?rightmost-np-unit))
              <-
              (?relate-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?plural-nominal-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate exist count))
               --
               (footprints (NOT plural-relate exist count))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-nom-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *COCO*)

(def-fcg-cxn plural-relate-that-are-cxn
             ((?relate-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (lex-class np)
                        (syn-function nominal)
                        (number plural))
               (subunits (?plural-nominal-unit ?that-are ?prepositional-phrase-unit))
               (superunits nil)
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-np-unit)
               (footprints (plural-relate)))
              (?plural-nominal-unit
               (superunits (?relate-unit))
               (footprints (plural-relate)))
              (?prepositional-phrase-unit
               (args ((sources ?input-set)
                      (target ?region-set)))
               (syn-cat (phrase-type pp)
                        (number plural)
                        (definite +))
               (sem-cat (sem-function spatial-relation))
               (subunits (?spatial-relation-unit ?determined-noun-phrase-unit))
               (leftmost-unit ?spatial-relation-unit)
               (rightmost-unit ?rightmost-np-unit))
              <-
              (?relate-unit
               (HASH meaning ((relate ?related-set ?object ?spatial-relation)))
               --
               )
              (?plural-nominal-unit
               (args ((sources ?related-set)
                      (target ?target)))
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (superunits nil)
               (footprints (NOT plural-relate exist count))
               --
               (footprints (NOT plural-relate exist count))
               (superunits nil)
               (syn-cat (syn-function nominal)
                        (lex-class np)
                        (number plural))
               (leftmost-unit ?leftmost-nom-unit)
               (rightmost-unit ?rightmost-nom-unit))
              (?that-are
               --
               (HASH form ((string ?that-are "that are")
                           (meets ?rightmost-nom-unit ?that-are)
                           (meets ?that-are ?spatial-relation-unit))))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               --
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?source)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               --
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-set cxn
             :cxn-inventory *COCO*)