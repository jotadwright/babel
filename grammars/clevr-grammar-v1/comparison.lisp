;;;; comparison.lisp

(in-package :clevr-grammar-v1)

;; ----------------------------------------------------- ;;
;; This file contains  grammatical constructions for the ;;
;; comparison question family                            ;;
;; ----------------------------------------------------- ;;

(def-fcg-cxn the-same-T-cxn ;; feature end +, split -, anaphoric -
             ((?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (syn-cat (syn-class comparative-conjunction))
               (property-type ?type)
               (subunits (?the-same ?type-unit))
               (leftmost-unit ?the-same)
               (rightmost-unit ?type-unit)
               (end +)
               (split -)
               (anaphoric -))
              <-
              (?the-same
               (HASH meaning ((equal? ?target ?src-1 ?src-2 ?attribute)
                              (query ?src-1 ?object-1 ?attribute)
                              (query ?src-2 ?object-2 ?attribute)))
               --
               (HASH form ((string ?the-same "the same")
                           (meets ?the-same ?type-unit))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn the-T-of-the-same-as-cxn ;; feature end -, split +, anaphoric -
             ((?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (syn-cat (syn-class comparative-conjunction))
               (property-type ?type)
               (subunits (?the ?type-unit ?of ?the-same-as))
               (leftmost-unit ?the)
               (rightmost-unit ?the-same-as)
               (end -)
               (split +)
               (anaphoric -))
              (?the-same-as
               (HASH form ((precedes ?of ?the-same-as))))
              <-
              (?the
               (HASH meaning ((equal? ?target ?src-1 ?src-2 ?attribute)))
               --
               (HASH form ((string ?the "the")
                           (meets ?the ?type-unit))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute)))
              (?of
               (HASH meaning ((query ?src-1 ?object-1 ?attribute)))
               --
               (HASH form ((string ?of "of")
                           (meets ?type-unit ?of))))
              (?the-same-as
               (HASH meaning ((query ?src-2 ?object-2 ?attribute)))
               --
               (HASH form ((string ?the-same-as "the same as")
                           ;(precedes ?of ?the-same-as)
                           ))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn the-same-T-as-compare-cxn ;; feature end - split - anaphoric +
             ((?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (syn-cat (syn-class comparative-conjunction))
               (property-type ?type)
               (subunits (?the-same ?type-unit ?as))
               (leftmost-unit ?the-same)
               (rightmost-unit ?as)
               (end -)
               (split -)
               (anaphoric +))
              <-
              (?the-same
               (HASH meaning ((equal? ?target ?src-1 ?src-2 ?attribute)))
               --
               (HASH form ((string ?the-same "the same"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?the-same ?type-unit)
                           (meets ?type-unit ?as))))
              (?as
               (HASH meaning ((query ?src-1 ?object-1 ?attribute)
                              (query ?src-2 ?object-2 ?attribute)))
               --
               (HASH form ((string ?as "as")))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

(def-fcg-cxn T-the-same-as-cxn ;; feature end - split + anaphoric +
             ((?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (syn-cat (syn-class comparative-conjunction))
               (property-type ?type)
               (subunits (?type-unit ?the-same-as))
               (leftmost-unit ?type-unit)
               (rightmost-unit ?the-same-as)
               (end -)
               (split +)
               (anaphoric +))
              <-
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute)))
              (?the-same-as
               (HASH meaning ((equal? ?target ?src-1 ?src-2 ?attribute)
                              (query ?src-1 ?object-1 ?attribute)
                              (query ?src-2 ?object-2 ?attribute)))
               --
               (HASH form ((string ?the-same-as "the same as")
                           (meets ?type-unit ?the-same-as)))))
             :cxn-set cxn
             :cxn-inventory *clevr*)

;; compare-do <- "do" + np1 + "and" + np2 + "have" + compare-type
(def-fcg-cxn compare-do-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?and ?rel-unit-2 ?have ?compare-type-unit)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "do")
                           (meets ?compare-unit ?leftmost-np-unit-1))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?and
               --
               (HASH form ((string ?and "and")
                           (meets ?rightmost-np-unit-1 ?and)
                           (meets ?and ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))
              (?have
               --
               (HASH form ((string ?have "have")
                           (meets ?rightmost-np-unit-2 ?have)
                           (meets ?have ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end +)(split -)(anaphoric -)
               --
               (end +)(split -)(anaphoric -)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; compare-is-split <- "is" + compare-type + np1 + np2
(def-fcg-cxn compare-is-split-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?compare-type-unit ?rel-unit-1 ?rel-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is")
                           (meets ?compare-unit ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split +)(anaphoric -)
               --
               (end -)(split +)(anaphoric -)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-np-unit-1 ?rightmost-compare-unit)
                           (meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; compare-is <- "is" + np1 + compare-unit + np2
(def-fcg-cxn compare-is-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?compare-type-unit ?rel-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is")
                           (meets ?compare-unit ?leftmost-np-unit-1))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split -)(anaphoric +)
               --
               (end -)(split -)(anaphoric +)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-np-unit-1 ?leftmost-compare-unit)
                           (meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              

;; compare-does <- "does" + np1 + "have" + compare-unit + np2
(def-fcg-cxn compare-does-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?have ?compare-type-unit ?rel-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "does")
                           (meets ?compare-unit ?leftmost-np-unit-1))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?have
               --
               (HASH form ((string ?have "have")
                           (meets ?rightmost-np-unit-1 ?have)
                           (meets ?have ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split -)(anaphoric +)
               --
               (end -)(split -)(anaphoric +)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
                           
;; compare-anaphoric-does <- np1 + ";" + "does it have" + compare-unit + np2
(def-fcg-cxn compare-anaphoric-does-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?semicolon ?compare-type-unit ?rel-unit-2)))
              <-
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit-1 ?semicolon)
                           (meets ?semicolon ?compare-unit))))
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "does it have")
                           (meets ?compare-unit ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split -)(anaphoric +)
               --
               (end -)(split -)(anaphoric +)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              

;; compare-anaphoric-is <- np1 + ";" + "is it" + compare-unit + np2
(def-fcg-cxn compare-anaphoric-is-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?semicolon ?compare-type-unit ?rel-unit-2)))
              <-
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit-1 ?semicolon)
                           (meets ?semicolon ?compare-unit))))
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is it")
                           (meets ?compare-unit ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split -)(anaphoric +)
               --
               (end -)(split -)(anaphoric +)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
;; compare-anaphoric-is-split <- np1 + ";" + "is its" + compare-unit + np2
(def-fcg-cxn compare-anaphoric-is-split-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?semicolon ?compare-type-unit ?rel-unit-2)))
              <-
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit-1 ?semicolon)
                           (meets ?semicolon ?compare-unit))))
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is its")
                           (meets ?compare-unit ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type ?type)
               (end -)(split +)(anaphoric +)
               --
               (end -)(split +)(anaphoric +)
               (property-type ?type)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))

;; EXCEPTIONS
(def-fcg-cxn compare-are-material-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?and ?rel-unit-2 ?made-of ?compare-type-unit)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "are")
                           (meets ?compare-unit ?leftmost-np-unit-1))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?and
               --
               (HASH form ((string ?and "and")
                           (meets ?rightmost-np-unit-1 ?and)
                           (meets ?and ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-np-unit-2 ?made-of)
                           (meets ?made-of ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type material)
               (end +)(split -)(anaphoric -)
               --
               (end +)(split -)(anaphoric -)
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))
              
(def-fcg-cxn compare-is-material-cxn
             ((?compare-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?rel-unit-1 ?made-of ?compare-type-unit ?rel-unit-2)))
              <-
              (?compare-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?compare-unit "is")
                           (meets ?compare-unit ?leftmost-np-unit-1))))
              (?rel-unit-1
               (args ((sources ?context)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-np-unit-1 ?made-of)
                           (meets ?made-of ?leftmost-compare-unit))))
              (?compare-type-unit
               (args ((sources ?object-1 ?object-2)
                      (target ?target)))
               (sem-cat (sem-function compare-property))
               (property-type material)
               (end -)(split -)(anaphoric +)
               --
               (end -)(split -)(anaphoric +)
               (property-type material)
               (syn-cat (syn-class comparative-conjunction))
               (leftmost-unit ?leftmost-compare-unit)
               (rightmost-unit ?rightmost-compare-unit)
               (HASH form ((meets ?rightmost-compare-unit ?leftmost-np-unit-2))))
              (?rel-unit-2
               (args ((sources ?context)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2)))
             :cxn-set cxn
             :cxn-inventory *clevr*
             :attributes (:terminal t))