;;;; multi-hop.lisp

(in-package :clevr-grammar)

;; ----------------------------------------------------- ;;
;; This file contains  grammatical constructions for the ;;
;; '*-hop.json' question family                          ;;
;; ----------------------------------------------------- ;;

;; COUNT PATTERNS
;; how many Xs are there (zero hop only)
;; what number of Xs are there (zero hop only)
(def-fcg-cxn count-zero-hop-cxn
             ((?count-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?counting-unit ?there)))
              (?counting-unit
               (footprints (hop)))
              <-
              (?count-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?counting-unit
               (args ((sources ?context)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (qtype hop)
               (footprints (NOT other))
               --
               (footprints (NOT other))
               (qtype hop)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-counting-unit)
               (rightmost-unit ?rightmost-counting-unit))
              (?there
               --
               (HASH form ((string ?there "there")
                           (meets ?rightmost-counting-unit ?there)))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; how many Xs are R Y (R Z)
;; what number of Xs are R Y (R Z)
(def-fcg-cxn hop-count-cxn
             ((?count-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?counting-unit ?spatial-relation-unit ?relational-unit)))
              (?counting-unit
               (footprints (hop)))
              <-
              (?count-unit
               (HASH meaning ((get-context ?context)
                              (relate ?count-set ?object ?spatial-relation)))
               --
               )
              (?counting-unit
               (args ((sources ?count-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (qtype hop)
               (footprints (NOT other))
               --
               (footprints (NOT other))
               (qtype hop)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-counting-unit)
               (rightmost-unit ?rightmost-counting-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-counting-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?relational-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; there is a X (R Y (R Z)), how many Xs are R it?
;; there is a X (R Y (R Z)), what number of Xs are R it?
(def-fcg-cxn hop-count-anaphoric-cxn
             ((?count-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?relational-unit ?semicolon ?counting-unit ?spatial-relation-unit ?it)))
              (?counting-unit
               (footprints (hop)))
              <-
              (?count-unit
               (HASH meaning ((get-context ?context)
                              (relate ?count-set ?object ?spatial-relation)))
               --
               )
              (?relational-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?leftmost-counting-unit))))
              (?counting-unit
               (args ((sources ?count-set)
                      (target ?target)))
               (sem-cat (sem-function count-referent))
               (qtype hop)
               (footprints (NOT other))
               --
               (footprints (NOT other))
               (qtype hop)
               (sem-cat (sem-function count-referent))
               (syn-cat (number plural))
               (leftmost-unit ?leftmost-counting-unit)
               (rightmost-unit ?rightmost-counting-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-counting-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?it))))
              (?it
               --
               (HASH form ((string ?it "it")))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; EXIST PATTERNS 
;; are any Xs visible (zero hop only)
(def-fcg-cxn exist-zero-hop-visible-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-any ?plural-nominal-unit ?visible)))
              <-
              (?exist-unit
               (HASH meaning ((get-context ?context)
                              (exist ?target ?exist-set)))
               --
               )
              (?are-any
               --
               (HASH form ((string ?are-any "are any")
                           (meets ?are-any ?leftmost-nom-unit))))
              (?plural-nominal-unit
               (args ((sources ?context)
                      (target ?exist-set)))
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
              (?visible
               --
               (HASH form ((string ?visible "visible")
                           (meets ?rightmost-nom-unit ?visible)))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

(def-fcg-cxn exist-zero-hop-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?existing-unit)))
              (?existing-unit
               (footprints (exist hop)))
              <-
              (?exist-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?existing-unit
               (args ((sources ?context)
                      (target ?target)))
               (suffix-type none)
               (qtype hop)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (footprints (NOT exist other))
               --
               (footprints (NOT exist other))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype hop)
               (suffix-type none)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))
              

;; are there any Xs [that are] (R Y (R Z))
;; is there a X [that it] (R Y (R Z))
(def-fcg-cxn hop-exist-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?existing-unit ?spatial-relation-unit ?relational-unit)))
              (?existing-unit
               (footprints (exist hop)))
              <-
              (?exist-unit
               (HASH meaning ((get-context ?context)
                              (relate ?exist-set ?object ?spatial-relation)))
               --
               )
              (?existing-unit
               (args ((sources ?exist-set)
                      (target ?target)))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype hop)
               (footprints (NOT exist other))
               --
               (footprints (NOT exist other))
               (qtype hop)
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-exist-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit))))
              (?relational-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; there is a X (R Y (R Z)) ; are there any Xs [that are] R it? (multi hop only)
;; there is a X (R Y (R Z)) ; is there a X [that is] R it? (multi hop only)
(def-fcg-cxn hop-exist-anaphoric-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?relational-unit ?semicolon ?existing-unit ?spatial-relation-unit ?it)))
              (?existing-unit
               (footprints (exist hop)))
              <-
              (?exist-unit
               (HASH meaning ((get-context ?context)
                              (relate ?exist-set ?object ?spatial-relation)))
               --
               )
              (?relational-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?leftmost-exist-unit))))
              (?existing-unit
               (args ((sources ?exist-set)
                      (target ?target)))
               (sem-cat (sem-function exist-referent))
               (syn-cat (number ?number))
               (qtype hop)
               (footprints (NOT exist other))
               --
               (footprints (NOT exist other))
               (qtype hop)
               (syn-cat (number ?number))
               (sem-cat (sem-function exist-referent))
               (leftmost-unit ?leftmost-exist-unit)
               (rightmost-unit ?rightmost-exist-unit))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-exist-unit ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?it))))
              (?it
               --
               (HASH form ((string ?it "it")))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))


;; QUERY PROPERTY PATTERN
(def-fcg-cxn hop-query-property-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-noun-phrase-unit)))
              (?determined-noun-phrase-unit
               (footprints (query)))
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
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

(def-fcg-cxn hop-query-property-reverse-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-noun-phrase-unit)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?determined-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
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
               (rightmost-unit ?rightmost-qt-unit)
               (HASH form ((meets ?rightmost-np-unit ?leftmost-qt-unit)))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

(def-fcg-cxn hop-query-property-anaphoric-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?query-type-unit ?semicolon ?declared-noun-phrase-unit)))
              (?declared-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               (superunits nil) ;; only apply to the topmost np
               --
               (superunits nil) ;; only apply to the topmost np
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
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
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; EXCEPTIONS:
;; what [material] is the ... made of
(def-fcg-cxn hop-what-is-X-made-of-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?value)))
               (subunits (?what-is ?determined-noun-phrase-unit ?made-of)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               )
              (?what-is
               --
               (HASH form ((string ?what-is "what is")
                           (meets ?what-is ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-np-unit ?made-of)))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

(def-fcg-cxn hop-what-material-is-X-made-of-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?query-type-unit ?determined-noun-phrase-unit ?made-of)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)))
               --
               )
              (?query-type-unit
               (args ((sources ?object)
                      (target ?target)))
               (property-type material)
               (sem-cat (sem-function query-property))
               (syn-cat (anaphoric -)
                        (position front)
                        (material-suffix +))
               --
               (property-type material)
               (syn-cat (anaphoric -))
               (leftmost-unit ?leftmost-qt-unit)
               (rightmost-unit ?rightmost-qt-unit)
               (HASH form ((meets ?rightmost-qt-unit ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?made-of
               --
               (HASH form ((string ?made-of "made of")
                           (meets ?rightmost-np-unit ?made-of)))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; there is a ... ; what is it made of
(def-fcg-cxn hop-what-is-it-made-of-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?semicolon ?what-is-it-made-of)))
              (?declared-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute material)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?what-is-it-made-of))))
              (?what-is-it-made-of
               --
               (HASH form ((string ?what-is-it-made-of "what is it made of")))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; the ... is made of what material
(def-fcg-cxn hop-X-is-made-of-what-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?determined-noun-phrase-unit ?is-made-of-what ?type-unit)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)))
               --
               )
              (?determined-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?is-made-of-what
               --
               (HASH form ((string ?is-made-of-what "is made of what")
                           (meets ?rightmost-np-unit ?is-made-of-what)
                           (meets ?is-made-of-what ?type-unit))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type material)
               --
               (property-type material)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; how big is the ...
(def-fcg-cxn hop-how-big-is-X-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?how-big-is ?determined-noun-phrase-unit)))
              (?determined-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute size)))
               --
               )
              (?how-big-is
               --
               (HASH form ((string ?how-big-is "how big is")
                           (meets ?how-big-is ?leftmost-np-unit))))
              (?determined-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit)))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))

;; there is a ... ; how big is it
(def-fcg-cxn hop-how-big-is-it-cxn
             ((?query-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?declared-noun-phrase-unit ?semicolon ?how-big-is-it)))
              (?declared-noun-phrase-unit
               (footprints (query)))
              <-
              (?query-unit
               (HASH meaning ((get-context ?context)
                              (query ?target ?object ?attribute)
                              (bind attribute-category ?attribute size)))
               --
               )
              (?declared-noun-phrase-unit
               (args ((sources ?context)
                      (target ?object)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite -))
               (footprints (NOT query))
               --
               (footprints (NOT query))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite -))
               (leftmost-unit ?leftmost-np-unit)
               (rightmost-unit ?rightmost-np-unit))
              (?semicolon
               --
               (HASH form ((string ?semicolon ";")
                           (meets ?rightmost-np-unit ?semicolon)
                           (meets ?semicolon ?how-big-is-it))))
              (?how-big-is-it
               --
               (HASH form ((string ?how-big-is-it "how big is it")))))
             :cxn-inventory *clevr*
             :cxn-set cxn
             :attributes (:terminal t))