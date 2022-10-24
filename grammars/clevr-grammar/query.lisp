;;;; query.lisp

(in-package :clevr-grammar)

;; ----------------------------------------------------- ;;
;; This file contains grammatical constructions for      ;;
;; certain patterns used for the query function          ;;
;; ----------------------------------------------------- ;;

;; In the *-hop question families, questions types 3 -> 6
;; there are a lot of similar sentences regarding the types of objects
;; "what T is", "what is the T of", "what T is it", "what is its T"
;; where you replace T with a type like size, color, shape or material.
;; We handle this by constructions as well and use this cxn
;; to fill in the last argument of the query predicate,
;; e.g (query ?target ?source ?category)
;; that is now being used in these questions.

;; Unfortunately, there are still a few exceptions to this
;; (e.g. "how big is" or "what is it made of")
;; that do not follow this pattern. These still need specific cxns.

;; For now, the ?category is placed directly in the query predicate.
;; This could also be done using a bind-statement. For this, we would
;; need to introduce e.g. attribute-category class.

(def-fcg-cxn what-T-is-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric -)
                        (position front)
                        (material-suffix +))
               (leftmost-unit ?what)
               (rightmost-unit ?is)
               (subunits (?what ?type-unit ?is))
               (footprints (query)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?what
               --
               (HASH form ((string ?what "what"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?what ?type-unit)
                           (meets ?type-unit ?is))))
              (?is
               --
               (HASH form ((string ?is "is")))))
             :cxn-inventory *CLEVR*
             :cxn-set cxn)

(def-fcg-cxn what-is-the-T-of-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what-is ?the ?type-unit ?of))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric -)
                        (position front)
                        (material-suffix -))
               (leftmost-unit ?what-is)
               (rightmost-unit ?of)
               (footprints (query)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?what-is
               --
               (HASH form ((string ?what-is "what is"))))
              (?the
               --
               (HASH form ((string ?the "the")
                           (meets ?what-is ?the))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?the ?type-unit)
                           (meets ?type-unit ?of))))
              (?of
               --
               (HASH form ((string ?of "of")))))
             :cxn-inventory *CLEVR*
             :cxn-set cxn)

(def-fcg-cxn what-T-is-it-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what ?type-unit ?is-it))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?what)
               (rightmost-unit ?is-it)
               (footprints (query)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?what
               --
               (HASH form ((string ?what "what"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?what ?type-unit)
                           (meets ?type-unit ?is-it))))
              (?is-it
               --
               (HASH form ((string ?is-it "is it")))))
             :cxn-inventory *CLEVR*
             :cxn-set cxn)

(def-fcg-cxn what-is-its-T-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?what-is-its ?type-unit))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric +))
               (leftmost-unit ?what-is-its)
               (rightmost-unit ?type-unit)
               (footprints (query)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?what-is-its
               --
               (HASH form ((string ?what-is-its "what is its"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?what-is-its ?type-unit)))))
             :cxn-inventory *CLEVR*
             :cxn-set cxn)

;; these cxns can only apply on the size, color and shape types
;; to avoid the material type, the type itself is placed in the
;; footprints
(def-fcg-cxn has-what-T-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?has-what ?type-unit))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric -)
                        (position rear))
               (leftmost-unit ?has-what)
               (rightmost-unit ?type-unit)
               (footprints (query ?type)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?has-what
               --
               (HASH form ((string ?has-what "has what"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?has-what ?type-unit)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)

(def-fcg-cxn is-what-T-cxn
             ((?query-type-unit
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?is-what ?type-unit))
               (sem-cat (sem-function query-property))
               (property-type ?type)
               (syn-cat (anaphoric -)
                        (position rear))
               (leftmost-unit ?is-what)
               (rightmost-unit ?type-unit)
               (footprints (query ?type)))
              (root
               (footprints (mutually-exclusive)))
              <-
              (root
               (footprints (NOT mutually-exclusive))
               --
               (footprints (NOT mutually-exclusive)))
              (?query-type-unit
               (HASH meaning ((query ?target ?source ?attribute)))
               --
               )
              (?is-what
               --
               (HASH form ((string ?is-what "is what"))))
              (?type-unit
               (args ((target ?attribute)))
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (property-type ?type)
               --
               (property-type ?type)
               (syn-cat (lex-class noun))
               (sem-cat (sem-class attribute))
               (HASH form ((meets ?is-what ?type-unit)))))
             :cxn-inventory *clevr*
             :cxn-set cxn)