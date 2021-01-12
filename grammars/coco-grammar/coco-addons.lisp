(in-package :coco-grammar)

;; On which side <of the <photo<graph>/picture>> is the X?
;; Where in the <photo<graph>/picture> is the X?
;; Where is the X?
;; Which side is X on?
;; On which side is the X?

;; (get-context ?context)
;; (filter ?set ?context X)
;; (unique ?obj ?set)
;; (query ?answer ?obj side)

(def-fcg-cxn photo-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular)))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the photo"))))
              :cxn-set lex))

(def-fcg-cxn image-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular)))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the image"))))
              :cxn-set lex))

(def-fcg-cxn photograph-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular)))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the photograph"))))
              :cxn-set lex))

(def-fcg-cxn picture-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular)))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the picture"))))
              :cxn-set lex))
              
(def-fcg-cxn where-in-the-view-is-x-cxn
             ((?relate-unit
               (subunits (?where-in-unit ?view-unit ?is-unit
                          ?determined-noun-phrase-unit))
               (leftmost-unit ?where-in-unit)
               (rightmost-unit ?rightmost-np-unit)
               (args ((sources ?source)
                      (target ?out))))
              <-
              (?where-in-unit
               (HASH meaning ((query ?out ?object ?side)
                              (bind attribute-category ?side side)))
               --
               (HASH form ((string ?where-in-unit "where in")
                           (meets ?where-in-unit ?view-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?view-unit ?is-unit)
                           (meets ?is-unit ?leftmost-np-unit))))
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
             :cxn-set cxn)

(def-fcg-cxn on-which-side-of-the-view-is-x-cxn
             ((?relate-unit
               (subunits (?on-which-side-unit ?view-unit ?is-unit
                          ?determined-noun-phrase-unit))
               (leftmost-unit ?on-which-side-unit)
               (rightmost-unit ?rightmost-np-unit)
               (superunits nil)
               (args ((sources ?source)
                      (target ?target))))
              (?determined-noun-phrase-unit
               (superunits (?relate-unit)))
              (?on-which-side-unit
               (superunits (?relate-unit)))
              (?view-unit
               (superunits (?relate-unit)))
              (?is-unit
               (superunits (?relate-unit)))
              <-
              (?on-which-side-unit
               (HASH meaning ((query ?target ?object ?side)
                              (bind attribute-category ?side side)))
               --
               (HASH form ((string ?on-which-side-unit "on which side of")
                           (meets ?on-which-side-unit ?view-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?view-unit ?is-unit)
                           (meets ?is-unit ?leftmost-np-unit))))
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
             :cxn-set cxn)

(def-fcg-cxn where-is-x-cxn
             ((?relate-unit
               (subunits (?where-is-unit
                          ?determined-noun-phrase-unit))
               (leftmost-unit ?where-is-unit)
               (rightmost-unit ?rightmost-np-unit)
               (args ((sources ?source)
                      (target ?out))))
              <-
              (?where-is-unit
               (HASH meaning ((query ?out ?object ?side)
                              (bind attribute-category ?side side)
                              (get-context ?source)))
               --
               (HASH form ((string ?where-is-unit "where is")
                           (meets ?where-is-unit ?leftmost-np-unit))))
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
             :cxn-set cxn)

(def-fcg-cxn which-side-is-X-on-cxn
             ((?query-unit
               (subunits (?which-side-is-unit ?determined-noun-phrase-unit ?on-unit)))
              <-
              (?which-side-is-unit
               (HASH meaning ((get-context ?source)
                              (query ?target ?object ?side)
                              (bind attribute-category ?side side)))
               --
               (HASH form ((string ?which-side-is-unit "which side is")
                           (meets ?which-side-is-unit ?leftmost-np-unit))))
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
               (rightmost-unit ?rightmost-np-unit))
              (?on-unit
               --
               (HASH form ((string ?on-unit "on")
                           (meets ?rightmost-np-unit ?on-unit))))))

(def-fcg-cxn on-which-side-is-X-cxn
             ((?query-unit
               (subunits (?on-which-side-is-unit ?determined-noun-phrase-unit)))
              <-
              (?on-which-side-is-unit
               (HASH meaning ((get-context ?source)
                              (query ?target ?object ?side)
                              (bind attribute-category ?side side)))
               --
               (HASH form ((string ?on-which-side-is-unit "on which side is")
                           (meets ?on-which-side-is-unit ?leftmost-np-unit))))
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
               (rightmost-unit ?rightmost-np-unit))))
              
              

;; Is the X on the R side of the photo/picture?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj ?set-1)
;; (verify ?target ?obj R)

(def-fcg-cxn is-x-on-the-side-of-cxn
             ((?relate-unit
               (subunits (?is-unit ?determined-noun-phrase-unit
                          ?relational-unit ?view-unit))
               (leftmost-unit ?is-unit)
               (rightmost-unit ?view-unit)
               (superunits nil)
               (args ((sources ?source)
                      (target ?out))))
              (?is-unit
               (superunits (?relate-unit)))
              (?determined-noun-phrase-unit
               (superunits (?relate-unit)))
              (?relational-unit
               (superunits (?relate-unit)))
              (?view-unit
               (superunits (?relate-unit)))
              <-
              (?is-unit
               (HASH meaning ((verify ?out ?object ?side)))
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?is-unit ?leftmost-np-unit))))
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
               (rightmost-unit ?rightmost-np-unit)
               (HASH form ((meets ?rightmost-np-unit ?relational-unit)
                           (meets ?relational-unit ?view-unit))))
              (?relational-unit
               (args ((target ?side)))
               (sem-cat (sem-class spatial-relation))
               (has-side +)
               --
               (syn-cat (lex-class preposition))
               (has-side +))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))))
             :cxn-set cxn)

;; is X R Y?
;; is the woman to the left of a person?

;; (get-context ?context)
;; (filter ?set-1 ?context Y)
;; (unique ?obj-1 ?set-1)
;;
;; (filter ?set-2 ?context)
;; (unique ?obj-2 ?set-2)
;;
;; (verify ?target ?obj-2 ?obj-1 R)

(def-fcg-cxn is-X-R-Y-cxn
             ((?verify-unit
               (subunits (?is-unit ?det-np-unit-1 ?spatial-relation-unit ?det-np-unit-2)))
              <-
              (?is-unit
               (HASH meaning ((get-context ?source)
                              (verify ?target ?object-2 ?object-1 ?spatial-relation)))
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?is-unit ?leftmost-np-unit-1))))
              (?det-np-unit-1
               (args ((sources ?source)
                      (target ?object-2)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-1)
               (rightmost-unit ?rightmost-np-unit-1))
              (?spatial-relation-unit
               (args ((target ?spatial-relation)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (HASH form ((meets ?rightmost-np-unit-1 ?spatial-relation-unit)
                           (meets ?spatial-relation-unit ?leftmost-np-unit-2))))
              (?det-np-unit-2
               (args ((sources ?source)
                      (target ?object-1)))
               (sem-cat (sem-function referring-expression))
               (syn-cat (definite +))
               (footprints (NOT relate base-relate))
               --
               (footprints (NOT relate base-relate))
               (syn-cat (phrase-type np)
                        (number singular)
                        (definite +))
               (leftmost-unit ?leftmost-np-unit-2)
               (rightmost-unit ?rightmost-np-unit-2))))            
              

;; Is the X A?
;; Is the cat black?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj ?set-1)
;; (verify ?target ?obj A)

(def-fcg-cxn is-X-A-cxn
             ((?exist-unit
               (subunits (?is-unit ?determined-noun-phrase-unit ?adjective-unit)))
              <-
              (?is-unit
               (HASH meaning ((get-context ?source)
                              (verify ?target ?object ?category)))
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?is-unit ?leftmost-np-unit))))
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
               (rightmost-unit ?rightmost-np-unit)
               (HASH form ((meets ?rightmost-np-unit ?adjective-unit))))
              (?adjective-unit
               (args ((target ?category)))
               (sem-cat (sem-class ?class))
               (syn-cat (lex-class adjective))
               (footprints (NOT nominal))
               --
               (footprints (NOT nominal))
               (syn-cat (lex-class adjective))))
             :cxn-set cxn)

;; Is the X on the left or the right?
;; Is the X on the left side or on the right side?
;; Is the X on the left side or on the right side of the picture?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj-1 ?set-1)
;; (choose ?target ?obj-1 ?option-A ?option-B)


;(def-fcg-cxn ...)

;; Is the X A or B?
;; Is the cat black or white?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj-1 ?set-1)
;; (choose ?target ?obj-1 ?option-A ?option-B)

(def-fcg-cxn is-x-a-or-b-cxn
             ((?either-unit
               (subunits (?is-unit ?determined-noun-phrase-unit
                          ?adjective-1-unit ?or-unit
                          ?adjective-2-unit))
               )
              <-
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?is-unit ?leftmost-np-unit)))
               )
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
               (rightmost-unit ?rightmost-np-unit)
               (HASH form ((meets ?rightmost-np-unit ?adjective-1-unit))))
              (?adjective-1-unit
               (args ((target ?category-A)))
               (sem-cat (sem-class ?class))
               (syn-cat (lex-class adjective))
               (footprints (NOT nominal))
               --
               (footprints (NOT nominal))
               (syn-cat (lex-class adjective)))
              (?or-unit
               (HASH meaning ((get-context ?source)
                              (choose ?target ?object ?category-A ?category-B)))
               --
               (HASH form ((string ?or-unit "or")
                           (meets ?adjective-1-unit ?or-unit)
                           (meets ?or-unit ?adjective-2-unit)))
               )
              (?adjective-2-unit
               (args ((target ?category-B)))
               (sem-cat (sem-class ?class))
               (syn-cat (lex-class adjective))
               (footprints (NOT nominal))
               --
               (footprints (NOT nominal))
               (syn-cat (lex-class adjective))))
             :cxn-set cxn)



