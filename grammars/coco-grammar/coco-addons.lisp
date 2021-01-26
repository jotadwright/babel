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
                        (number singular))
               (leftmost-unit ?photo-unit)
               (rightmost-unit ?photo-unit))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the photo")))))
              :cxn-set lex)

(def-fcg-cxn image-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular))
               (leftmost-unit ?photo-unit)
               (rightmost-unit ?photo-unit))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the image")))))
              :cxn-set lex)

(def-fcg-cxn photograph-lex-cxn
             ((?photo-unit
               (args ((sources nil)
                      (target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular))
               (leftmost-unit ?photo-unit)
               (rightmost-unit ?photo-unit))
              <-
              (?photo-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?photo-unit "the photograph")))))
              :cxn-set lex)

(def-fcg-cxn picture-view-lex-cxn
             ((?the-picture-unit
               (args ((sources nil)
                      (target ?context)))
               (subunits (?the-unit ?picture-unit))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)
                        (definite +)
                        (number singular))
               (leftmost-unit ?the-unit)
               (rightmost-unit ?picture-unit))
              <-
              (?the-unit
               --
               (HASH form ((string ?the-unit "the")
                           (meets ?the-unit ?picture-unit))))
              (?picture-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?picture-unit "picture")))))
              :cxn-set lex)




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
                           (meets ?where-in-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?rightmost-view-unit ?is-unit)
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
                           (meets ?on-which-side-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?rightmost-view-unit ?is-unit)
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
               (HASH meaning ((verify_side ?out ?object ?side)))
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
                           (meets ?relational-unit ?leftmost-view-unit))))
              (?relational-unit
               (args ((target ?side)))
               (sem-cat (sem-class spatial-relation))
               (suffix side-of)
               --
               (syn-cat (lex-class preposition))
               (suffix side-of))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
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
                              (verify_relation ?target ?object-2 ?object-1 ?spatial-relation)))
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


;; Is the X on the left or on the right?
;; Is the X on the left side or on the right side?
;; Is the X on the left side or on the right side of the picture?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj-1 ?set-1)
;; (choose ?target ?obj-1 ?option-A ?option-B)

;; Where in the view is the X, on the left or on the right?
;; Where in the view is the X, on the left side or on the right side?

;; (get-context ?context)
;; (filter ?set-1 ?context X)
;; (unique ?obj-1 ?set-1)
;; (choose ?target ?obj-1 ?option-A ?option-B)

(def-fcg-cxn choose-spatial-subordinate-clause-cxn
             ((?choose-spatial-unit
               (subunits (?spatial-unit-1 ?or-unit ?spatial-unit-2))
               (args ((sources ?object)
                      (target ?target)))
               (leftmost-unit ?spatial-unit-1)
               (rightmost-unit ?spatial-unit-2)
               (sem-cat (sem-class choose-spatial-relation))
               (syn-cat (phrase-type subordinate-clause))
               (left-suffix ?left-suffix)
               (right-suffix ?right-suffix))
              (?spatial-unit-1
               (superunits (?choose-spatial-unit)))
              (?or-unit
               (superunits (?choose-spatial-unit)))
              (?spatial-unit-2
               (superunits (?choose-spatial-unit)))
              <-
              (?spatial-unit-1
               (args ((target ?spatial-relation-1)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               (suffix ?left-suffix)
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (suffix ?left-suffix))
              (?or-unit
               (HASH meaning ((choose ?target ?object ?spatial-relation-1 ?spatial-relation-2)))
               --
               (HASH form ((string ?or-unit "or")
                           (meets ?spatial-unit-1 ?or-unit)
                           (meets ?or-unit ?spatial-unit-2))))
              (?spatial-unit-2
               (args ((target ?spatial-relation-2)))
               (sem-cat (sem-class spatial-relation))
               (footprints (NOT relate))
               (suffix ?right-suffix)
               --
               (footprints (NOT relate))
               (syn-cat (lex-class preposition))
               (suffix ?right-suffix)))
             :cxn-set cxn)

(def-fcg-cxn is-X-choose-spatial-cxn
             ((?clause
               (subunits (?is-unit ?determined-noun-phrase-unit ?choose-spatial-unit))
               (args ((sources ?source)
                      (target ?target))))
              (?determined-noun-phrase-unit
               (superunits (?clause)))
              (?choose-spatial-unit
               (superunits (?clause)))
              <-
              (?is-unit
               (HASH meaning ((get-context ?source)))
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
               (rightmost-unit ?rightmost-np-unit))
              (?choose-spatial-unit
               (args ((sources ?object)
                      (target ?target)))
               (sem-cat (sem-class choose-spatial-relation))
               (syn-cat (phrase-type subordinate-clause))
               --
               (syn-cat (phrase-type subordinate-clause))
               (leftmost-unit ?leftmost-choose-unit)
               (rightmost-unit ?rightmost-choose-unit)
               (HASH form ((meets ?rightmost-np-unit ?leftmost-choose-unit)))))
             :cxn-set cxn)

(def-fcg-cxn is-X-choose-spatial+view-cxn
             ((?clause
               (subunits (?is-unit ?determined-noun-phrase-unit
                          ?choose-spatial-unit ?view-unit))
               (args ((sources ?source)
                      (target ?target))))
              (?determined-noun-phrase-unit
               (superunits (?clause)))
              (?choose-spatial-unit
               (superunits (?clause)))
              (?view-unit
               (superunits (?clause)))
              <-
              (?is-unit
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
               (rightmost-unit ?rightmost-np-unit))
              (?choose-spatial-unit
               (args ((sources ?object)
                      (target ?target)))
               (sem-cat (sem-class choose-spatial-relation))
               (syn-cat (phrase-type subordinate-clause))
               --
               (right-suffix side-of)
               (syn-cat (phrase-type subordinate-clause))
               (leftmost-unit ?leftmost-choose-unit)
               (rightmost-unit ?rightmost-choose-unit)
               (HASH form ((meets ?rightmost-np-unit ?leftmost-choose-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               (HASH form ((meets ?rightmost-choose-unit ?leftmost-view-unit)))))
             :cxn-set cxn)
               
(def-fcg-cxn where-in-view-is-X-choose-spatial-cxn
             ((?clause
               (args ((sources ?source)
                      (target ?target)))
               (subunits (?where-in-unit ?view-unit ?is-unit
                          ?determined-noun-phrase-unit ?comma
                          ?choose-spatial-unit)))
              (?view-unit
               (superunits (?clause)))
              (?determined-noun-phrase-unit
               (superunits (?clause)))
              (?choose-spatial-unit
               (superunits (?clause)))
              <-
              (?where-in-unit
               --
               (HASH form ((string ?where-in-unit "where in")
                           (meets ?where-in-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?source)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is")
                           (meets ?rightmost-view-unit ?is-unit)
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
               (rightmost-unit ?rightmost-np-unit))
              (?comma
               --
               (HASH form ((string ?comma ",")
                           (meets ?rightmost-np-unit ?comma)
                           (meets ?comma ?leftmost-choose-unit))))
              (?choose-spatial-unit
               (args ((sources ?object)
                      (target ?target)))
               (sem-cat (sem-class choose-spatial-relation))
               (syn-cat (phrase-type subordinate-clause))
               --
               (syn-cat (phrase-type subordinate-clause))
               (leftmost-unit ?leftmost-choose-unit)
               (rightmost-unit ?rightmost-choose-unit)))
             :cxn-set cxn)

;; Are there Xs or Ys?
;; Are there Xs or Ys in the view?
;; Are there any Xs or Ys?
;; Are there either any Xs or Ys?

;; ((get-context ?context)
;;  (filter ?X-set ?context X)
;;  (exist ?X-exist ?X-set)
;;  (filter ?Y-set ?context Y)
;;  (exist ?Y-exist ?Y-set)
;;  (or ?exist ?X-exist ?Y-exist))

(def-fcg-cxn Xs-or-Ys-cxn
             ((?X-or-Y-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (subunits (?plural-nominal-unit-1 ?plural-nominal-unit-2))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction disjunction))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-2))
              (?plural-nominal-unit-1
               (superunits (?X-or-Y-unit)))
              (?plural-nominal-unit-2
               (superunits (?X-or-Y-unit)))
              <-
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
              (?X-or-Y-unit
               (HASH meaning ((or ?target ?bool-1 ?bool-2)))
               --
               (HASH form ((string ?X-or-Y-unit "or")
                           (meets ?rightmost-nom-unit-1 ?X-or-Y-unit)
                           (meets ?X-or-Y-unit ?leftmost-nom-unit-2))))
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
             :cxn-set cxn)

(def-fcg-cxn are-there-disjunction-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-unit ?there-unit ?disjunction-unit)))
              <-
              (?are-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?are-unit "are")
                           (meets ?are-unit ?there-unit))))
              (?there-unit
               (HASH meaning ((exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?there-unit "there")
                           (meets ?there-unit ?leftmost-disjunction-unit))))
              (?disjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction disjunction))
               (leftmost-unit ?leftmost-disjunction-unit)
               (rightmost-unit ?rightmost-disjunction-unit)))
             :cxn-set cxn)

(def-fcg-cxn are-there-any-disjunction-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-there-unit ?disjunction-unit)))
              <-
              (?are-there-unit
               (HASH meaning ((get-context ?context)
                              (exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?are-there-unit "are there any")
                           (meets ?are-there-unit ?leftmost-disjunction-unit))))
              (?disjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction disjunction))
               (leftmost-unit ?leftmost-disjunction-unit)
               (rightmost-unit ?rightmost-disjunction-unit)))
             :cxn-set cxn)

(def-fcg-cxn are-there-either-any-disjunction-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-there-unit ?disjunction-unit)))
              <-
              (?are-there-unit
               (HASH meaning ((get-context ?context)
                              (exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?are-there-unit "are there either any")
                           (meets ?are-there-unit ?leftmost-disjunction-unit))))
              (?disjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction disjunction))
               (leftmost-unit ?leftmost-disjunction-unit)
               (rightmost-unit ?rightmost-disjunction-unit)))
             :cxn-set cxn)


(def-fcg-cxn are-there-disjunction-in-view-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-unit ?there-unit ?disjunction-unit ?in-unit ?view-unit)))
              <-
              (?are-unit
               (HASH meaning ((exist ?bool-1 ?set-1)))
               --
               (HASH form ((string ?are-unit "are")
                           (meets ?are-unit ?there-unit))))
              (?there-unit
               (HASH meaning ((exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?there-unit "there")
                           (meets ?there-unit ?leftmost-disjunction-unit))))
              (?disjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction disjunction))
               (leftmost-unit ?leftmost-disjunction-unit)
               (rightmost-unit ?rightmost-disjunction-unit))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in")
                           (meets ?rightmost-disjunction-unit ?in-unit)
                           (meets ?in-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))))
             :cxn-set cxn)
              
              



;; Are there Xs and Ys?
;; Are there both Xs and Ys?
;; Are there both Xs and Ys in the view?

;; ((get-context ?context)
;;  (filter ?X-set ?context X)
;;  (exist ?X-exist ?X-set)
;;  (filter ?Y-set ?context Y)
;;  (exist ?Y-exist ?Y-set)
;;  (and ?exist ?X-exist ?Y-exist))

(def-fcg-cxn Xs-and-Ys-cxn
             ((?X-and-Y-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (subunits (?plural-nominal-unit-1 ?plural-nominal-unit-2))
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction conjunction))
               (sem-cat (sem-function referring-expression))
               (leftmost-unit ?leftmost-nom-unit-1)
               (rightmost-unit ?rightmost-nom-unit-2))
              (?plural-nominal-unit-1
               (superunits (?X-and-Y-unit)))
              (?plural-nominal-unit-2
               (superunits (?X-and-Y-unit)))
              <-
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
              (?X-and-Y-unit
               (HASH meaning ((and ?target ?bool-1 ?bool-2)))
               --
               (HASH form ((string ?X-and-Y-unit "and")
                           (meets ?rightmost-nom-unit-1 ?X-and-Y-unit)
                           (meets ?X-and-Y-unit ?leftmost-nom-unit-2))))
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
             :cxn-set cxn)

(def-fcg-cxn are-there-conjunction-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-unit ?there-unit ?conjunction-unit)))
              <-
              (?are-unit
               (HASH meaning ((get-context ?context)))
               --
               (HASH form ((string ?are-unit "are")
                           (meets ?are-unit ?there-unit))))
              (?there-unit
               (HASH meaning ((exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?there-unit "there")
                           (meets ?there-unit ?leftmost-conjunction-unit))))
              (?conjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction conjunction))
               (leftmost-unit ?leftmost-conjunction-unit)
               (rightmost-unit ?rightmost-conjunction-unit)))
             :cxn-set cxn)

(def-fcg-cxn are-there-both-conjunction-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-there-unit ?conjunction-unit)))
              <-
              (?are-there-unit
               (HASH meaning ((get-context ?context)
                              (exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?are-there-unit "are there both")
                           (meets ?are-there-unit ?leftmost-conjunction-unit))))
              (?conjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction conjunction))
               (leftmost-unit ?leftmost-conjunction-unit)
               (rightmost-unit ?rightmost-conjunction-unit)))
             :cxn-set cxn)

(def-fcg-cxn are-there-both-conjunction-in-view-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?are-there-unit ?conjunction-unit ?in-unit ?view-unit)))
              <-
              (?are-there-unit
               (HASH meaning ((exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?are-there-unit "are there both")
                           (meets ?are-there-unit ?leftmost-conjunction-unit))))
              (?conjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction conjunction))
               (leftmost-unit ?leftmost-conjunction-unit)
               (rightmost-unit ?rightmost-conjunction-unit))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in")
                           (meets ?rightmost-conjunction-unit ?in-unit)
                           (meets ?in-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))))
             :cxn-set cxn)

(def-fcg-cxn is-there-both-conjunction-in-view-cxn
             ((?exist-unit
               (args ((sources ?context)
                      (target ?target)))
               (subunits (?is-there-unit ?conjunction-unit ?in-unit ?view-unit)))
              <-
              (?is-there-unit
               (HASH meaning ((exist ?bool-1 ?set-1)
                              (exist ?bool-2 ?set-2)))
               --
               (HASH form ((string ?is-there-unit "is there both")
                           (meets ?is-there-unit ?leftmost-conjunction-unit))))
              (?conjunction-unit
               (args ((sources ?context)
                      (target ?target)
                      (bindings ?set-1 ?set-2 ?bool-1 ?bool-2)))
               (sem-cat (sem-function referring-expression))
               --
               (syn-cat (phrase-type conjuncted-clauses)
                        (conjunction conjunction))
               (leftmost-unit ?leftmost-conjunction-unit)
               (rightmost-unit ?rightmost-conjunction-unit))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in")
                           (meets ?rightmost-conjunction-unit ?in-unit)
                           (meets ?in-unit ?leftmost-view-unit))))
              (?view-unit
               (args ((target ?context)))
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))
               --
               (leftmost-unit ?leftmost-view-unit)
               (rightmost-unit ?rightmost-view-unit)
               (sem-cat (sem-class view))
               (syn-cat (phrase-type det-noun))))
             :cxn-set cxn)

