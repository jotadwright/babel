(in-package :clevr-dialog-grammar)

(def-fcg-cxn caption-cxn
             ((?caption-unit
               (footprints (caption)))
               <-
              (?caption-unit
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption))
               ;(HASH meaning ((get-context ?context)))
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               (footprints (NOT caption))
               --
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption))
               (footprints (NOT caption)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn in-the-image-caption-cxn
             ((?in-the-image-unit
               (subunits (?in-unit ?the-unit ?image-unit ?caption-unit)))
              (?caption-unit
               (footprints (caption)))
              <-
              (?in-the-image-unit
               ;(HASH meaning ((get-context ?context)))
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?rightmost-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              
              (?caption-unit
               (args ((context ?segmented-scene))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT caption))
               
               --
               (footprints (NOT caption))
               (args ((context ?segmented-scene))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption) 
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn of-the-image-caption-cxn
             ((?of-the-image-unit
               (subunits (?of-unit ?the-unit ?image-unit ?caption-unit)))
              (?caption-unit
               (footprints (caption)))
              <-
              (?of-the-image-unit
               ;(HASH meaning ((get-context ?context)))
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?rightmost-unit ?of-unit) (meets ?of-unit ?the-unit) (meets ?the-unit ?image-unit))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?caption-unit
               (args ((context ?segmented-scene))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT caption))
               
               --
               (footprints (NOT caption))
               (args ((context ?segmented-scene))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption) 
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn there-is-X-cxn
             ((?there-is-X-unit
               (subunits (?there-unit ?is-unit ?X-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-unit))
               (args ((context ?source))))

              <-
              (?there-is-X-unit
               (HASH meaning ((exist yes ?target)))
               --
               (HASH form ((meets ?there-unit ?is-unit) (meets ?is-unit ?leftmost-unit))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?X-unit 
               (args ((target ?target)
                      (source ?source)
                      ))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit))
               --
               (args ((target ?target)
                      (source ?source)
                      ))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn there-are-X-cxn
             ((?there-are-X-unit
               (subunits (?there-unit ?are-unit ?X-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-unit))
               (args ((context ?context))))
              <-
              (?there-are-X-unit
               --
               (HASH form ((meets ?there-unit ?are-unit) (meets ?are-unit ?leftmost-multiple-unit))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?x-unit
               (args ((target yes) (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target yes) (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn the-image-has-multiple-X-cxn
             ((?the-image-has-multiple-X-unit
               (subunits (?the-unit ?image-unit ?has-unit ?X-unit)))
              <-
              (?the-image-has-multiple-X-unit
               ;(HASH meaning ((get-context ?context)))
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?the-unit ?image-unit) (meets ?image-unit ?has-unit) (meets ?has-unit ?leftmost-multiple-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?has-unit
               --
               (HASH form ((string ?has-unit "has"))))
              (?x-unit
               (args ((target yes) (source ?segmented-scene)
                      ;(original-source ?segmented-scene)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target yes) (source ?segmented-scene)
                      ;(original-source ?segmented-scene)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn the-image-has-one-X-cxn
             ((?the-image-has-multiple-X-unit
               (subunits (?the-unit ?image-unit ?has-unit ?X-unit)))
              <-
              (?the-image-has-multiple-X-unit
               ;(HASH meaning ((get-context ?context)))
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?the-unit ?image-unit) (meets ?image-unit ?has-unit) (meets ?has-unit ?leftmost-multiple-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?has-unit
               --
               (HASH form ((string ?has-unit "has"))))
              (?x-unit
               (args ((target yes) (source ?segmented-scene)
                      ;(original-source ?segmented-scene)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target yes) (source ?segmented-scene)
                      ;(original-source ?segmented-scene)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)|#

(def-fcg-cxn multiple-X-are-present-cxn
             ((?X-are-present-unit
               (subunits (?present-unit ?are-unit ?X-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?present-unit))
               (args ((context ?context))))
              <-
              (?X-are-present-unit
               ;(HASH meaning ((get-context ?context)))
               --
               (HASH form ((meets ?rightmost-unit ?are-unit) (meets ?are-unit ?present-unit))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?x-unit
               (args ((target yes) (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target yes) (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn the-image-has-X-cxn
             ((?the-image-has-X-unit
               (subunits (?the-unit ?image-unit ?has-unit ?X-unit)))
              <-
              (?the-image-has-X-unit
               (HASH meaning ((exist yes ?target)
                              ;(get-context ?original-input )
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?the-unit ?image-unit ) (meets ?image-unit ?has-unit) (meets ?has-unit ?leftmost-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?has-unit
               --
               (HASH form ((string ?has-unit "has"))))
              (?X-unit 
               (args ((target ?target)
                      (source ?segmented-scene)
                      ))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit))
               --
               (args ((target ?target)
                      (source ?segmented-scene)
                      ))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn no-other-X-except-for-one
             ((?no-other-X-except-for-one-unit
               (subunits (?no-unit ?other-unit ?x-unit ?except-unit ?for-unit ?one-unit)))
              <-
              (?no-other-x-except-for-one-unit
               (HASH meaning ((exist yes ?unique)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?no-unit ?other-unit)
                           (meets ?other-unit ?leftmost-unit)
                           (meets ?rightmost-unit ?except-unit)
                           (meets ?except-unit ?for-unit)
                           (meets ?for-unit ?one-unit))))
              (?no-unit
               --
               (HASH form ((string ?no-unit "no"))))
              (?other-unit
               --
               (HASH form ((string ?other-unit "other"))))
              (?x-unit
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit)))
              (?except-unit
               --
               (HASH form ((string ?except-unit "except"))))
              (?for-unit
               --
               (HASH form ((string ?for-unit "for"))))
              (?one-unit
               (HASH meaning ((unique ?unique ?target)))
               --
               (HASH form ((string ?one-unit "one"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn no-other-X-except-for-exactly-one
             ((?no-other-X-except-for-one-unit
               (subunits (?no-unit ?other-unit ?x-unit ?except-unit ?for-unit ?exactly-unit ?one-unit)))
              <-
              (?no-other-x-except-for-one-unit
               (HASH meaning ((exist yes ?unique)
                             ; (get-context ?source)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?no-unit ?other-unit) (meets ?other-unit ?leftmost-unit) (meets ?rightmost-unit ?except-unit) (meets ?except-unit ?for-unit)(meets ?for-unit ?leftmost-one-unit))))
              (?no-unit
               --
               (HASH form ((string ?no-unit "no"))))
              (?other-unit
               --
               (HASH form ((string ?other-unit "other"))))
              (?x-unit
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit)))
              (?except-unit
               --
               (HASH form ((string ?except-unit "except"))))
              (?for-unit
               --
               (HASH form ((string ?for-unit "for"))))
              (?exactly-unit
               --
               (HASH form ((string ?exactly-unit "exactly"))))
              (?one-unit
               (HASH meaning ((unique ?unique ?target)))
               --
               (HASH form ((string ?one-unit "one"))))
              #|(?one-unit
               (args ((target ?unique) (source ?source-one)))
               (sem-cat (sem-class unique))
               (syn-cat (leftmost-unit ?leftmost-one-unit)
                        (rightmost-unit ?rightmost-one-unit))
               --
               (args ((target ?unique) (source ?source-one)))
               (syn-cat (leftmost-unit ?leftmost-one-unit)
                        (rightmost-unit ?rightmost-one-unit))
               (sem-cat (sem-class unique)))|#
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn X-is-present-R-of-Y-cxn
             ((?X-is-present-R-of-Y-unit
               (subunits (?x-unit ?is-unit ?present-unit ?relation-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?segmented-scene))))
              <-
              (?X-is-present-R-of-Y-unit
               (HASH meaning ((exist yes ?first-np-target)
                              (immediate-relate ?first-np-source ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-first-np-unit ?is-unit) (meets ?is-unit ?present-unit) (meets ?present-unit ?leftmost-relation-unit))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?relation-unit
               (args ((target ?unique)
                      ;(original-source ?segmented-scene)
                      (source ?segmented-scene)
                      (relation ?relation)
                      ;(unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?unique)
                      ;(original-source ?segmented-scene)
                      (source ?segmented-scene)
                      (relation ?relation)
                      ;(unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn X-stands-R-of-Y-cxn
             ((?X-stands-R-of-Y-unit
               (subunits (?x-unit ?stands-unit ?relation-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?segmented-scene))))
              <-
              (?X-stands-R-of-Y-unit
               (HASH meaning ((exist yes ?first-np-target)
                              (immediate-relate ?first-np-source ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-first-np-unit ?stands-unit) (meets ?stands-unit ?leftmost-relation-unit) )))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?stands-unit
               --
               (HASH form ((string ?stands-unit "stands"))))
              (?relation-unit
               (args ((target ?unique)
                      ;(original-source ?segmented-scene)
                      (source ?segmented-scene)
                      (relation ?relation)
                      ;(unique ?unique) 
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               --
               (args ((target ?unique)
                      ;(original-source ?segmented-scene)
                      (source ?segmented-scene)
                      (relation ?relation)
                      ;(unique ?unique)
                      ))
               (sem-cat (sem-class relation-noun))
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn X-is-extreme-relation-cxn
             ((?x-is-extreme-relation-unit
               (args ((context ?context))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-extreme-unit))
               (subunits (?X-unit ?is-unit ?extreme-relation-unit)))
              <-
              (?X-is-extreme-relation-unit
               (HASH meaning ((exist yes ?first-np-target)))
               --
               (HASH form ((meets ?rightmost-unit ?is-unit) (meets ?is-unit ?leftmost-extreme-relation-unit))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?extreme-relation-unit
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-unit))
               --
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn extreme-relation-is-X-cxn
             ((?x-is-extreme-relation-unit
               (subunits (?X-unit ?is-unit ?extreme-relation-unit)))
              <-
              (?X-is-extreme-relation-unit
               (HASH meaning ((exist yes ?first-np-target)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?thing-unit ?is-unit) (meets ?is-unit ?determiner-unit))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?extreme-relation-unit
               (args ((target ?first-np-source) (source ?segmented-scene)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit))
               --
               (args ((target ?first-np-source) (source ?segmented-scene)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?thing-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn extreme-relation-in-the-image-is-X-cxn
             ((?x-is-extreme-relation-unit
               (subunits (?X-unit ?is-unit ?extreme-relation-unit ?in-unit ?the-unit ?image-unit)))
              <-
              (?X-is-extreme-relation-unit
               (HASH meaning ((exist yes ?first-np-target)
                              ;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?rightmost-extreme-relation-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit) (meets ?image-unit ?is-unit) (meets ?is-unit ?determiner-unit))))
              (?extreme-relation-unit
               (args ((target ?first-np-source) (source ?segmented-scene)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-relation-unit))
               --
               (args ((target ?first-np-source) (source ?segmented-scene)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-relation-unit)))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn there-is-X-sitting-extreme-cxn
             ((?there-is-X-sitting-extreme-unit
               (subunits (?there-unit ?is-unit ?x-unit ?sitting-unit ?extreme-relation-unit))
               (args ((context ?context))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-extreme-unit))
               ;(footprints (NOT caption))
               )
              <-
              (?there-is-x-sitting-extreme-unit
               (HASH meaning ((exist yes ?first-np-target)))
               --
               (HASH form ((meets ?there-unit ?is-unit)
                           (meets ?is-unit ?determiner-unit)
                           (meets ?rightmost-first-np-unit ?sitting-unit)
                           (meets ?sitting-unit ?leftmost-extreme-relation-unit))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              #|(?x-unit
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))|#
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?sitting-unit
               --
               (HASH form ((string ?sitting-unit "sitting"))))
              (?extreme-relation-unit
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-unit))
               (footprints (NOT xmost-thing))
               --
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn there-is-X-extreme-cxn
             ((?there-is-X-extreme-unit
               (subunits (?there-unit ?is-unit ?x-unit ?extreme-relation-unit))
               (args ((context ?context))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-extreme-unit))
               ;(footprints (NOT caption))
               )
              <-
              (?there-is-x-extreme-unit
               (HASH meaning ((exist yes ?first-np-target)))
               --
               (HASH form ((meets ?there-unit ?is-unit)
                           (meets ?is-unit ?determiner-unit)
                           (meets ?rightmost-first-np-unit ?leftmost-extreme-relation-unit))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              #|(?x-unit
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target) (source ?first-np-source) (original-source ?target)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))|#
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?extreme-relation-unit
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-relation-unit))
               (footprints (NOT xmost-thing))
               --
               (args ((target ?first-np-source) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-unit))))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn there-is-X-extreme-cxn
             ((?there-is-X-extreme-unit
               (subunits (?there-unit ?is-unit ?x-unit ?extreme-relation-unit))
               (args ((context ?context))) 
               (sem-cat (sem-class caption-no-context)) 
               (syn-cat (syn-class caption)
                        (rightmost-unit ?rightmost-extreme-relation-unit))
               ;(footprints (NOT caption))
               )
              <-
              (?there-is-x-extreme-unit
               (HASH meaning ((exist yes ?first-np-target)))
               --
               (HASH form ((meets ?there-unit ?is-unit)
                           (meets ?is-unit ?determiner-unit)
                           (meets ?rightmost-first-np-unit ?leftmost-extreme-relation-unit))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?extreme-relation-unit
               (args ((target ?target) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-relation-unit))
               (footprints (NOT xmost-thing))
               --
               (args ((target ?target) (source ?context)))
               (sem-cat (sem-class extreme-relation))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-extreme-relation-unit)
                        (rightmost-unit ?rightmost-extreme-relation-unit))))
             :cxn-inventory *clevr-dialog*)|#

(def-fcg-cxn X-is-present-cxn
             ((?X-is-present-unit
               (subunits (?x-unit ?is-unit ?present-unit))
               (sem-cat (sem-class caption-no-context))
               (syn-cat (syn-class caption)
                        (rightmost-unit ?present-unit))
               (args ((context ?first-np-source))))
              <-
              (?X-is-present-unit
               (HASH meaning ((exist yes ?first-np-target)))
               --
               (HASH form ((meets ?rightmost-first-np-unit ?is-unit)
                           (meets ?is-unit ?present-unit))))
              (?X-unit
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def))
               --
               (args ((target ?first-np-target)
                      (source ?first-np-source)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (rightmost-unit ?rightmost-first-np-unit)
                        (leftmost-unit ?determiner-unit)
                        (definiteness ?def)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present")))))
             :cxn-inventory *clevr-dialog*)

               

