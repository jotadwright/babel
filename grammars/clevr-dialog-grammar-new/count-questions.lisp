(in-package :clevr-dialog-grammar)

(def-fcg-cxn how-many-x-are-in-the-image-cxn
             ((?how-many-x-are-in-the-image-unit
               (subunits (?how-many-unit ?x-unit ?are-unit ?in-unit ?the-unit ?image-unit))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               (context +))
               <-
              (?how-many-x-are-in-the-image-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit ?are-unit) (meets ?are-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit))))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-x-are-there-cxn
             ((?how-many-x-are-there-unit
               (subunits (?how-many-unit ?x-unit ?are-unit ?there-unit))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?there-unit))
               (present -)
               (context +))
               <-
              (?how-many-x-are-there-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit ?are-unit) (meets ?are-unit ?there-unit) )))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              
              (?there-unit
               --
               (HASH form ((string ?the-unit "there"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-x-are-there-in-the-image-cxn
             ((?how-many-x-are-there-in-the-image-unit
               (subunits (?how-many-unit ?x-unit ?are-unit ?there-unit ?in-unit ?the-unit ?image-unit))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               (context +))
               <-
              (?how-many-x-are-there-in-the-image-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit ?are-unit) (meets ?are-unit ?there-unit) (meets ?there-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit))))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?segmented-scene)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn if-present-cxn
             ((?if-present-unit
               (subunits (?if-unit ?present-unit)))
              <-
              (?if-present-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               (context +)
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               (context +)
               (HASH form ((string ?if-unit "if") (string ?present-unit "present") (meets ?if-unit ?present-unit) (meets ?present-unit ?how-unit)))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn if-present-2-cxn
             ((?if-present-unit
               (subunits (?if-unit ?present-unit)))
              <-
              (?if-present-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (present -)
               
               (HASH form ((string ?if-unit "if") (string ?present-unit "present") (meets ?if-unit ?present-unit) (meets ?image-unit ?if-unit)))))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn count-x-cxn
             ((?count-x-unit
               (subunits (?count-unit ?x-unit))
               (args ((context ?context)))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class count-question))
               (context -))
               <-
              (?count-x-unit
               ;(HASH meaning ((get-context ?context)))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit))))
              (?count-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        ;(number plural)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        ;(number plural)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?context)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?context)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)




(def-fcg-cxn how-many-x-are-present-in-the-image-cxn
             ((?how-many-x-are-in-the-image-unit
               (subunits (?how-many-unit ?x-unit ?are-unit ?present-unit ?in-unit ?the-unit ?image-unit))
               (context +))
               <-
              (?how-many-x-are-in-the-image-unit
               (HASH meaning ((segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit)
                           (meets ?rightmost-unit ?are-unit)
                           (meets ?are-unit ?present-unit)
                           (meets ?present-unit ?in-unit)
                           (meets ?in-unit ?the-unit)
                           (meets ?the-unit ?image-unit))))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target)
                      (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (scene-unit
               --
               (scene ?scene))
              )
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-x-are-present-cxn
             ((?how-many-x-are-in-the-image-unit
               (subunits (?how-many-unit ?x-unit ?are-unit ?present-unit))
               (context +))
               <-
              (?how-many-x-are-in-the-image-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit ?are-unit) (meets ?are-unit ?present-unit) )))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (original-source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (original-source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-in-the-image-cxn
             ((?count-in-the-image-unit
               (subunits (?how-many-unit ?in-unit ?the-unit ?image-unit))
               (context +)
               (present -)
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?image-unit)))
              (?how-many-unit
               (footprints (context))
               )
               <-
               (?count-in-the-image-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?rightmost-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit))))
              (?how-many-unit
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT context))
               --
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT context)))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (scene-unit
               --
               (scene ?scene))
              )
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-context-cxn
             ((?count-context-unit
               (footprints (context))
               
               (present -)
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
               <-
              (?count-context-unit
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT context))
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT context)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-x-are-present-to-its-R-cxn
             ((?count-x-are-present-to-its-R-unit
               (subunits (?count-x-unit ?are-unit ?present-unit ?to-its-R-unit))
               (args ((context ?context)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?count-x-unit
               (footprints (context)))
              <-
              (?count-x-are-present-to-its-r-unit
               --
               (HASH form ((meets ?rightmost-count-unit ?are-unit)
                           (meets ?are-unit ?present-unit)
                           (meets ?present-unit ?leftmost-relation-unit))))
              (?count-x-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context))
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?to-its-r-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))
               (mentioned-object nil)
               --
               (mentioned-object nil)
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-x-are-present-relation-cxn
             ((?count-x-are-present-relation-unit
               (subunits (?count-unit ?x-unit ?are-unit ?present-unit ?relation-unit))
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?count-unit
               (footprints (context)))
              <-
              (?count-x-are-present-relation-unit
               (HASH meaning ((relate ?np-source ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit)
                           (meets ?rightmost-unit ?are-unit)
                           (meets ?are-unit ?present-unit)
                           (meets ?present-unit ?leftmost-relation-unit))))
              (?count-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        ;(number plural)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        ;(number plural)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?np-source)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?np-source)))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?relation-unit
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((target ?unique) (source ?segmented-scene) (relation ?relation)))
               (sem-cat (sem-class relation-noun))
               --
               (syn-cat (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((target ?unique) (source ?segmented-scene) (relation ?relation)))
               (sem-cat (sem-class relation-noun)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-x-in-total-cxn
             ((?how-many-x-in-total-unit
               (subunits (?how-many-unit ?x-unit  ?in-unit  ?total-unit))
               (args ((context ?context)))
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?total-unit))
               (context -))
               <-
              (?how-many-x-in-total-unit
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit  ?in-unit) (meets ?in-unit ?total-unit))))
              (?how-many-unit
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit  ?in-unit) (meets ?in-unit ?total-unit)))
               (args ((source ?target)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (original-source ?context)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (original-source ?context)))
               
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?total-unit
               --
               (HASH form ((string ?total-unit "total")))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn how-many-other-cxn
             ((?how-many-other-unit
               (subunits (?how-many-unit ?other-unit))
               (args ((original-source ?inset) (source ?original-source) (target ?target) (count ?out)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit))
               (context -))
              (?other-unit
               (footprints (how-many)))
               <-
              (?how-many-other-unit
               --
               (HASH form ((meets ?many-unit ?leftmost-other-unit))))
              (?how-many-unit
               (args ((source ?out)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?out)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?other-unit
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT history))
               --
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT history))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-other-in-the-image-cxn
             ((?how-many-other-unit
               (subunits (?how-many-unit ?other-unit ?in-unit ?the-unit ?image-unit))
               (args ((original-source ?segmented-scene) (source ?original-source) (target ?target) (count ?out)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?image-unit))
               (context +))
              (?other-unit
               (footprints (how-many)))
               <-
              (?how-many-other-unit
               (HASH meaning (;(get-context ?inset)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?many-unit ?leftmost-other-unit) (meets ?rightmost-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit)))
               )
              (?how-many-unit
               (args ((source ?out)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?out)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?other-unit
               (args ((target ?target) (source ?segmented-scene) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT history))
               
               --
               (args ((target ?target) (source ?segmented-scene) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT history)))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-other-share-its-attr-cxn
             ((?count-other-sharing-attr-unit
               (subunits (?how-many-other-unit ?sharing-unit ?possessive-unit))
               (args ((context ?context)))
               (context ?cont)
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit)))
              <-
              (?count-other-sharing-attr-unit
               (HASH meaning ((filter-by-attribute ?count ?source ?scene ?specific-attr)
                              (query ?specific-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?rightmost-count-unit ?sharing-unit) (meets ?sharing-unit ?leftmost-poss-unit))))
              (?how-many-other-unit
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               --
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               )
              (?sharing-unit
               --
               (HASH form ((string ?sharing-unit "share"))))
              (?possessive-unit
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-other-have-its-attr-cxn
             ((?count-other-sharing-attr-unit
               (subunits (?how-many-other-unit ?sharing-unit ?possessive-unit))
               (args ((context ?context)))
               (context ?cont)
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit)))
              <-
              (?count-other-sharing-attr-unit
               (HASH meaning ((filter-by-attribute ?count ?source ?scene ?specific-attr)
                              (query ?specific-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?rightmost-count-unit ?sharing-unit) (meets ?sharing-unit ?leftmost-poss-unit))))
              (?how-many-other-unit
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               --
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               )
              (?sharing-unit
               --
               (HASH form ((string ?sharing-unit "have"))))
              (?possessive-unit
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-other-that-share-its-attr-cxn
             ((?count-other-sharing-attr-unit
               (subunits (?how-many-other-unit ?that-unit ?sharing-unit ?possessive-unit))
               (args ((context ?context)))
               (context ?cont)
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit)))
              <-
              (?count-other-sharing-attr-unit
               (HASH meaning ((filter-by-attribute ?count ?source ?scene ?specific-attr)
                              (query ?specific-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?rightmost-count-unit ?that-unit) (meets ?that-unit ?sharing-unit)  (meets ?sharing-unit ?leftmost-poss-unit))))
              (?how-many-other-unit
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               --
               (args ((original-source ?context) (target ?source) (source ?object) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               )
              (?that-unit
               --
               (HASH form ((string ?that-unit "that"))))
              (?sharing-unit
               --
               (HASH form ((string ?sharing-unit "share"))))
              (?possessive-unit
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?unique) (object ?object) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn share-similar-attr-with-cxn
             ((?sharing-similar-attr-with-unit
               (args ((target ?target) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?share-unit)
                        (rightmost-unit ?with-unit))
               (sem-cat (sem-class sharing))
               (subunits (?share-unit ?similar-unit ?attribute-unit ?with-unit)))
               <-
              (?sharing-similar-attr-with-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?spec-attr)
                              (query ?spec-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?share-unit ?similar-unit) (meets ?similar-unit ?leftmost-unit) (meets ?rightmost-unit ?with-unit))))
              (?share-unit
               --
               (HASH form ((string ?share-unit "share"))))
              (?similar-unit
               --
               (HASH form ((string ?similar-unit "similar"))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?with-unit
               --
               (HASH form ((string ?with-unit "with"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn are-of-same-attr-as-cxn
             ((?sharing-similar-attr-with-unit
               (args ((target ?target) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?are-unit)
                        (rightmost-unit ?as-unit))
               (sem-cat (sem-class sharing))
               (subunits (?are-unit ?of-unit ?same-unit ?attribute-unit ?as-unit)))
               <-
              (?sharing-similar-attr-with-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?spec-attr)
                              (query ?spec-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?are-unit ?of-unit) (meets ?of-unit ?same-unit) (meets ?same-unit ?leftmost-unit) (meets ?rightmost-unit ?as-unit))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?same-unit
               --
               (HASH form ((string ?same-unit "same"))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?as-unit
               --
               (HASH form ((string ?as-unit "as"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn are-of-same-attr-with-cxn
             ((?sharing-similar-attr-with-unit
               (args ((target ?target) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?are-unit)
                        (rightmost-unit ?with-unit))
               (sem-cat (sem-class sharing))
               (subunits (?are-unit ?of-unit ?same-unit ?attribute-unit ?with-unit)))
               <-
              (?sharing-similar-attr-with-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?spec-attr)
                              (query ?spec-attr ?unique ?scene ?attribute)))
               --
               )
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?same-unit
               --
               (HASH form ((string ?same-unit "same"))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?with-unit
               --
               (HASH form ((string ?with-unit "with"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn have-the-same-attr-as-cxn
             ((?sharing-similar-attr-with-unit
               (args ((target ?target) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?have-unit)
                        (rightmost-unit ?as-unit))
               (sem-cat (sem-class sharing))
               (subunits (?have-unit ?the-unit ?same-unit ?attribute-unit ?as-unit)))
               <-
              (?sharing-similar-attr-with-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?spec-attr)
                              (query ?spec-attr ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?have-unit ?the-unit) (meets ?the-unit ?same-unit) (meets ?same-unit ?leftmost-unit) (meets ?rightmost-unit ?as-unit))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?same-unit
               --
               (HASH form ((string ?same-unit "same"))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?as-unit
               --
               (HASH form ((string ?as-unit "as"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-other-sharing-earlier-cxn
             ((?count-other-sharing-earlier-unit
               (subunits (?how-many-other-unit ?sharing-unit ?anaphoric-unit))
               (args ((context ?context)))
               (context ?cont)
               (syn-cat (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-np-unit)))
              <-
              (?count-other-sharing-earlier-unit
               --
               (HASH form ((meets ?rightmost-count-unit ?leftmost-sharing-unit) (meets ?rightmost-sharing-unit ?leftmost-np-unit))))
              (?how-many-other-unit
               (args ((original-source ?context) (target ?source) (source ?object-set) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               --
               (args ((original-source ?context) (target ?source) (source ?object-set) (count ?count)))
               (syn-cat (syn-class question-count-other)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (context ?cont)
               )
              (?sharing-unit
               (args ((target ?count) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-class sharing))
               --
               (args ((target ?count) (source ?source) (original-source ?unique)))
               (syn-cat (syn-class vp)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-class sharing)))
              (?anaphoric-unit
               (args ((target ?unique) (source ?context) (input ?object-set) ;(original-source ?history)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit)
                        (meaning +))
               --
               (args ((target ?unique) (source ?context) (input ?object-set) ;(original-source ?history)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit)
                        (meaning +))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn count-x-does-it-have-to-its-R-cxn
             ((?count-x-does-it-have-to-its-R-unit
               (subunits (?count-x-unit ?does-unit ?it-unit ?have-unit ?to-its-R-unit))
               (args ((context ?context)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?count-x-unit
               (footprints (context)))
              <-
              (?count-x-does-it-have-to-its-r-unit
               --
               (HASH form ((meets ?rightmost-count-unit ?does-unit)
                           (meets ?does-unit ?it-unit)
                           (meets ?it-unit ?have-unit)
                           (meets ?have-unit ?leftmost-relation-unit))))
              (?count-x-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context))
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context)))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              
              (?it-unit
               --
               (HASH form ((string ?it-unit "it"))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?to-its-r-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))
               (mentioned-object nil)
               --
               (mentioned-object nil)
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-x-to-its-R-cxn
             ((?count-x-to-its-R-unit
               (subunits (?count-x-unit ?to-its-R-unit))
               (args ((context ?context)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              (?count-x-unit
               (footprints (context)))
              <-
              (?count-x-to-its-r-unit
               --
               (HASH form ((meets ?rightmost-count-unit ?leftmost-relation-unit))))
              (?count-x-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context))
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context)))
              (?to-its-r-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))
               (mentioned-object nil)
               --
               (mentioned-object nil)
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-relation-unit)
                        (rightmost-unit ?rightmost-relation-unit))
               (args ((context ?context) (target ?objects)))
               (sem-cat (sem-class possessive))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-x-does-x-have-to-its-R-cxn
             ((?count-x-does-it-have-to-its-R-unit
               (subunits (?count-x-unit ?does-unit ?x-unit ?have-unit ?to-unit ?its-unit ?relation-unit))
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?relation-unit)))
              (?count-x-unit
               (footprints (context)))
              <-
              (?count-x-does-it-have-to-its-r-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-count-unit ?does-unit)
                           (meets ?does-unit ?leftmost-np-unit)
                           (meets ?rightmost-np-unit ?have-unit)
                           (meets ?have-unit ?to-unit)
                           (meets ?to-unit ?its-unit)
                           (meets ?its-unit ?leftmost-rel-unit))))
              (?count-x-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context))
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context)))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              
              (?x-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (sem-cat (sem-class det-anaphoric))
               (args ((target ?unique) (source ?segmented-scene))) 
               --
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (sem-cat (sem-class det-anaphoric))
               (args ((target ?unique) (source ?segmented-scene))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?its-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?relation-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation))
               --
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn count-x-has-to-its-R-cxn
             ((?count-x-does-it-have-to-its-R-unit
               (subunits (?count-x-unit ?x-unit ?has-unit ?to-unit ?its-unit ?relation-unit))
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?relation-unit)))
              (?count-x-unit
               (footprints (context)))
              <-
              (?count-x-does-it-have-to-its-r-unit
               (HASH meaning ((relate ?objects ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?rightmost-count-unit ?leftmost-np-unit)
                           (meets ?rightmost-np-unit ?has-unit)
                           (meets ?has-unit ?to-unit)
                           (meets ?to-unit ?its-unit)
                           (meets ?its-unit ?leftmost-rel-unit))))
              (?count-x-unit
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context))
               --
               (syn-cat (syn-class question)
                        (leftmost-unit ?leftmost-count-unit)
                        (rightmost-unit ?rightmost-count-unit))
               (args ((context ?objects)))
               (sem-cat (sem-class count-question))
               (context -)
               (footprints (NOT context)))
              (?x-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (sem-cat (sem-class det-anaphoric))
               (args ((target ?unique) (source ?segmented-scene))) 
               --
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (sem-cat (sem-class det-anaphoric))
               (args ((target ?unique) (source ?segmented-scene))))
              (?has-unit
               --
               (HASH form ((string ?has-unit "has"))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?its-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?relation-unit
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation))
               --
               (args ((target ?relation)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?leftmost-rel-unit)
                        (rightmost-unit ?rightmost-rel-unit))
               (sem-cat (sem-class relation)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-of-them-cxn
             ((?how-many-of-them-unit
               (subunits (?how-unit ?many-unit ?of-unit ?them-unit)))
              <-
              (?how-many-of-them-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              ;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              (count-objects ?count ?target)))
               --
               (HASH form ((meets ?how-unit ?many-unit) (meets ?many-unit ?of-unit) (meets ?of-unit ?them-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?them-unit
               --
               (HASH form ((string ?them-unit "them"))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn what-number-of-them-cxn
             ((?what-number-of-them-unit
               (subunits (?what-unit ?number-unit ?of-unit ?them-unit)))
              <-
              (?what-number-of-them-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?target)))
               --
               (HASH form ((meets ?what-unit ?number-unit) (meets ?number-unit ?of-unit) (meets ?of-unit ?them-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?number-unit
               --
               (HASH form ((string ?number-unit "number"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?them-unit
               --
               (HASH form ((string ?them-unit "them"))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-of-them-are-x-cxn
             ((?how-many-of-them-are-x-unit
               (subunits (?how-unit ?many-unit ?of-unit ?them-unit ?are-unit ?x-unit)))
              <-
              (?how-many-of-them-are-x-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?out)))
               --
               (HASH form ((meets ?how-unit ?many-unit) (meets ?many-unit ?of-unit) (meets ?of-unit ?them-unit) (meets ?them-unit ?are-unit) (meets ?are-unit ?leftmost-unit ))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?them-unit
               --
               (HASH form ((string ?them-unit "them"))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?x-unit
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-many-question-cxn
             ((?how-many-unit
               (subunits (?how-unit ?many-unit)))
              <-
              (?how-many-unit
               (HASH meaning ((get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              (count-objects ?count ?target)))
               --
               (HASH form ((string ?how-unit "how")
                           (string ?many-unit "many")
                           (meets ?how-unit ?many-unit))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :score 0.10
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn how-many-such-things-cxn
             ((?how-many-such-things-unit

               (subunits (?how-unit ?many-unit ?such-unit ?things-unit))
               )
              <-
              (?how-many-such-things-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?out)))
               --
               (HASH form ((meets ?how-unit ?many-unit) (meets ?many-unit ?such-unit) (meets ?such-unit ?leftmost-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many"))))
              (?such-unit
               --
               (HASH form ((string ?such-unit "such"))))
              (?things-unit
               (args ((target ?out) (source ?target)))
               
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?out) (source ?target)))
               
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn how-many-such-things-in-the-group-cxn
             ((?how-many-such-things-unit

               (subunits (?how-unit ?many-unit ?such-unit ?things-unit ?in-unit ?the-unit ?group-unit))
               )
              <-
              (?how-many-such-things-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?out)))
               --
               (HASH form ((meets ?how-unit ?many-unit) (meets ?many-unit ?such-unit) (meets ?such-unit ?leftmost-unit) (meets ?rightmost-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?group-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many"))))
              (?such-unit
               --
               (HASH form ((string ?such-unit "such"))))
              (?things-unit
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?group-unit
               --
               (HASH form ((string ?group-unit "group"))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn how-many-x-are-there-among-them-cxn
             ((?how-many-x-are-there-among-them-unit

               (subunits (?how-unit ?many-unit ?x-unit ?are-unit ?there-unit ?among-unit ?them-unit))
               )
              <-
              (?how-many-x-are-there-among-them-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?out)))
               --
               (HASH form ((meets ?how-unit ?many-unit) (meets ?many-unit ?leftmost-unit) (meets ?rightmost-unit ?are-unit) (meets ?are-unit ?there-unit) (meets ?there-unit ?among-unit) (meets ?among-unit ?them-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many"))))
              
              (?x-unit
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?out) (source ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?among-unit
               --
               (HASH form ((string ?among-unit "among"))))
              (?them-unit
               --
               (HASH form ((string ?them-unit "them"))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn what-number-cxn
             ((?what-number-unit
               (subunits (?what-unit ?number-unit)))
              <-
              (?what-number-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)
                              (segment-scene ?segmented-scene ?scene)
                              ;(get-context ?context)
                              (count-objects ?count ?target)))
               --
               (HASH form ((string ?what-unit "what")
                           (string ?number-unit "number")
                           (meets ?what-unit ?number-unit))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)