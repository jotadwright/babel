(in-package :clevr-dialog-grammar)

;; exist

(def-fcg-cxn are-there-X-cxn
             ((?are-there-X-unit
               (subunits (?there-unit ?are-unit ?X-unit))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question)
                        (rightmost-unit ?rightmost-unit))
               (args ((context ?context)))
               )
              (?x-unit
               (footprints (question)))
              <-
              (?are-there-X-unit
               (HASH meaning ((exist ?bool ?target)))
               --
               (HASH form ((meets ?are-unit ?there-unit) (meets ?there-unit ?leftmost-unit))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?x-unit
               (args ((target ?target) (source ?context)))
               (sem-cat (grammar clevr)) 
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))
               --
               (args ((target ?target) (source ?context)))
               (sem-cat (grammar clevr)) 
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn are-there-digits-cxn
             ((?are-there-digits-unit
               (subunits (?there-unit ?are-unit ?digits-unit))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question)
                        (rightmost-unit ?rightmost-unit))
               (args ((context ?context)))
               )
              (?digits-unit
               (footprints (question)))
              <-
              (?are-there-digits-unit
               (HASH meaning ((count-objects ?number ?target)))
               --
               (HASH form ((meets ?are-unit ?there-unit) (meets ?there-unit ?leftmost-unit))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?digits-unit
               (args ((target ?target) (source ?context)))
               (sem-cat (grammar mnist)) 
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))
               --
               (args ((target ?target) (source ?context)))
               (sem-cat (grammar mnist)) 
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn any-x-cxn
             ((?any-x-unit
               (subunits (?any-unit ?other-unit))
               (sem-cat (sem-class uncountable)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?any-unit)
                        (rightmost-unit ?rightmost-unit))
               (exist -)
               (args ( (original-source ?original-source)(source ?original-source) (target ?target)))
               (footprints (any)))
              <-
              (?any-x-unit
               --
               (HASH form ((meets ?any-unit ?leftmost-unit))))
              (?any-unit
               --
               (HASH form ((string ?any-unit "any"))))
              (?other-unit
               (args ((source ?original-source) (target ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               --
               (args ((source ?original-source) (target ?target)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))))
             :cxn-inventory *clevr-dialog*)
               
(def-fcg-cxn exist-cxn
             ((?exist-unit
               (footprints (exist-question)))
              <-
              (?exist-unit
               (sem-cat (sem-class uncountable))
               (syn-cat (syn-class np)
                        (leftmost-unit ?any-unit)
                        (rightmost-unit ?rightmost-unit))
               (exist -)
               (args ((source ?segmented-scene) (target ?target)))
               (HASH meaning (;(get-context ?original-source)
                              (segment-scene ?segmented-scene ?scene)
                              (exist ?bool ?target)))
               (footprints (NOT exist-question question))
               --
               (sem-cat (sem-class uncountable))
               (syn-cat (syn-class np)
                        (leftmost-unit ?any-unit)
                        (rightmost-unit ?rightmost-unit))
               (exist -)
               (args ((source ?segmented-scene) (target ?target)))
               (footprints (NOT exist-question question)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn exist-in-the-image-cxn
             ((?exist-unit
               (subunits (?in-unit ?the-unit ?image-unit))
               (footprints (exist-question)))
              <-
              (?exist-unit
               (sem-cat (sem-class uncountable))
               (syn-cat (syn-class np)
                        (leftmost-unit ?any-unit)
                        (rightmost-unit ?rightmost-unit))
               (exist -)
               (args ((source ?segmented-scene) (target ?target)))
               (HASH meaning (;(get-context ?original-source)
                              (segment-scene ?segmented-scene ?scene)
                              (exist ?bool ?target)))
               (footprints (NOT exist-question question))
               --
               (sem-cat (sem-class uncountable))
               (syn-cat (syn-class np)
                        (leftmost-unit ?any-unit)
                        (rightmost-unit ?rightmost-unit))
               (exist -)
               (args ((source ?segmented-scene) (target ?target)))
               (HASH form ((meets ?rightmost-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit)))
               (footprints (NOT exist-question question)))
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

(def-fcg-cxn exist-question-cxn
             ((?exist-question-unit
               (footprints (exist-question)))
               <-
              (?exist-question-unit
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question))
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               (footprints (NOT exist-question))
               --
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question))
               (footprints (NOT exist-question)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn in-the-image-exist-question-cxn
             ((?in-the-image-exist-question-unit
               (subunits (?in-unit ?the-unit ?image-unit ?exist-question-unit)))
              (?exist-question-unit
               (footprints (exist-question)))
              <-
              (?in-the-image-exist-question-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?rightmost-unit ?in-unit)
                           (meets ?in-unit ?the-unit)
                           (meets ?the-unit ?image-unit))
               ))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?exist-question-unit
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))
               --
               (args ((context ?segmented-scene)))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn does-the-image-have-X-cxn
             ((?does-the-image-have-x-unit
               (subunits (?does-unit ?the-unit ?image-unit ?have-unit ?x-unit)))
              <-
              (?does-the-image-have-x-unit
               (HASH meaning ((exist ?boolean ?out)
                              ;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)))
               --
               (HASH form ((meets ?does-unit ?the-unit)
                           (meets ?the-unit ?image-unit)
                           (meets ?image-unit ?have-unit)
                           (meets ?have-unit ?leftmost-other-unit))))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?x-unit
               (args ((target ?out) (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               ;(sem-cat (sem-cat relative))
               (footprints (NOT exist-question))
               --
               (args ((target ?out) (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               ;(sem-cat (sem-cat relative))))
               (footprints (NOT exist-question)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn does-the-image-contain-X-cxn
             ((?does-the-image-contain-x-unit
               (subunits (?does-unit ?the-unit ?image-unit ?contain-unit ?x-unit)))
              (?x-unit
               (footprints (question)))
              <-
              (?does-the-image-contain-x-unit
               (HASH meaning ((exist ?boolean ?out)
                              ;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?does-unit ?the-unit)
                           (meets ?the-unit ?image-unit)
                           (meets ?image-unit ?contain-unit)
                           (meets ?have-unit ?leftmost-other-unit))))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?image-unit
               --
               (lex-id image))
              (?contain-unit
               --
               (HASH form ((string ?contain-unit "contain"))))
              (?x-unit
               (args ((target ?out) (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               ;(sem-cat (sem-cat relative))
               (footprints (NOT exist-question))
               --
               (args ((target ?out) (source ?segmented-scene)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               ;(sem-cat (sem-cat relative))))
               (footprints (NOT exist-question)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn does-it-have-x-cxn
             ((?does-it-have-x-unit
               (subunits (?does-unit ?it-unit ?have-unit ?X-unit))
               (sem-cat (sem-class exist-question-no-context))
               (syn-cat (syn-class exist-question)
                        (rightmost-unit ?rightmost-unit))
               (args ((context ?context))))
              (?x-unit
               (footprints (question)))
              <-
              (?does-it-have-x-unit
               (HASH meaning ((exist ?bool ?target)))
               --
               (HASH form ((meets ?does-unit ?it-unit) (meets ?it-unit ?have-unit) (meets ?have-unit ?leftmost-unit))))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              (?it-unit
               --
               (HASH form ((string ?it-unit "it"))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?x-unit
               (args ((target ?target) (source ?context)))
               ;(sem-cat (sem-class plural-adj-nom)) 
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))
               --
               (args ((target ?target) (source ?context)))
               ;(sem-cat (sem-class plural-adj-nom)) 
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit) 
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT exist-question))))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn do-they-have-x-cxn
             ((?do-they-have-x-unit
               (subunits (?do-unit ?they-unit ?have-unit ?x-unit)))
              (?x-unit
               (footprints (question)))
              <-
              (?do-they-have-x-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              (exist ?bool ?source)
                              ;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?segmented-scene ?objects)))
               --
               (HASH form ((meets ?do-unit ?they-unit) (meets ?they-unit ?have-unit) (meets ?have-unit ?leftmost-unit))))
              (?do-unit
               --
               (HASH form ((string ?do-unit "do"))))
              (?they-unit
               --
               (HASH form ((string ?they-unit "they"))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?x-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (args ((source ?target) (target ?source)))
               (footprints (NOT exist-question))
               --
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (args ((source ?target) (target ?source)))
               (footprints (NOT exist-question)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn exist-or-count-to-its-R-cxn
             ((?exist-or-count-to-its-R-cxn
               (subunits (?exist-or-count-unit ?to-its-R-unit)))
              <-
              (?exist-or-count-to-its-r-cxn
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?rightmost-exist-unit ?leftmost-R-unit))))
              (?exist-or-count-unit
               (args ((source ?objects)))
               (sem-cat (sem-class exist-question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?leftmost-exist-unit)
                        (rightmost-unit ?rightmost-exist-unit))
               --
               (args ((source ?objects)))
               (sem-cat (sem-class exist-question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?leftmost-exist-unit)
                        (rightmost-unit ?rightmost-exist-unit)))
              (?to-its-R-unit
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (mentioned-object t)
               --
               (mentioned-object t)
               (args ((target ?objects) (context ?segmented-scene)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn x-to-its-R-cxn
             ((?x-to-its-R-cxn
               (args ((target ?target) (original-source ?context) (source ?context)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit) 
                        (rightmost-unit ?rightmost-R-unit))
               (subunits (?x-unit ?to-its-R-unit)))
              <-
              (?x-to-its-r-cxn
               --
               (HASH form ((meets ?rightmost-x-unit ?leftmost-R-unit))))
              (?x-unit
               (args ((target ?target) (source ?objects) (original-source ?objects)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit)
                        (rightmost-unit ?rightmost-x-unit))
               --
               (sem-cat (grammar ?grammar))
               (args ((target ?target) (source ?objects) (original-source ?objects)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit)
                        (rightmost-unit ?rightmost-x-unit)))
              (?to-its-R-unit
               (args ((target ?objects) (context ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-R-unit))
               (mentioned-object nil)
               --
               (mentioned-object nil)
               (args ((target ?objects) (context ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-R-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn x-present-to-its-R-cxn
             ((?x-to-its-R-cxn
               (args ((target ?target) (source ?context)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit) 
                        (rightmost-unit ?rightmost-R-unit))
               (subunits (?x-unit ?present-unit ?to-its-R-unit)))
              <-
              (?x-to-its-r-cxn
               --
               (HASH form ((meets ?rightmost-x-unit ?present-unit) (meets ?present-unit ?leftmost-R-unit))))
              (?x-unit
               (args ((target ?target) (source ?objects) (original-source ?objects)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit)
                        (rightmost-unit ?rightmost-x-unit))
               --
               (sem-cat (grammar ?grammar))
               (args ((target ?target) (source ?objects) (original-source ?objects)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-x-unit)
                        (rightmost-unit ?rightmost-x-unit)))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              (?to-its-R-unit
               (args ((target ?objects) (context ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-R-unit))
               (mentioned-object nil)
               --
               (mentioned-object nil)
               (args ((target ?objects) (context ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-R-unit)
                        (rightmost-unit ?rightmost-R-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn does-x-have-things-to-its-R-cxn
             ((?does-x-have-things-to-its-R-unit
               (subunits (?does-unit ?x-unit ?have-unit ?things-unit ?to-unit ?its-unit ?r-unit)))
              <-
              (?does-x-have-things-to-its-R-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              (relate ?target ?unique ?segmented-scene ?scene ?relation)
                              (exist ?bool ?objects)))
               --
               (HASH form ((meets ?does-unit ?leftmost-unit)
                           (meets ?rightmost-unit ?have-unit)
                           (meets ?have-unit ?leftmost-thing-unit)
                           (meets ?rightmost-thing-unit ?to-unit)
                           (meets ?to-unit ?its-unit)
                           (meets ?its-unit ?leftmost-rel-unit))))
              (?does-unit
               --
               (HASH form ((string ?does-unit "does"))))
              (?x-unit
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (args ((target ?unique) (source ?segmented-scene)))
               --
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (args ((target ?unique) (source ?segmented-scene))))
              (?have-unit
               --
               (HASH form ((string ?have-unit "have"))))
              (?things-unit
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-thing-unit)
                        (rightmost-unit ?rightmost-thing-unit))
               (args ((target ?objects) (source ?target) (original-source ?target)))
               --
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-thing-unit)
                        (rightmost-unit ?rightmost-thing-unit))
               (args ((target ?objects) (source ?target) (original-source ?target))))
              (?to-unit
               --
               (HASH form ((string ?to-unit "to"))))
              (?its-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?r-unit
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



(def-fcg-cxn are-there-x-present-relation-cxn
             ((?are-there-x-present-relation-unit
               (subunits (?are-unit ?there-unit ?x-unit ?present-unit ?relation-unit))
               (args ((context ?segmented-scene)))
               (context -)
               (syn-cat (leftmost-unit ?are-unit)
                        (rightmost-unit ?rightmost-relation-unit)))
              <-
              (?are-there-x-present-relation-unit
               (HASH meaning ((exist ?bool ?target)
                              (relate ?np-source ?unique ?segmented-scene ?scene ?relation)))
               --
               (HASH form ((meets ?are-unit ?there-unit)
                           (meets ?there-unit ?leftmost-unit)
                           (meets ?rightmost-unit ?present-unit)
                           (meets ?present-unit ?leftmost-relation-unit))))
              (?are-unit
               --
               (HASH form ((string ?are-unit "are"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
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

             