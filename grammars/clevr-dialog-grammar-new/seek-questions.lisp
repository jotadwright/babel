(in-package :clevr-dialog-grammar)

;;; SENTENCE CXNS

(def-fcg-cxn what-attribute-is-x-cxn
             ((?what-attribute-is-x-unit
               (subunits (?what-unit ?attribute-unit ?is-unit ?x-unit))
               (sem-cat (sem-class query))
               (syn-cat (syn-class question)
                        (meaning ?meaning)
                        (leftmost-unit ?what-unit))
               (args ((unique ?source) (source ?context))))
              (?x-unit
               (footprints (poss)))
              <-
              (?what-attribute-is-x-unit
               (HASH meaning ((query ?query-target ?source ?scene ?attribute)))
               --
               (HASH form ((meets ?what-unit ?attribute-unit) (meets ?attribute-unit ?is-unit) (meets ?is-unit ?leftmost-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?x-unit
               (args ((target ?source) (source ?context) ;(original-target ?original-target) (original-source ?original-source)
                      ))
               ;(sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (meaning ?meaning))
               --
               (args ((target ?source) (source ?context) ;(original-target ?original-target) (original-source ?original-source)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (meaning ?meaning)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


;; SEEK ATTRIBUTE IMMEDIATE PAIR

(def-fcg-cxn question-attribute-no-object-cxn
             ((?question-attribute-unit
               (subunits (?question-marker-unit ?attribute-unit))
               (sem-cat (sem-class query))
               (syn-cat (syn-class question)
                        (meaning +))
               (args ((target ?unique) (source ?context))))
               <-
              (?question-attribute-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?object ?memory)
                              (find-in-context ?outset ?context ?object)
                              (unique ?unique ?outset)
                              ))
               --
               (HASH form ((meets ?about-unit ?attribute-unit))))
              (?question-marker-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (meaning +)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?about-unit)
                        (question-attribute +))
               --
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?about-unit)
                        (question-attribute +)
                        (question-object -)))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn question-object-no-attribute-cxn
             ((?question-object-no-attribute-unit
               (subunits (?question-marker-unit ?anaphoric-np-unit))
               (sem-cat (sem-class query))
               (syn-cat (syn-class question)
                        (meaning +))
               (args ((target ?unique) (source ?context)))
               )
               <-
              (?question-object-no-attribute-unit
               (HASH meaning ((get-last-attribute-category ?attribute ?memory)))
               --
               (HASH form ((meets ?about-unit ?leftmost-unit))))
              (?question-marker-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?about-unit)
                        (question-object +))
               --
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?about-unit)
                        (question-object +)
                        ;(question-attribute -)
                        ))
              (?anaphoric-np-unit
               (args ((target ?unique) (source ?context) ;(original-source ?memory)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit))
               --
               (args ((target ?unique) (source ?context) ;(original-source ?memory)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

;; SEEK ATTRIBUTE EARLIER

(def-fcg-cxn seek-question-cxn
             ((?seek-question-unit
               (subunits (?query-unit)))
              (?query-unit
               (footprints (remember-footprint)))
              <-
              (?seek-question-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)))
               --)
              (?query-unit
               (sem-cat (sem-class query))
               (syn-cat (meaning +))
               (args ((source ?segmented-scene)))
               (footprints (NOT remember-footprint))
               --
               (footprints (NOT remember-footprint))
               (sem-cat (sem-class query))
               (syn-cat (meaning +))
               (args ((source ?segmented-scene))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn query-attribute-object-cxn
             ((?query-attribute-object-unit
               (args ((source ?context) (unique ?unique)))
               (sem-cat (sem-class query))
               (syn-cat (syn-class question)
                        (meaning ?meaning)
                        (leftmost-unit ?what-unit))
               (subunits (?question-marker-unit ?x-unit)))
              <-
              (?query-attribute-object-unit
               --
               (HASH form ((meets ?is-unit ?possessor-unit))))
              (?question-marker-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?is-unit)
                        (question-attribute +)
                        (question-object +))
               --
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?is-unit)
                        (question-attribute +)
                        (question-object +)))
              (?x-unit
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?possessor-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               --
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?possessor-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))))
             :cxn-inventory *clevr-dialog*)
      
(def-fcg-cxn if-there-is-X-what-is-its-y-cxn
             ((?if-there-is-X-what-is-its-y-unit
               (subunits (?if-unit ?there-unit ?is-unit ?X-unit ?what-unit ?is2-unit ?its-unit ?y-unit)))
              <-
              (?if-there-is-X-what-is-its-y-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              (query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?if-unit ?there-unit) (meets ?there-unit ?is-unit) (meets ?is-unit ?leftmost-unit) (meets ?rightmost-unit ?what-unit) (meets ?what-unit ?is2-unit) (meets ?is2-unit ?its-unit) (meets ?its-unit ?y-unit))))
              (?if-unit
               --
               (HASH form ((string ?if-unit "if"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?X-unit
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat (sem-class immediate-relation))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit)
                        ;(definiteness indefinite)
                        )
               --
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat (sem-class immediate-relation))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit)
                        ;(definiteness indefinite)
                        ))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?is2-unit
               --
               (HASH form ((string ?is2-unit "is"))))
              (?its-unit
               --
               (HASH form ((string ?its-unit "its"))))
              (?y-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn if-there-is-X-what-y-is-it-cxn
             ((?if-there-is-X-what-y-is-it-unit
               (subunits (?if-unit ?there-unit ?is-unit ?X-unit ?what-unit ?y-unit ?is2-unit ?it-unit )))
              <-
              (?if-there-is-X-what-y-is-it-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              (query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?if-unit ?there-unit) (meets ?there-unit ?is-unit) (meets ?is-unit ?leftmost-unit) (meets ?rightmost-unit ?what-unit) (meets ?what-unit ?y-unit) (meets ?y-unit ?is2-unit) (meets ?is2-unit ?it-unit))))
              (?if-unit
               --
               (HASH form ((string ?if-unit "if"))))
              (?there-unit
               --
               (HASH form ((string ?there-unit "there"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?X-unit
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat (sem-class immediate-relation))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit)
                        ;(definiteness indefinite)
                        )
               --
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat (sem-class immediate-relation))
               (syn-cat (number singular)
                        (rightmost-unit ?rightmost-unit)
                        (leftmost-unit ?leftmost-unit)
                        ;(definiteness indefinite)
                        ))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?y-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)))
              (?is2-unit
               --
               (HASH form ((string ?is2-unit "is"))))
              (?it-unit
               --
               (HASH form ((string ?its-unit "it"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)
              


(def-fcg-cxn query-number-x-cxn
             ((?query-number-x-unit
               (subunits (?query-number-unit ?x-unit))
               (args ((context ?segmented-scene)))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class query))
               (footprints (context)))
               <-
              (?query-number-x-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit))))
              (?query-number-unit
               (args ((source ?target)))
               (sem-cat (sem-class query-number))
               (syn-cat (syn-class question-marker)
                        (number singular)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?target)))
               (sem-cat (sem-class query-number))
               (syn-cat (syn-class question-marker)
                        (number singular)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?target) (source ?segmented-scene)))
               (sem-cat (grammar mnist)
                        (relation t))
               (syn-cat (syn-class np)
                        (number singular)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?segmented-scene)))
               (sem-cat (grammar mnist)
                        (relation t))
               (syn-cat (syn-class np)
                        (number singular)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn query-number-digit-cxn
             ((?query-number-digit-unit
               (subunits (?query-number-unit ?x-unit))
               (args ((context ?segmented-scene)))
               (syn-cat (syn-class question)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class query))
               (footprints (context)))
               <-
              (?query-number-digit-unit
               (HASH meaning (;(get-context ?context)
                              (segment-scene ?segmented-scene ?scene)
                              ))
               --
               (HASH form ((meets ?many-unit ?leftmost-unit))))
              (?query-number-unit
               (args ((source ?unique)))
               (sem-cat (sem-class query-number))
               (syn-cat (syn-class question-marker)
                        (number singular)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit))
               --
               (args ((source ?unique)))
               (sem-cat (sem-class query-number))
               (syn-cat (syn-class question-marker)
                        (number singular)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?many-unit)))
              (?x-unit
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat 
                        (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?unique) (source ?segmented-scene)))
               (sem-cat 
                        (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)