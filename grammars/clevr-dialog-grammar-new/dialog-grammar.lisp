(in-package :clevr-dialog-grammar)

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-valid-children)))
  "Checks whether there are no more applicable constructions when a node is
fully expanded and no constructions could apply to its children
nodes."
  (and (or (not (children node))
	   (loop for child in (children node)
                 never (and (cxn-applied child)
                            (not (find 'duplicate (statuses child))))))
       (fully-expanded? node)))

(def-fcg-constructions dialog-grammar-constructicon
  :feature-types ((args set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (footprints set))
  :fcg-configurations ((:construction-inventory-processor-mode . :heuristic-search)
                       (:node-expansion-mode . :full-expansion)
                       (:cxn-supplier-mode . :hashed)
                       (:search-algorithm . :best-first)
                       (:hash-mode . :hash-string-meaning-lex-id)
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched :cxn-score)
                       (:heuristic-value-mode . :sum-heuristics-and-parent)
                       
                       (:parse-goal-tests :no-applicable-cxns
                                          :no-strings-in-root
                                          :connected-semantic-network
                                          :connected-structure
                                          )
                       (:production-goal-tests :no-applicable-cxns
                                               :no-meaning-in-root :connected-structure)
                       (:max-nr-of-nodes . 20000)
                       (:de-render-mode . :de-render-scene-and-memory))
  :visualization-configurations ((:with-search-debug-data . t)
                                 (:hide-features . nil)
                                 (:show-constructional-dependencies . nil))
  :hashed t
  :cxn-inventory *clevr-dialog*)


(setf *fcg-constructions* *clevr-dialog*)

(def-fcg-cxn itself-cxn
             ((?itself-unit
               (args ((target ?unique) (source ?context) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?itself-unit)
                        (rightmost-unit ?itself-unit)
                        (meaning +)))
              <-
              (?itself-unit
               (HASH meaning ((find-in-context ?object-set ?context ?object)
                              (unique ?unique ?object-set)
                              (get-last-topic ?object ?memory)
                              ;(get-memory ?history)
                              ))
               --
               (HASH form ((string ?itself-unit "itself"))))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn its-no-meaning-cxn
             ( <-
              (?its-unit
               (args ((target ?unique) (source ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class pronoun)
                        (meaning -))
               --
               (HASH form ((string ?its-unit "its"))))))|#

(def-fcg-cxn it-cxn
             ((?it-unit
               (args ((target ?unique) (source ?context) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric)
                        (grammar clevr))
               (syn-cat (syn-class np)
                        (number singular)
                        (leftmost-unit ?it-unit)
                        (rightmost-unit ?it-unit)
                        (meaning +)))
              <-
              (?it-unit
               (HASH meaning ((find-in-context ?object-set ?context ?object)
                              (unique ?unique ?object-set)
                              (get-last-topic ?object ?memory)
                              ;(get-memory ?history)
                              ))
               --
               (HASH form ((string ?it-unit "it"))))
              (memory-unit
               --
               (memory ?memory)))
             :score 0.10
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn it-no-meaning-cxn
             (
              <-
              (?it-unit
               (args ((target ?unique) (source ?context) (original-target ?original-target) (original-source ?original-source)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?it-unit)
                        (rightmost-unit ?it-unit)
                        (meaning -))
               --
               (HASH form ((string ?it-unit "it"))))))|#


(def-fcg-cxn this-thing-cxn
             ((?this-thing-unit
               (args ((target ?unique) (source ?context)))
               (sem-cat (sem-class demonstrative)
                        (grammar clevr))
               (syn-cat (syn-class np)
                          (leftmost-unit ?this-unit)
                          (rightmost-unit ?thing-unit)
                          (meaning +))
               (subunits (?this-unit ?thing-unit)))
              (?thing-unit
               (footprints (object)))
              <-
              (?this-thing-unit
               (HASH meaning ((find-in-context ?object-set ?context ?target)
                              (unique ?unique ?object-set)
                              (get-last-topic ?object ?memory)
                              ;(get-memory ?history)
                              ))
               --
               (HASH form ((meets ?this-unit ?leftmost-unit))))
              (?this-unit
               --
               (HASH form ((string ?this-unit "this"))))
              (?thing-unit
               (args ((target ?target)
                      (source ?object)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?starts)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?starts)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


#|(def-fcg-cxn possessive-its-cxn
             ((?possessive-its-unit
               (args ((target ?unique) (source ?context) (object ?object) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?possessor-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (subunits (?possessor-unit ?possessed-unit))
               (footprints (poss)))
              <-
              (?possessive-its-unit
               --
               (HASH form ((meets ?possessor-unit ?possessed-unit))))
              (?possessor-unit
               (args ((target ?unique) (object ?object) (source ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class pronoun)
                        (meaning ?meaning))
               --
               (args ((target ?unique) (object ?object) (source ?context)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class pronoun)
                        (meaning ?meaning)))
              (?possessed-unit
               
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit)))))|#


(def-fcg-cxn possessive-its-cxn
             ((?possessive-its-unit
               (args ((target ?unique) (source ?context) (object ?object) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?possessor-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (subunits (?possessor-unit ?possessed-unit))
               (footprints (poss)))
              <-
              (?possessive-its-unit
               (HASH meaning ((find-in-context ?object-set ?context ?object)
                              (unique ?unique ?object-set)
                              (get-last-topic ?object ?memory)
                              ;(get-memory ?history)
                              ))
               --
               (HASH form ((string ?its-unit "its")
                           (meets ?possessor-unit ?possessed-unit))))
              (?possessed-unit
               
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
              (memory-unit
               --
               (memory ?memory)))
             :score 0.10
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn its-cxn
             ((?its-unit
               (args ((target ?unique) (source ?context) (object ?object)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class pronoun)
                        (meaning +)))
              <-
              (?its-unit
               (HASH meaning ((find-in-context ?object-set ?context ?object) (unique ?unique ?object-set) (get-last-topic ?object ?history) (get-memory ?history)))
               --
               (HASH form ((string ?its-unit "its"))))))|#

(def-fcg-cxn possessive-of-cxn
             ((?possessive-of-unit
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (subunits (?possessor-unit ?possessed-unit ?the-unit ?of-unit)))
              <-
              (?possessive-of-unit
               --
               (HASH form ((meets ?the-unit ?leftmost-poss-unit) (meets ?rightmost-poss-unit ?of-unit) (meets ?of-unit ?leftmost-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?possessed-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
                        
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
                        
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?possessor-unit
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar clevr))
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT poss))
               --
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar clevr))
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn possessive-of-mnist-cxn
             ((?possessive-of-unit
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (subunits (?possessor-unit ?possessed-unit ?the-unit ?of-unit)))
              <-
              (?possessive-of-unit
               --
               (HASH form ((meets ?the-unit ?leftmost-poss-unit) (meets ?rightmost-poss-unit ?of-unit) (meets ?of-unit ?leftmost-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?possessed-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        )
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        )
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?possessor-unit
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar mnist)
                        (relation t)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT poss))
               --
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar mnist)
                        (relation t)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               
               ))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn possessive-of-mnist-2-cxn
             ((?possessive-of-unit
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning ?meaning))
               (subunits (?possessor-unit ?possessed-unit ?the-unit ?of-unit)))
              <-
              (?possessive-of-unit
               --
               (HASH form ((meets ?the-unit ?leftmost-poss-unit) (meets ?rightmost-poss-unit ?of-unit) (meets ?of-unit ?leftmost-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?possessed-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        )
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute)
                        )
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-poss-unit)))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?possessor-unit
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar mnist)
                        (sem-class det-anaphoric)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT poss))
               --
               (args ((target ?unique) (source ?context)))
               (sem-cat (grammar mnist)
                        (sem-class det-anaphoric)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               
               ))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn possessive-of-demonstrative-cxn
             ((?possessive-of-unit
               (args ((target ?unique) (source ?context) (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?possessor-unit ?possessed-unit ?the-unit ?this-unit ?thing-unit ?of-unit)))
              <-
              (?possessive-of-unit
               (HASH meaning ((find-in-context ?object-set ?context ?object) (unique ?unique ?object-set)))
               --
               (HASH form ((meets ?the-unit ?possessed-unit) (meets ?possessed-unit ?of-unit) (meets ?of-unit ?this-unit) (meets ?this-unit ?thing-unit))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?possessed-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (?possessor-unit
               (args ((target ?object)))
               (sem-cat (sem-class demonstrative))
               (syn-class (syn-cat np)
                          (leftmost-unit ?leftmost-unit)
                          (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?object)))
               (sem-cat (sem-class demonstrative))
               (syn-class (syn-cat np)
                          (leftmost-unit ?leftmost-unit)
                          (rightmost-unit ?rightmost-unit)))))|#


;; question markers

(def-fcg-cxn what-is-cxn
             ((?what-is-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?is-unit)
                        (question-object +)
                        (question-attribute +))
               (subunits (?what-unit ?is-unit))
               (footprints (question-marker)))
              <-
              (?what-is-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               
               --
               (HASH form ((string ?what-unit "what")
                           (string ?is-unit "is")
                           (meets ?what-unit ?is-unit))))
              (scene-unit
               --
               (scene ?scene)))
             :score 0.10
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn what-about-cxn
             ((?what-about-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?about-unit)
                        (question-object ?object)
                        (question-attribute +))
               (subunits (?what-unit ?about-unit))
               (footprints (question-marker)))
              <-
              (?what-about-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               
               --
               (HASH form ((meets ?what-unit ?about-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?about-unit
               --
               (HASH form ((string ?about-unit "about"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn and-cxn
             ((?and-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?and-unit)
                        (rightmost-unit ?and-unit)
                        (question-attribute +)
                        (question-object ?object))
               (footprints (question-marker))
               )
              <-
              (?and-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((string ?and-unit "and"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn and-that-of-cxn
             ((?and-that-of-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?and-unit)
                        (rightmost-unit ?of-unit)
                        (question-object +)
                        (question-attribute -))
               (subunits (?and-unit ?that-unit ?of-unit))
               (footprints (question-marker)))
              <-
              (?and-that-of-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((string ?and-unit "and") (string ?that-unit "that") (string ?of-unit "of"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)
               

(def-fcg-cxn how-about-cxn
             ((?how-about-unit
               (args ((target ?specific-attribute) (source ?unique) (attribute ?attribute)))
               (sem-cat (sem-class question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?about-unit)
                        (question-object ?object)
                        (question-attribute +))
               (subunits (?how-unit ?about-unit))
               (footprints (question-marker)))
              <-
              (?how-about-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               
               --
               (HASH form ((meets ?how-unit ?about-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?about-unit
               --
               (HASH form ((string ?about-unit "about"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn how-about-exist-or-count-cxn
             ((?how-about-unit
               (args ((source ?objects)))
               (sem-cat (sem-class exist-question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?about-unit))
               (subunits (?how-unit ?about-unit))
               (footprints (question-marker)))
              <-
              (?how-about-unit
               (HASH meaning ((exist-or-count ?bool ?objects)))
               --
               (HASH form ((meets ?how-unit ?about-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?about-unit
               --
               (HASH form ((string ?about-unit "about")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn what-about-exist-cxn
             ((?what-about-unit
               (args ((source ?objects)))
               (sem-cat (sem-class exist-question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?about-unit))
               (subunits (?what-unit ?about-unit))
               (footprints (question-marker)))
              <-
              (?what-about-unit
               (HASH meaning ((exist-or-count ?bool ?objects)))
               --
               (HASH form ((meets ?what-unit ?about-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?about-unit
               --
               (HASH form ((string ?about-unit "about")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn and-exist-cxn
             ((?and-unit
               (args ((source ?objects)))
               (sem-cat (sem-class exist-question))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?and-unit)
                        (rightmost-unit ?and-unit))
               (footprints (question-marker)))
              <-
              (?and-unit
               (HASH meaning ((exist-or-count ?bool ?objects)))
               --
               (HASH form ((string ?and-unit "and")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn what-number-of-cxn
             ((?what-number-of-unit
               (args ((source ?source)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?what-unit ?number-unit ?of-unit))
               (footprints (question-marker)))
              <-
              (?what-number-of-unit
               (HASH meaning ((count-objects ?count ?source)))
               --
               (HASH form ((meets ?what-unit ?number-unit)
                           (meets ?number-unit ?of-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?number-unit
               --
               (HASH form ((string ?number-unit "number"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn what-is-the-number-of-cxn
             ((?what-is-the-number-of-unit
               (args ((source ?source)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (number plural)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?what-unit ?is-unit ?the-unit ?number-unit ?of-unit))
               (footprints (question-marker)))
              <-
              (?what-is-the-number-of-unit
               (HASH meaning ((count-objects ?count ?source)))
               --
               (HASH form ((meets ?what-unit ?is-unit)
                           (meets ?is-unit ?the-unit)
                           (meets ?number-unit ?of-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?number-unit
               --
               (HASH form ((string ?number-unit "number"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn what-is-the-number-of-mnist-cxn
             ((?what-is-the-number-of-unit
               (args ((source ?source)))
               (sem-cat (sem-class query-number))
               (syn-cat (syn-class question-marker)
                        (number singular)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?what-unit ?is-unit ?the-unit ?number-unit ?of-unit))
               (footprints (question-marker)))
              <-
              (?what-is-the-number-of-unit
               (HASH meaning ((bind attribute-category ?attribute digit)
                              (query ?digit ?source ?scene ?attribute)))
               --
               (HASH form ((meets ?what-unit ?is-unit)
                           (meets ?is-unit ?the-unit)
                           (meets ?number-unit ?of-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?number-unit
               --
               (HASH form ((string ?number-unit "number"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of"))))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn what-is-the-count-of-cxn
             ((?what-is-the-count-of-unit
               (args ((source ?source)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (number plural)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?of-unit))
               (subunits (?what-unit ?is-unit ?the-unit ?count-unit ?of-unit))
               (footprints (question-marker)))
              <-
              (?what-is-the-count-of-unit
               (HASH meaning ((count-objects ?count ?source)))
               --
               (HASH form ((meets ?what-unit ?is-unit)
                           (meets ?is-unit ?the-unit)
                           (meets ?count-unit ?of-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?is-unit
               --
               (HASH form ((string ?is-unit "is"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?count-unit
               --
               (HASH form ((string ?count-unit "count"))))
              (?of-unit
               --
               (HASH form ((string ?of-unit "of")))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn how-many-cxn
             ((?how-many-unit
               (args ((source ?source)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?how-unit)
                        (rightmost-unit ?such-unit))
               (subunits (?how-unit ?many-unit))
               (footprints (question-marker)))
              <-
              (?how-many-unit
               (HASH meaning ((count-objects ?count ?source)))
               --
               (HASH form ((meets ?how-unit ?many-unit))))
              (?how-unit
               --
               (HASH form ((string ?how-unit "how"))))
              (?many-unit
               --
               (HASH form ((string ?many-unit "many")))))
             :cxn-inventory *clevr-dialog*)




#|(def-fcg-cxn what-number-cxn
             ((?what-number-unit
               (args ((source ?source)))
               (sem-cat (sem-class count))
               (syn-cat (syn-class question-marker)
                        (leftmost-unit ?what-unit)
                        (rightmost-unit ?number-unit))
               (subunits (?what-unit ?number-unit))
               (footprints (question-marker)))
              <-
              (?what-number-unit
               (HASH meaning ((count-objects ?count ?source)))
               --
               (HASH form ((meets ?what-unit ?number-unit))))
              (?what-unit
               --
               (HASH form ((string ?what-unit "what"))))
              (?number-unit
               --
               (HASH form ((string ?number-unit "number"))))))|#




;;articles



;;; NOMINAL CXNS

(def-fcg-cxn nom-cxn
             ((?nom-unit
               (args ((target ?target)
                      (source ?source)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?starts)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?noun-unit)))
               (?noun-unit 
                (footprints (nom)))
              <-
              (?nom-unit
               (HASH meaning ((filter-by-attribute ?target ?source ?scene ?object)))
               --)
              (?noun-unit
               (args ((target ?object)))
               ;(sem-cat (sem-class thing))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT nom))
               --
               (args ((target ?object)))
               ;(sem-cat (sem-class thing))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class noun)
                        (number ?number)
                        (starts-with ?first-letter)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT nom)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*
             :disable-automatic-footprints t)

(def-fcg-cxn adj-nom-cxn
             ((?adj-nom-unit
               (args ((target ?target)
                      (source ?source)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (adjective +)
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?adjective-unit ?nom-unit)))
              (?nom-unit
               (footprints (adj-nom adj-nom-cxn)))
              <-
              (?adj-nom-unit
               (HASH meaning ((filter-by-attribute ?target ?between ?scene ?attribute)))
               --
               (HASH form ((meets ?adjective-unit ?leftmost-unit))))
              (?adjective-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?first))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?first)))
              (?nom-unit
               (args ((target ?between)
                      (source ?source)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT adj-nom-cxn))
               --
               (args ((target ?between)
                      (source ?source)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT adj-nom-cxn)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*
             :disable-automatic-footprints t)




(def-fcg-cxn anaphoric-nom-cxn
             ((?anaphoric-nom-unit
               (args ((target ?target)
                      (source ?source)
                      (original-source ?memory)
                      (original-target ?object-set)
                      ))
               (sem-cat (sem-class anaphoric)
                        (grammar ?grammar))
               (syn-cat (syn-function nom)
                        (number ?number)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-nom-unit))
               (subunits (?anaphoric-unit ?nom-unit)))
              <-
              (?anaphoric-nom-unit
               (HASH meaning ((find-in-context ?target ?source ?object-set)))
               --
               (HASH form ((meets ?anaphoric-unit ?leftmost-nom-unit))))
              (?anaphoric-unit
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective))
               --
               (args ((target ?memory)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (syn-class adjective)))
              (?nom-unit
               (args ((target ?object-set)
                      (source ?memory)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-nom-unit)
                        (rightmost-unit ?rightmost-nom-unit))
               --
               (args ((target ?object-set)
                      (source ?memory)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-nom-unit)
                        (rightmost-unit ?rightmost-nom-unit)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

;;; NP CXNS



(def-fcg-cxn the-np-cxn
             ((?np-unit
               (args ((target ?unique)
                      (source ?source)
                      ;(original-source ?source)
                      ))
               (subunits (?det-unit ?nom-unit))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?det-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               --
               (HASH form ((meets ?det-unit ?leftmost-unit))))
              (?det-unit
               (HASH meaning ((unique ?unique ?target)))
               --
               (HASH form ((string ?det-unit "the"))))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn a-np-cxn
             ((?np-unit
               (args ((target ?unique) (source ?source)
                      ;(original-source ?target)
                      ))
               (subunits (?det-unit ?nom-unit))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?det-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               --
               (HASH form ((meets ?det-unit ?leftmost-unit))))
              (?det-unit
               (HASH meaning ((select-one ?unique ?target)))
               --
               (HASH form ((string ?det-unit "a"))))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn an-np-cxn
             ((?np-unit
               (args ((target ?unique) (source ?source)
                      ;(original-source ?original-source)
                      ))
               (subunits (?det-unit ?nom-unit))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?det-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               --
               (HASH form ((meets ?det-unit ?leftmost-unit))))
              (?det-unit
               (HASH meaning ((select-one ?unique ?target)))
               --
               (HASH form ((string ?det-unit "an"))))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn one-np-cxn
             ((?np-unit
               (args ((target ?unique) (source ?source)
                      ;(original-source ?source)
                      ))
               (subunits (?one-unit ?nom-unit))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?leftmost-one-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               (HASH meaning ((unique ?unique ?target)))
               --
               (HASH form ((meets ?rightmost-one-unit ?leftmost-unit))))
              (?one-unit
               --
               (HASH form ((string ?one-unit "one"))))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn exactly-one-np-cxn
             ((?np-unit
               (args ((target ?unique)
                      (source ?source)
                      ;(original-source ?source)
                      ))
               (subunits (?exactly-unit ?one-unit ?nom-unit))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?leftmost-one-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               (HASH meaning ((unique ?unique ?target)))
               --
               (HASH form ((meets ?rightmost-one-unit ?leftmost-unit))))
              (?exactly-unit
               --
               (HASH form ((string ?exactly-unit "exactly"))))
              (?one-unit
               --
               (HASH form ((string ?one-unit "one"))))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)

;;;wanneer worden deze gebruikt?????????
#|(def-fcg-cxn anaphoric-an-np-cxn
             ((?anaphoric-np-unit
               (args ((target ?onetarget)
                      (source ?context)
                      (original-target ?target)
                      (original-source ?history)
                      (input ?object-set)
                      ))
               (sem-cat (sem-class det-anaphoric)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?det-unit ?anaphoric-nom-unit))) 
              <-
              (?anaphoric-np-unit
               ;(HASH meaning ((get-context ?context) ))
               --
               (HASH form ((meets ?det-unit ?anaphoric-unit))))
              (?det-unit
               (HASH meaning ((unique ?onetarget ?target)))
               --
               (HASH form ((string ?det-unit "an"))))
              (?anaphoric-nom-unit
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric)
                        )
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)|#

#|(def-fcg-cxn anaphoric-a-np-cxn
             ((?anaphoric-np-unit
               (args ((target ?onetarget) (source ?context) (original-target ?target) (original-source ?history) (input ?object-set)))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?det-unit ?anaphoric-nom-unit))) 
              <-
              (?anaphoric-np-unit
               ;(HASH meaning ((get-context ?context) ))
               --
               (HASH form ((meets ?det-unit ?anaphoric-unit))))
              (?det-unit
               (HASH meaning ((unique ?onetarget ?target)))
               --
               (HASH form ((string ?det-unit "a"))))
              (?anaphoric-nom-unit
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric))
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)|#


(def-fcg-cxn the-digit-cxn
             ((?anaphoric-digit-unit
               (args ((target ?unique)
                      (source ?context)
                      (original-target ?target)
                      (original-source ?history)
                      (input ?object-set)))
               (sem-cat (sem-class det-anaphoric)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?determiner-unit ?digit-unit )))
              (?digit-unit
               (footprints (nom)))
              <-
              (?anaphoric-digit-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?target ?memory)
                              (find-in-context ?target-2 ?context ?target)
                              (unique ?unique ?target-2)))
               --
               (HASH form ((meets ?determiner-unit ?digit-unit))))
              (?determiner-unit
               --
               (HASH form ((string ?determiner-unit "the"))))
              (?digit-unit
               --
               (HASH form ((string ?digit-unit "digit"))))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

#|(def-fcg-cxn the-anaphoric-digit-adj-np-cxn
             ((?np-unit
               (args ((target ?unique) (source ?context) (original-source ?context) ))
               (subunits (?det-unit ?adjective-unit ?nom-unit))
               (sem-cat (sem-class det-anaphoric)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?det-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               (HASH meaning ((get-memory ?source)
                              
                              (filter-by-attribute ?second-target ?target ?attribute)
                              (find-in-context ?out ?context ?second-target)))
               --
               (HASH form ((meets ?det-unit ?adjective-unit)
                           (meets ?adjective-unit ?leftmost-unit))))
              (?det-unit
               (HASH meaning ((unique ?unique ?out)))
               --
               (HASH form ((string ?det-unit "the"))))
              (?adjective-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?first))
               --
               (args ((target ?attribute)))
               (sem-cat (sem-class property))
               (syn-cat (syn-class adjective)
                        (starts-with ?first)))
              (?nom-unit
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar mnist))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)
                      ))
               (sem-cat (grammar mnist))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))))
             :cxn-inventory *clevr-dialog*)|#


(def-fcg-cxn the-anaphoric-digit-adj-np-cxn
             ((?np-unit
               (args ((target ?unique) (source ?context) (original-source ?context) ))
               (subunits (?det-unit  ?nom-unit))
               (sem-cat (sem-class det-anaphoric)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?det-unit)
                        (rightmost-unit ?noun-unit)
                        (definiteness ?def)))
              <-
              (?np-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?source ?memory)
                              (find-in-context ?out ?context ?target)))
               --
               (HASH form ((meets ?det-unit  ?leftmost-unit))))
              (?det-unit
               (HASH meaning ((unique ?unique ?out)))
               --
               (HASH form ((string ?det-unit "the"))))
              
              (?nom-unit
               (args ((target ?target)
                      (source ?source)))
               (adjective +)
               (sem-cat (grammar mnist))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit))
               --
               (args ((target ?target)
                      (source ?source)))
               (adjective +)
               (sem-cat (grammar mnist))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?first)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?noun-unit)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn anaphoric-digit-cxn
             ((?anaphoric-digit-unit
               (args ((target ?unique) (source ?context) (original-target ?target) (original-source ?history) (input ?object-set)))
               (sem-cat (sem-class det-anaphoric)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?determiner-unit ?digit-unit )))
              (?digit-unit
               (footprints (nom)))
              <-
              (?anaphoric-digit-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?topic ?memory)
                              (filter-by-attribute ?target ?topic ?scene ?digit)
                              (find-in-context ?target-2 ?context ?target)
                              (unique ?unique ?target-2)))
               --
               (HASH form ((meets ?determiner-unit ?digit-unit))))
              (?determiner-unit
               --
               (HASH form ((string ?determiner-unit "the"))))
              (?digit-unit
               (sem-cat (sem-class digit)
                        (grammar mnist))
               (syn-cat (syn-class noun))
               (args ((source ?digit)))
               --
               (sem-cat (sem-class digit)
                        (grammar mnist))
               (syn-cat (syn-class noun))
               (args ((source ?digit))))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn anaphoric-the-np-cxn
             ((?anaphoric-np-unit
               (args ((target ?onetarget) (source ?context) (original-target ?target) (original-source ?history) (input ?object-set)))
               (sem-cat (sem-class det-anaphoric)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?det-unit ?anaphoric-nom-unit))) 
              <-
              (?anaphoric-np-unit
               ;(HASH meaning ((get-context ?context) ))
               --
               (HASH form ((meets ?det-unit ?anaphoric-unit))))
              (?det-unit
               (HASH meaning ((unique ?onetarget ?target)))
               --
               (HASH form ((string ?det-unit "the"))))
              (?anaphoric-nom-unit
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric)
                        (grammar ?grammar))
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?target) (source ?context) (original-target ?object-set) (original-source ?history)))
               (sem-cat (sem-class anaphoric)
                        (grammar ?grammar))
               (syn-cat (number singular)
                        (leftmost-unit ?anaphoric-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn anaphoric-np-that-cxn
             ((?anaphoric-np-unit
               (args ((target ?onetarget)
                      (source ?source)
                      ;(original-target ?target)
                      ;(original-source ?memory)
                      (input ?object-set)
                      ))
               (sem-cat (sem-class det-anaphoric)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?the-unit)
                        (rightmost-unit ?rightmost-unit)
                        (meaning +))
               (subunits (?that-unit ?anaphoric-nom-unit))) 
              <-
              (?anaphoric-np-unit
               (HASH meaning ((unique ?onetarget ?target)
                              (find-in-context ?target ?source ?object-set)))
               --
               (HASH form ((meets ?that-unit ?leftmost-nom-unit))))
              (?that-unit                  
               --
               (HASH form ((string ?that-unit "that"))))
              (?anaphoric-nom-unit
               (args ((target ?object-set)
                      (source ?memory)))
               (sem-cat ;(sem-class nominal)
                        (grammar ?grammar))
               (syn-cat ;(syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-nom-unit)
                        (rightmost-unit ?rightmost-nom-unit))
               --
               (args ((target ?object-set)
                      (source ?memory)))
               (sem-cat ;(sem-class nominal)
                        (grammar ?grammar))
               (syn-cat ;(syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-nom-unit)
                        (rightmost-unit ?rightmost-nom-unit)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)



;; different plural np's

(def-fcg-cxn plural-np-cxn 
             ((?plural-np-unit
               (args ((target ?inset) (source ?context) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit)
                        (number plural))
               (subunits (?nom-unit))
               (footprints (plural)))
              (?nom-unit
               (footprints (plural)))
              <-
              (?plural-np-unit
               --)
              (?nom-unit
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT  plural adj-nom exist-question))
               --
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT  plural adj-nom exist-question))))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn all-the-x-cxn 
             ((?plural-np-unit
               (args ((target ?inset) (source ?context) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?all-unit)
                        (rightmost-unit ?rightmost-unit)
                        (number plural))
               (subunits (?nom-unit ?all-unit ?the-unit)))
              (?nom-unit
               (footprints (plural)))
              <-
              (?plural-np-unit
               --
               (HASH form ((meets ?all-unit ?the-unit) (meets ?the-unit ?nominal-unit))))
              (?all-unit
               --
               (HASH form ((string ?all-unit "all"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?nom-unit
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT plural adj-nom))
               --
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT plural adj-nom))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn other-plural-np-cxn 
             ((?other-plural-np-unit
               (args ((target ?target) (source ?context) (input ?original-source) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (number plural)
                        
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?other-unit ?nom-unit))
               (footprints (other))
               )
              (?nom-unit
               (footprints (plural)))
              <-
              (?other-plural-np-unit
                --
               (HASH form ((meets ?adjective-unit ?nominal-unit))))
              (?other-unit
               (args ((target ?target) (source ?inset) (input ?original-source)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?adjective-unit))
               --
               (args ((target ?target) (source ?inset) (input ?original-source)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?adjective-unit)))
              (?nom-unit
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number plural)
                        
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT plural))
               --
               (footprints (NOT plural))
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn other-plural-np-mnist-cxn 
             ((?other-plural-np-unit
               (args ((target ?target) (source ?context) (input ?original-source) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (number plural)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?other-unit ?nom-unit))
               )
              (?nom-unit
               (footprints (plural)))
              <-
              (?other-plural-np-unit
                --
               (HASH form ((meets ?adjective-unit ?nominal-unit))))
              (?other-unit
               (args ((target ?target) (source ?inset) (input ?original-source)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?adjective-unit))
               --
               (args ((target ?target) (source ?inset) (input ?original-source)))
               (syn-cat (syn-class adjective)
                        (leftmost-unit ?adjective-unit)))
              (?nom-unit
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT plural))
               --
               (footprints (NOT plural))
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (syn-class np)
                                          
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn history-other-things-mnist-cxn
             ((?history-other-things-unit
               (args ((target ?target) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (history)))
              <-
              (?history-other-things-unit
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (HASH meaning ((get-last-topic ?original-source ?memory)
                              ;(get-memory ?history)
                              ))
               (footprints (NOT how-many relative history))
               --
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar mnist))
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT how-many relative history)))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn history-other-things-cxn
             ((?history-other-things-unit
               (args ((target ?target) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (history)))
              <-
              (?history-other-things-unit
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar clevr)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (HASH meaning ((filter-by-attribute ?original-source ?memory ?scene ?things)
                              (bind shape-category ?things thing)
                              ;(get-memory ?history)
                              ))
               (footprints (NOT how-many relative history))
               --
               (args ((target ?target) (source ?inset) (input ?original-source) (original-source ?context)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar clevr)
                        )
               (syn-cat (syn-class np)
                        (leftmost-unit ?adjective-unit)
                        (rightmost-unit ?rightmost-unit))
               (footprints (NOT how-many relative history)))
              (scene-unit
               --
               (scene ?scene))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)



(def-fcg-cxn multiple-plural-np
             ((?multiple-plural-np-unit
               (args ((target ?bool)
                      (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-unit)) 
               (subunits (?multiple-unit ?nom-unit)))
              (?nom-unit
               (footprints (plural)))
              <- 
              (?multiple-plural-np-unit
               --
               (HASH form ((meets ?rightmost-multiple-unit ?nominal-unit)) )
               )
             (?multiple-unit
               (args ((target ?bool) (source ?inset))) 
               (sem-cat (sem-class multiple)) 
               (syn-cat (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-multiple-unit)) 
               --
               (args ((target ?bool) (source ?inset))) 
               (sem-cat (sem-class multiple)) 
               (syn-cat (leftmost-unit ?leftmost-multiple-unit) 
                        (rightmost-unit ?rightmost-multiple-unit)))
              (?nom-unit
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (footprints (NOT plural))
               (args ((target ?inset) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn number-plural-np
             ((?number-plural-np-unit
               (args ((target ?bool) (source ?context)
                      ;(original-source ?context)
                      ))
               (sem-cat (sem-class multiple-plural-np)) 
               (syn-cat (syn-class np) 
                        (leftmost-unit ?number-unit) 
                        (rightmost-unit ?rightmost-unit)) 
               (subunits (?number-unit ?nom-unit)))
              (?nom-unit
               (footprints (plural)))
              <- 
              (?number-plural-np-unit
               --
               (HASH form ((meets ?number-unit ?nominal-unit))))
             (?number-unit
               (args ((source ?source)))
               (sem-cat (sem-class number))
               (syn-cat (syn-class adjective))
               --
               (args ((source ?source)))
               (sem-cat (sem-class number))
               (syn-cat (syn-class adjective)))
              (?nom-unit
               (args ((target ?source) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (footprints (NOT plural))
               (args ((target ?source) (source ?context)))
               (sem-cat (sem-class nominal))
               (syn-cat (syn-function nominal)
                        (number plural)
                        (starts-with ?first-letter)
                        (leftmost-unit ?nominal-unit)
                        (rightmost-unit ?rightmost-unit))))
             :cxn-inventory *clevr-dialog*)

; relative clause

(def-fcg-cxn other-things-sharing-x-with-y-cxn
             ((?nom-that-x-unit
               (args ((original-source ?context) (source ?context) (target ?out)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-cat relative)
                        (grammar ?grammar))
               (subunits (?other-things-unit ?sharing-unit)))
              (?other-things-unit
               (footprints (relative)))
              <-
              (?nom-that-x-unit
               (HASH meaning ((filter-by-attribute ?out ?target ?scene ?specific-attribute )))
               --
               (HASH form ((meets ?rightmost-objects-unit ?leftmost-sharing-unit))))
              (?other-things-unit
               (args ((target ?target) (source ?context) (input ?original-source) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history))
               --
               (args ((target ?target) (source ?context) (input ?original-source) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history)))
              (?sharing-unit
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               --
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn other-things-in-the-image-sharing-x-with-y-cxn
             ((?nom-that-x-unit
               (args ((original-source ?context) (source ?context) (target ?out)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-cat relative)
                        (grammar ?grammar))
               (subunits (?other-things-unit ?in-unit ?the-unit ?image-unit ?sharing-unit)))
              (?other-things-unit
               (footprints (relative)))
              <-
              (?nom-that-x-unit
               (HASH meaning ((filter-by-attribute ?out ?target ?scene ?specific-attribute)))
               --
               (HASH form ((meets ?rightmost-objects-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit) (meets ?image-unit ?leftmost-sharing-unit))))
              (?other-things-unit
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history))
               --
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
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
              (?sharing-unit
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               --
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn other-things-present-in-the-image-sharing-x-with-y-cxn
             ((?nom-that-x-unit
               (args ((original-source ?context) (source ?context) (target ?out)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-cat relative)
                        (grammar ?grammar))
               (subunits (?other-things-unit ?present-unit ?in-unit ?the-unit ?image-unit ?sharing-unit)))
              (?other-things-unit
               (footprints (relative)))
              <-
              (?nom-that-x-unit
               (HASH meaning ((filter-by-attribute ?out ?target ?scene ?specific-attribute)))
               --
               (HASH form ((meets ?rightmost-objects-unit ?present-unit) (meets ?present-unit ?in-unit) (meets ?in-unit ?the-unit) (meets ?the-unit ?image-unit) (meets ?image-unit ?leftmost-sharing-unit))))
              (?other-things-unit
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history))
               --
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history)))
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
              (?sharing-unit
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               --
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn other-things-present-sharing-x-with-y-cxn
             ((?nom-that-x-unit
               (args ((original-source ?context) (source ?context) (target ?out)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               (sem-cat (sem-cat relative)
                        (grammar ?grammar))
               (subunits (?other-things-unit ?present-unit ?sharing-unit)))
              (?other-things-unit
               (footprints (relative)))
              <-
              (?nom-that-x-unit
               (HASH meaning ((filter-by-attribute ?out ?target ?scene ?specific-attribute)))
               --
               (HASH form ((meets ?rightmost-objects-unit ?present-unit) (meets ?present-unit ?leftmost-sharing-unit))))
              (?other-things-unit
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history))
               --
               (args ((target ?target) (input ?original-source) (source ?context) (original-source ?inset)))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-other-unit)
                        (rightmost-unit ?rightmost-objects-unit))
               (footprints (NOT history)))
              (?present-unit
               --
               (HASH form ((string ?present-unit "present"))))
              
              (?sharing-unit
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit))
               --
               (args ((source ?context) (original-target ?original-source) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?leftmost-sharing-unit)
                        (rightmost-unit ?rightmost-sharing-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn that-share-the-same-attr-with-y-cxn
             ((?that-share-the-same-attr-with-y-unit
               (args ((source ?context) (original-target ?input) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?that-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?attribute-unit ?that-unit ?share-unit ?the-unit ?same-unit ?with-unit ?y-unit)))
              <-
              (?that-share-the-same-attr-with-y-unit
               (HASH meaning ((query ?specific-attribute ?unique-object ?scene ?attribute)))
               --
               (HASH form ((string ?that-unit "that")
                           (string ?share-unit "share")
                           (string ?the-unit "the")
                           (string ?same-unit "same")
                           (meets ?that-unit ?share-unit)
                           (meets ?share-unit ?the-unit)
                           (meets ?the-unit ?same-unit)
                           (meets ?same-unit ?attribute-unit)
                           (meets ?attribute-unit ?with-unit)
                           (meets ?with-unit ?leftmost-unit))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (syn-cat (syn-class noun))
               (sem-cat (sem-class attribute)))
              (?with-unit
               --
               (HASH form ((string ?with-unit "with"))))
              (?Y-unit
               (args ((source ?context)  (target ?unique-object) (input ?input) ;(original-target ?original-target)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((source ?context)  (target ?unique-object) (input ?input); (original-target ?original-target)
                      ))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class det-anaphoric)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn that-are-of-same-attr-as-y-cxn
             ((?that-are-of-same-attr-as-y-unit
               (args ((source ?context) (original-target ?input) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?that-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?attribute-unit ?that-unit ?are-unit ?of-unit ?same-unit ?as-unit ?y-unit)))
              <-
              (?that-are-of-same-attr-as-y-unit
               (HASH meaning ((query ?specific-attribute ?unique-object ?scene ?attribute)))
               --
               (HASH form ((string ?that-unit "that") (string ?are-unit "are") (string ?of-unit "of") (string ?same-unit "same")
                           (meets ?that-unit ?are-unit) (meets ?are-unit ?of-unit) (meets ?of-unit ?same-unit) (meets ?same-unit ?leftmost-attr-unit) (meets ?rightmost-attr-unit ?as-unit) (meets ?as-unit ?leftmost-unit))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-attr-unit)
                        (rightmost-unit ?rightmost-attr-unit))
               --
               (args ((target ?attribute)))
               (syn-cat (syn-class noun)
                        (leftmost-unit ?leftmost-attr-unit)
                        (rightmost-unit ?rightmost-attr-unit))
               (sem-cat (sem-class attribute)))
              (?as-unit
               --
               (HASH form ((string ?as-unit "as"))))
              (?Y-unit
               (args ((source ?context) (target ?unique-object) (input ?input) ;(original-target ?original-target)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((source ?context) (target ?unique-object) (input ?input) ;(original-target ?original-target)
))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class det-anaphoric)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn that-are-of-same-attr-with-y-cxn
             ((?that-are-of-same-attr-as-y-unit
               (args ((source ?context) (original-target ?input) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?that-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?attribute-unit ?that-unit ?are-unit ?of-unit ?same-unit ?as-unit ?y-unit)))
              <-
              (?that-are-of-same-attr-as-y-unit
               (HASH meaning ((query ?specific-attribute ?unique-object ?scene ?attribute)))
               --
               (HASH form ((string ?that-unit "that")
                           (string ?are-unit "are")
                           (string ?of-unit "of")
                           (string ?same-unit "same")
                           (meets ?that-unit ?are-unit)
                           (meets ?are-unit ?of-unit)
                           (meets ?of-unit ?same-unit)
                           (meets ?same-unit ?attribute-unit)
                           (meets ?attribute-unit ?as-unit)
                           (meets ?as-unit ?leftmost-unit))))
              (?attribute-unit
               (args ((target ?attribute)))
               (sem-cat (sem-class attribute))
               (syn-cat (syn-class noun))
               --
               (args ((target ?attribute)))
               (syn-cat (syn-class noun)
                        )
               (sem-cat (sem-class attribute)))
              (?as-unit
               --
               (HASH form ((string ?as-unit "with"))))
              (?Y-unit
               (args ((source ?context) (target ?unique-object) (input ?input); (original-target ?original-target)
                      ))
               (sem-cat (sem-class det-anaphoric))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((source ?context) (target ?unique-object) (input ?input); (original-target ?original-target)
                      ))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-unit)
                        (rightmost-unit ?rightmost-unit))
               (sem-cat (sem-class det-anaphoric)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn that-share-its-attr-cxn
             ((?that-share-its-attr-unit
               (args ((source ?context) (original-target ?object) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?that-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits (?that-unit ?share-unit ?possessive-unit))
               )
              <-
              (?that-share-its-attr-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?that-unit ?share-unit) (meets ?share-unit ?leftmost-poss-unit))))
              (?that-unit
               --
               (HASH form ((string ?that-unit "that"))))
              (?share-unit
               --
               (HASH form ((string ?share-unit "share"))))
              (?possessive-unit
               (args ((target ?unique) (source ?context) (object ?object)
                      (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?unique) (source ?context) (object ?object)
                      (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn sharing-its-attr-cxn
             ((?sharing-its-attr-unit
               (args ((source ?context) (original-target ?object) (target ?specific-attribute)))
               (sem-cat (sem-class sharing))
               (syn-cat (syn-class relative-clause)
                        (leftmost-unit ?share-unit)
                        (rightmost-unit ?rightmost-unit))
               (subunits ( ?share-unit ?possessive-unit))
               )
              <-
              (?sharing-its-attr-unit
               (HASH meaning ((query ?specific-attribute ?unique ?scene ?attribute)))
               --
               (HASH form ((meets ?share-unit ?leftmost-poss-unit))))
              
              (?share-unit
               --
               (HASH form ((string ?share-unit "sharing"))))
              (?possessive-unit
               (args ((target ?unique) (source ?context) (object ?object)
                      (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-unit))
               --
               (args ((target ?unique) (source ?context) (object ?object)
                      (attribute ?attribute)))
               (sem-cat (sem-class possessive))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-poss-unit)
                        (rightmost-unit ?rightmost-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)

;;pp

(def-fcg-cxn among-them-cxn
             ((?among-them-unit
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?among-unit)
                        (rightmost-unit ?them-unit))
               (sem-cat (sem-class multiple))
               (subunits (?among-unit ?them-unit)))
              <-
              (?among-them-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?context ?objects)))
               --
               (HASH form ((meets ?among-unit ?them-unit))))
              (?among-unit
               --
               (HASH form ((string ?among-unit "among"))))
              (?them-unit
               --
               (HASH form ((string ?them-unit "them"))))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)




(def-fcg-cxn in-this-group-cxn
             ((?in-this-group-unit
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?in-unit)
                        (rightmost-unit ?group-unit))
               (sem-cat (sem-class multiple))
               (subunits (?in-unit ?this-unit ?group-unit)))
              <-
              (?in-this-group-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?context ?objects)))
               --
               (HASH form ((meets ?in-unit ?this-unit) (meets ?this-unit ?group-unit))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?this-unit
               --
               (HASH form ((string ?this-unit "this"))))
              (?group-unit
               --
               (HASH form ((string ?group-unit "group"))))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn in-the-group-cxn
             ((?in-this-group-unit
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?in-unit)
                        (rightmost-unit ?group-unit))
               (sem-cat (sem-class multiple))
               (subunits (?in-unit ?the-unit ?group-unit)))
              <-
              (?in-this-group-unit
               (HASH meaning (;(get-memory ?history)
                              (get-last-topic ?objects ?memory)
                              (find-in-context ?target ?context ?objects)))
               --
               (HASH form ((meets ?in-unit ?the-unit) (meets ?this-unit ?group-unit))))
              (?in-unit
               --
               (HASH form ((string ?in-unit "in"))))
              (?the-unit
               --
               (HASH form ((string ?the-unit "the"))))
              (?group-unit
               --
               (HASH form ((string ?group-unit "group"))))
              (memory-unit
               --
               (memory ?memory)))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn in-a-x-cxn
             ((?in-a-x-unit
               (subunits (?in-unit ?a-unit ?x-unit))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?in-unit)
                        (rightmost-unit ?rightmost-unit))
               (args ((target ?target)
                      (context ?source))))
               <-
               (?in-a-x-unit
                --
                (HASH form ((meets ?in-unit ?a-unit)
                            (meets ?a-unit ?leftmost-unit))))
               (?in-unit
                --
                (HASH form ((string ?in-unit "in"))))
               (?a-unit
                --
                (HASH form ((string ?a-unit "a"))))
               (?x-unit
                (args ((target ?target)
                       (source ?source)
                       ))
                (syn-cat (syn-function nominal)
                         (number singular)
                         (leftmost-unit ?leftmost-unit)
                         (rightmost-unit ?rightmost-unit))
                --
                (args ((target ?target)
                       (source ?source)
                       ))
                (syn-cat (syn-function nominal)
                         (number singular)
                         (leftmost-unit ?leftmost-unit)
                         (rightmost-unit ?rightmost-unit)))))

(def-fcg-cxn x-pp-cxn
             ((?x-pp-unit
               (subunits (?x-unit ?pp-unit))
               (sem-cat (sem-class plural-adj-nom))
               (args ((target ?inset) (source ?context) (original-source ?context)))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-pp-unit)))
              <-
              (?x-pp-unit
               --
               (HASH form ((meets ?rightmost-np-unit ?leftmost-pp-unit))))
              (?x-unit
               (args ((target ?inset) (source ?target) (original-source ?target) #|(input ?original-source) (output ?target)|#))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               --
               (args ((target ?inset) (source ?target) (original-source ?target) #|(input ?original-source) (output ?target)|#))
               (sem-cat (sem-class plural-adj-nom))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit)))
              (?pp-unit
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit))
               (sem-cat (sem-class multiple))
               --
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit))
               (sem-cat (sem-class multiple))))
             :cxn-inventory *clevr-dialog*)

(def-fcg-cxn x-pp-singular-cxn
             ((?x-pp-unit
               (subunits (?x-unit ?pp-unit))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (args ((target ?inset) (source ?context) (original-source ?context)))
               (syn-cat (syn-class np)
                        (number ?number)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-pp-unit)))
              <-
              (?x-pp-unit
               --
               (HASH form ((meets ?rightmost-np-unit ?leftmost-pp-unit))))
              (?x-unit
               (args ((target ?inset) (source ?target) (original-source ?target) #|(input ?original-source) (output ?target)|#))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (footprints (NOT other))
               --
               (args ((target ?inset) (source ?target) (original-source ?target) #|(input ?original-source) (output ?target)|#))
               (sem-cat (sem-class plural-adj-nom)
                        (grammar ?grammar))
               (syn-cat (syn-class np)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (footprints (NOT other)))
              (?pp-unit
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit))
               
               --
               (args ((target ?target) (context ?context)))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit))))
             :cxn-inventory *clevr-dialog*)


(def-fcg-cxn x-property-pp-cxn
             ((?x-property-pp-unit
               (subunits (?x-unit ?pp-unit))
               (sem-cat (sem-class nominal)
                        (grammar ?grammar))
               (adjective +)
               (args ((target ?out) (source ?target)  ))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (starts-with ?letter)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-pp-unit))
               (footprints (pp)))
              (?x-unit
               (footprints (plural)))
              <-
              (?x-property-pp-unit
               (HASH meaning ((filter-by-attribute ?out ?inset ?scene ?attribute)))
               --
               (HASH form ((meets ?rightmost-np-unit ?leftmost-pp-unit))))
              (?x-unit
               (args ((target ?inset) (source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (footprints (NOT exist-question any other))
               --
               (args ((target ?inset) (source ?target)))
               (sem-cat (grammar ?grammar))
               (syn-cat (syn-function nominal)
                        (number ?number)
                        (leftmost-unit ?leftmost-np-unit)
                        (rightmost-unit ?rightmost-np-unit))
               (footprints (NOT exist-question any other)))
              (?pp-unit
               (args ((target ?attribute) ))
               (sem-cat (sem-class property))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit))
               
               --
               (args ((target ?attribute) ))
               (sem-cat (sem-class property))
               (syn-cat (syn-class pp)
                        (leftmost-unit ?leftmost-pp-unit)
                        (rightmost-unit ?rightmost-pp-unit)))
              (scene-unit
               --
               (scene ?scene)))
             :cxn-inventory *clevr-dialog*)
