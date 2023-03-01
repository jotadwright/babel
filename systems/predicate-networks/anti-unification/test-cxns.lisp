(ql:quickload :fcg)
(in-package :fcg)

(def-fcg-constructions anti-unified-cxns
  :feature-types ((form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (form-args sequence)
                  (meaning-args sequence))
  :fcg-configurations ((:cxn-supplier-mode . :scores)
                       (:parse-goal-tests :no-applicable-cxns
                                          :connected-semantic-network
                                          :no-strings-in-root)
                       (:production-goal-tests :no-applicable-cxns
                                               :connected-structure
                                               :no-meaning-in-root)
                       (:th-connected-mode . :neighbours))
  :visualization-configurations
  ((:show-constructional-dependencies . nil)
   (:show-categorial-network . t))

  (def-fcg-cxn what-color-is-the-x-cxn
               ((?item-based-unit
                 (syn-cat (lex-class what-color-is-the-x))
                 (meaning-args (?x-22678 ?x-22680 ?x-22682 ?x-22679))
                 (form-args (?x-22740 ?x-22741))
                 (subunits (?slot-unit)))
                <-
                (?item-based-unit
                 (HASH meaning ((get-context ?x-22676)
                                (filter ?x-22677 ?x-22676 ?x-22678)
                                (filter ?x-22679 ?x-22677 ?x-22680)
                                (unique ?x-22681 ?x-22682)
                                (query ?x-22683 ?x-22681 ?x-22684)
                                (bind attribute-category ?x-22684 color)))
                 --
                 (HASH form ((string ?x-22736 "what")
                             (meets ?x-22736 ?x-22737)
                             (string ?x-22737 "color")
                             (meets ?x-22737 ?x-22738)
                             (string ?x-22738 "is")
                             (meets ?x-22738 ?x-22739)
                             (string ?x-22739 "the")
                             (meets ?x-22739 ?x-22740)
                             (meets ?x-22740 ?x-22741))))
                 (?slot-unit
                  (meaning-args (?x-22678 ?x-22680 ?x-22682 ?x-22679))
                  (syn-cat (lex-class what-color-is-the-x-\(x\)))
                  --
                  (syn-cat (lex-class what-color-is-the-x-\(x\)))
                  (form-args (?x-22740 ?x-22741)))))

  (def-fcg-cxn large-cube-cxn
               ((?slot-unit
                 (syn-cat (lex-class large-cube))
                 (meaning-args (?shape-1 ?size-1 ?x ?x))
                 (form-args (?large-74978 ?cube-74979)))
                <-
                (?slot-unit
                 (HASH meaning ((bind shape-category ?shape-1 cube)
                                (bind size-category ?size-1 large)))
                 --
                 (HASH form ((string ?large-74978 "large")
                             (string ?cube-74979 "cube"))))))

  (def-fcg-cxn small-blue-sphere-cxn
               ((?slot-unit
                 (syn-cat (lex-class small-blue-sphere))
                 (meaning-args (?b1 ?b2 ?s3 ?s2))
                 (form-args (?small-74984 ?blue-74985)))
                <-
                (?slot-unit
                 (HASH meaning ((bind shape-category ?b1 sphere)
                                (bind color-category ?b2 blue)
                                (filter ?s3 ?s2 ?b3)
                                (bind size-category ?b3 small)))
                 --
                 (HASH form ((string ?small-74984 "small")
                             (string ?blue-74985 "blue")
                             (meets ?blue-74985 ?sphere-74986)
                             (string ?sphere-74986 "sphere"))))))

  
)


(add-categories '(small-blue-sphere large-cube what-color-is-the-x-\(x\) what-color-is-the-x)
                (categorial-network *fcg-constructions*))
(add-link 'small-blue-sphere 'what-color-is-the-x-\(x\)
          (categorial-network *fcg-constructions*))
(add-link 'large-cube 'what-color-is-the-x-\(x\)
          (categorial-network *fcg-constructions*))
(activate-monitor trace-fcg)
(comprehend-and-formulate "what color is the large cube")
(comprehend-and-formulate "what color is the small blue sphere")

#|

--- Result 1 (cost: 7) ---

- Generalisation:

((get-context ?x-22676)
 (filter ?x-22677 ?x-22676 ?x-22678)
 (filter ?x-22679 ?x-22677 ?x-22680)
 (unique ?x-22681 ?x-22682)
 (query ?x-22683 ?x-22681 ?x-22684)
 (bind attribute-category ?x-22684 color))

- Pattern bindings:

((?attribute-1 . ?x-22684)
 (?target . ?x-22683)
 (?set-2 . ?x-22682)
 (?object-1 . ?x-22681)
 (?size-1 . ?x-22680)
 (?set-2 . ?x-22679)
 (?shape-1 . ?x-22678)
 (?set-1 . ?x-22677)
 (?context . ?x-22676))

- Source bindings:

((?b4 . ?x-22684)
 (?t . ?x-22683)
 (?s3 . ?x-22682)
 (?o1 . ?x-22681)
 (?b2 . ?x-22680)
 (?s2 . ?x-22679)
 (?b1 . ?x-22678)
 (?s1 . ?x-22677)
 (?c . ?x-22676))

- Pattern delta:

((bind shape-category ?shape-1 cube)
 (bind size-category ?size-1 large))

- Source delta:

((bind shape-category ?b1 sphere)
 (bind color-category ?b2 blue)
 (filter ?s3 ?s2 ?b3)
 (bind size-category ?b3 small))





--- Result 1 (cost: 6) ---

- Generalisation:

((string ?x-22736 what)
 (meets ?x-22736 ?x-22737)
 (string ?x-22737 color)
 (meets ?x-22737 ?x-22738)
 (string ?x-22738 is)
 (meets ?x-22738 ?x-22739)
 (string ?x-22739 the)
 (meets ?x-22739 ?x-22740)
 (meets ?x-22740 ?x-22741))

- Pattern bindings:

((?cube-74979 . ?x-22741)
 (?large-74978 . ?x-22740)
 (?the-74977 . ?x-22739)
 (?is-74976 . ?x-22738)
 (?color-74975 . ?x-22737)
 (?what-74974 . ?x-22736))

- Source bindings:

((?blue-74985 . ?x-22741)
 (?small-74984 . ?x-22740)
 (?the-74983 . ?x-22739)
 (?is-74982 . ?x-22738)
 (?color-74981 . ?x-22737)
 (?what-74980 . ?x-22736))

- Pattern delta:

((string ?large-74978 large)
 (string ?cube-74979 cube))

- Source delta:

((string ?small-74984 small)
 (string ?blue-74985 blue)
 (meets ?blue-74985 ?sphere-74986)
 (string ?sphere-74986 sphere))


|#