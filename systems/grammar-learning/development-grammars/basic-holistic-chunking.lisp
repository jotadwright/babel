(ql:quickload :fcg)

(in-package :fcg)
(activate-monitor trace-fcg)



(def-fcg-constructions demo-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (de-render-mode de-render-string-meets)
                  (constituents sequence)
                  (dependents sequence))
  :hierarchy-features (constituents dependents))



(def-fcg-cxn large-gray-cxn
             ((?large-gray-unit
               (args (?size-4 ?color-2))
               (syn-cat (phrase-type holistic)
                        (lex-class large-gray))
               (boundaries
                (left ?large-unit)
                (right ?gray-unit))
               )
              <-
              (?large-gray-unit
               (HASH meaning ((bind size-category ?size-4 large)
                              (bind color-category ?color-2 gray)))
               --
               (HASH form ((string ?large-unit "large")
                           (string ?gray-unit "gray")
                           (meets ?large-unit ?gray-unit))))))

(comprehend "large gray")


(def-fcg-cxn the-x-object-is-what-shape-cxn
             ((?item-based-unit
               (syn-cat (phrase-type item-based))
               (subunits (?large-gray-unit)))
               
              (?large-gray-unit
               (syn-cat (lex-class large-gray))
               (boundaries
                (left ?large-unit)
                (right ?gray-unit)))
              <-
              (?item-based-unit
               (HASH meaning ((QUERY ?TARGET-8 ?SOURCE-10 ?ATTRIBUTE-2)
                              (FILTER ?TARGET-2 ?TARGET-1 ?COLOR-2)
                              (BIND SHAPE-CATEGORY ?SHAPE-8 THING)
                              (BIND ATTRIBUTE-CATEGORY ?ATTRIBUTE-2 SHAPE)
                              (FILTER ?TARGET-1 ?SOURCE-1 ?SHAPE-8)
                              (UNIQUE ?SOURCE-10 ?TARGET-39552)
                              (FILTER ?TARGET-39552 ?TARGET-2 ?SIZE-4)
                              (GET-CONTEXT ?SOURCE-1)))
               --
               (HASH form ((STRING ?THE-66 "The")
                           (STRING ?OBJECT-66 "object")
                           (STRING ?IS-66 "is")
                           (STRING ?WHAT-66 "what")
                           (STRING ?SHAPE?-66 "shape?")
                           (MEETS ?THE-66 ?large-unit)
                           (MEETS ?gray-unit ?OBJECT-66)
                           (MEETS ?OBJECT-66 ?IS-66)
                           (MEETS ?IS-66 ?WHAT-66)
                           (MEETS ?WHAT-66 ?SHAPE?-66))))
              (?large-gray-unit
               (args (?SIZE-4 ?COLOR-2))
               --)))

(comprehend "The large gray object is what shape?")