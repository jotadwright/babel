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


(def-fcg-cxn tiny-yellow-cxn
             ((?tiny-yellow-unit
               (args (?size-4 ?color-2))
               (syn-cat (phrase-type holistic)
                        (lex-class large-gray))
               (boundaries
                (left ?tiny-unit)
                (right ?yellow-unit))
               )
              <-
              (?tiny-yellow-unit
               (HASH meaning ((bind size-category ?size-4 tiny)
                              (bind color-category ?color-2 yellow)))
               --
               (HASH form ((string ?tiny-unit "tiny")
                           (string ?yellow-unit "yellow")
                           (meets ?tiny-unit ?yellow-unit))))))


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
               (HASH meaning ((query ?target-8 ?source-10 ?attribute-2)
                              (filter ?target-2 ?target-1 ?color-2)
                              (bind shape-category ?shape-8 thing)
                              (bind attribute-category ?attribute-2 shape)
                              (filter ?target-1 ?source-1 ?shape-8)
                              (unique ?source-10 ?target-39552)
                              (filter ?target-39552 ?target-2 ?size-4)
                              (get-context ?source-1)))
               --
               (HASH form ((string ?the-66 "The")
                           (string ?object-66 "object")
                           (string ?is-66 "is")
                           (string ?what-66 "what")
                           (string ?shape?-66 "shape?")
                           (meets ?the-66 ?large-unit)
                           (meets ?gray-unit ?object-66)
                           (meets ?object-66 ?is-66)
                           (meets ?is-66 ?what-66)
                           (meets ?what-66 ?shape?-66))))
              (?large-gray-unit
               (args (?size-4 ?color-2))
               --
               (boundaries
                (left ?large-unit)
                (right ?gray-unit)))))



(comprehend-and-formulate "The tiny yellow object is what shape?")
(comprehend-and-formulate "The large gray object is what shape?")
