(ql:quickload :fcg)
(in-package :fcg)

(def-fcg-cxn give-me-the-cities-in-usa-cxn-1
             ((?holistic-unit
               (category give-me-the-cities-in-usa-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-1)))
              <-
              (?holistic-unit
               (HASH meaning ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2)
                              (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0)
                              (FROM ?FILTER-1 ?FILTER-0)
                              (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1)
                              (BIND COLUMN ?COLUMN-2 CITY_NAME)
                              (BIND CONCEPT ?ALIAS-0 CITYALIAS0)
                              (BIND TABLE ?TABLE-0 CITY)))
               --
               (HASH form ((sequence "give me the cities in usa" ?left-1 ?right-1))))))

(def-fcg-cxn give-me-the-slot-1-in-usa-cxn-1
             ((?item-based-unit
               (category give-me-the-slot-1-in-usa-cxn-cat-1)
               (meaning-args (?target))
               (form-args (?left-1 ?right-02))
               (subunits (?slot-1)))
              <-
              (?item-based-unit
               (HASH meaning ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2)
                              (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0)
                              (FROM ?FILTER-1 ?FILTER-0)
                              (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1)
                              (BIND CONCEPT ?ALIAS-0 CITYALIAS0)
                              (BIND TABLE ?TABLE-0 CITY)))
               --
               (HASH form ((sequence "give me the " ?left-1 ?right-1)
                           (sequence " in usa" ?left-2 ?right-2))))
              (?slot-1
               (category give-me-the-slot-1-in-usa-slot-1-cat-1)
               (meaning-args (?attribute-1))
               --
               (form-args (?right-1 ?left-2))
               (category give-me-the-slot-1-in-usa-slot-1-cat-1))))


(def-fcg-cxn states-cxn-1
             ((?holistic-unit
               (category states-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 states)))
               --
               (HASH form ((sequence "states" ?left-1 ?right-1))))))

(def-fcg-cxn cities-cxn-1
             ((?holistic-unit
               (category cities-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 cities)))
               --
               (HASH form ((sequence "cities" ?left-1 ?right-1))))))

(def-fcg-cxn lakes-cxn-1
             ((?holistic-unit
               (category lakes-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?attribute-1)))
              <-
              (?holistic-unit
               (HASH meaning ((bind attribute-category ?attribute-1 lakes)))
               --
               (HASH form ((sequence "lakes" ?left-1 ?right-1))))))



