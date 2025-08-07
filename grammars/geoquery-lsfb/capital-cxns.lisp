(in-package :geoquery-lsfb-grammar-copy)

(def-fcg-cxn adj-capital-or-city-of-state-or-country-np-cxn\(?state-or-country-np\,?adj\,?city-or-capital\)
             ((?adj-capital-or-city-of-state-or-country-np-unit
               (subunits (?state-or-country-np-unit ?city-or-capital-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?city-or-country-np-left)
                            (right ?city-or-capital-np-right))))
              <-
              (?state-or-country-np-unit
               (sem-cat (referent-class state-or-country))
               (args ((scope ?e)
                      (target ?b)))
               (modified no)
               --
               (sem-cat (referent-class state-or-country)
                        (referent-number sg))
               (syn-cat np)
               (boundaries ((left ?state-or-country-np-left)
                            (right ?state-or-country-np-right))))
              (?city-or-capital-unit
               (sem-cat (referent-class city-or-capital))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-or-capital-np-left)
                            (right ?city-or-capital-np-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?adj-capital-or-city-of-state-or-country-np-unit
               (HASH meaning ((loc ?e ?a ?b)))
               --
               (HASH form ((adjacent ?state-or-country-np-right ?adj-left)
                           (adjacent ?adj-right ?city-or-capital-np-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn capital-cxn
             ((?capital-unit
               (sem-cat (referent-class capital))
               (args ((scope ?e)
                      (target ?f)))
               (syn-cat noun)
               (boundaries ((left ?capitale)
                            (right ?capitale))))
              <-
              (?capital-unit
               (HASH meaning ((capital ?e ?f)))
               --
               
               (HASH form ((two-hand-articulation ?capitale "")))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn capital-of-state-np-cxn\(?state-np\,?pt\,?capital\)
             ((?capital-located-in-state-np-unit
               (subunits (?state-np-unit ?capital-unit ?pt-unit))
               (sem-cat (referent entity))
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?capital-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (modified no)
               (args ((scope ?e)
                      (target ?b)))
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right)))
               )
              (?capital-unit
               (sem-cat (referent-class capital))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?capital-left)
                            (right ?capital-right))))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?capital-located-in-state-np-unit
               (HASH meaning ((loc ?e ?a ?b)))
               --
               (HASH form ((adjacent ?state-np-right ?pt-unit)
                           (adjacent ?pt-unit ?capital-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn capital-of-state-np-cxn\(?state-np\,mais\,?pt\,nom\,?capital\)
             ((?capital-of-state-np-unit
               (subunits (?state-np-unit ?pt-unit ?capital-unit))
               (sem-cat (referent entity-name))
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?capital-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?b)))
               --
               (syn-cat np)
               (modified yes)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right)))
               )
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?capital-unit
               (sem-cat (referent-class capital))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?capital-left)
                            (right ?capital-right))))
              (?capital-of-state-np-unit
               (HASH meaning ((loc ?e ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?mais "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?state-np-right ?mais)
                           (adjacent ?mais ?pt-unit)
                           (adjacent ?pt-unit ?nom)
                           (adjacent ?nom ?capital-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn state-capitals-cxn\(?state-np\,il-y-a\,different\,capitale\,?ds\)
             ((?state-capitals-unit
               (subunits (?state-np-unit ?ds-unit))
               (sem-cat (referent entity))
               (args ((scope ?f)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?ds-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number pl))
               (args ((scope ?f)
                      (target ?c)))
               (modified no)
               --
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right)))
               )
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-capitals-unit
               (HASH meaning ((capital ?f ?c ?a)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?capitale "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?capitale)
                           (adjacent ?capitale ?ds-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn capital-of-state-np-cxn\(?state-np\,il-y-a\,?capital-np\)
             ((?capital-located-in-?state-unit
               (subunits (?state-np-unit ?capital-np-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?capital-np-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?b)))
               (modified no)
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?capital-np-unit
               (sem-cat (referent-class capital))
               (args ((scope ?d)
                      (target ?a)))
               --
               (syn-cat np)
               (boundaries ((left ?capital-np-left)
                            (right ?capital-np-right))))
              (?state-np-city-unit
               (HASH meaning ((loc ?e ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?capital-np-left)))))
             :cxn-inventory *geoquery-lsfb*)
