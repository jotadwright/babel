(in-package :geoquery-lsfb-grammar-copy)


(def-fcg-cxn cities-located-in-state-cxn\(?state-np\,dans\,il-y-a\,?city\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?city-right))))
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
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-np-left)
                            (right ?city-np-right))))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?dans)
                           (adjacent ?dans ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           ))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,il-y-a\,un\,?city\,?adj\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?e)
                      (target ?b)))
               (boundaries ((left ?state-np-left)
                            (right ?adj-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state))
               (args ((scope ?f)
                      (target ?c)))
               (modified no)
               --
               (sem-cat (referent-class state)
                        (referent-number sg))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?b)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?e)
                      (target ?b)
                      (source ?f)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?f ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?un "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?un)
                           (adjacent ?un ?city-left)
                           (adjacent ?city-right ?adj-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn city-cxn\(ville\)
             ((?city-unit
               (args ((scope ?d)
                      (target ?a)))
               (sem-cat (referent-class city)
                        (referent-number sg))
               (syn-cat noun)
               (boundaries ((left ?ville)
                            (right ?ville))))
              <-
              (?city-unit
               (HASH meaning ((city ?d ?a)))
               --
               
               (HASH form ((two-hand-articulation ?ville "")))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn city-cxn\(ville-pl\)
             ((?city-unit
               (args ((scope ?d)
                      (target ?a)))
               (sem-cat (referent-class city)
                        (referent-number pl))
               (syn-cat noun)
               (boundaries ((left ?ville)
                            (right ?ville))))
              <-
              (?city-unit
               (HASH meaning ((city ?d ?a)))
               --
               
               (HASH form ((two-hand-articulation ?ville "")))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn city-np-cxn\(?pt\,ville\,?pt\)
             ((?city-unit
               (sem-cat (referent-class city))
               (subunits (?pt-unit-1 ?pt-unit-2))
               (args ((scope ?d)
                      (target ?a)))
               (syn-cat np)
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?city-unit
               (HASH meaning ((city ?d ?a)))
               --
               (HASH form ((two-hand-articulation ?ville "")
                           (adjacent ?pt-unit-1 ?ville)
                           (adjacent ?ville ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn city-np-cxn\(un\,?pt\,ville\)
             ((?city-unit
               (sem-cat (referent-class city))
               (subunits (?pt-unit))
               (args ((scope ?d)
                      (target ?a)))
               (syn-cat np)
               (boundaries ((left ?un)
                            (right ?ville))))
              <-
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?city-unit
               (HASH meaning ((city ?d ?a)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (two-hand-articulation ?ville "")
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?ville)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adv-city-located-in-state-cxn\(?state-np\,il-y-a\,?adv\,?city\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?adverb-unit))
               (sem-cat (referent number))
               (args ((scope ?a)
                      (target ?e)))
               (boundaries ((left ?state-np-left)
                            (right ?city-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?c)))
               (modified no)
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?b)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adverb-unit
               (args ((scope ?e)
                      (target ?b)
                      (source-1 ?f)
                      (source-2 ?a)))
               --
               (boundaries ((left ?adv-left)
                            (right ?adv-right)))
               (syn-cat adv))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?f ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?adv-left)
                           (adjacent ?adv-right ?city-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adv-city-located-in-country-cxn\(?country-np\,il-y-a\,?adv\,?city\,?ds\)
             ((?cities-located-in-country-unit
               (subunits (?country-np-unit ?city-unit ?adverb-unit ?ds-unit))
               (sem-cat (referent number))
               (args ((scope ?a)
                      (target ?e)))
               (boundaries ((left ?country-np-left)
                            (right ?city-right))))
              <-
              (?country-np-unit
               (sem-cat (referent-class country))
               (args ((scope ?f)
                      (target ?c)))
               --
               (sem-cat (referent-class country))
               (syn-cat np)
               (boundaries ((left ?country-np-left)
                            (right ?country-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?b)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adverb-unit
               (args ((scope ?e)
                      (target ?b)
                      (source-1 ?f)
                      (source-2 ?a)))
               --
               (boundaries ((left ?adv-left)
                            (right ?adv-right)))
               (syn-cat adv))
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
              (?cities-located-in-country-unit
               (HASH meaning ((loc ?f ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?adv-left)
                           (adjacent ?adv-right ?city-left)
                           (adjacent ?city-right ?ds-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-adv-city-located-in-state-cxn\(?state-np\,dans\,il-y-a\,?adj\,?city\,?adv\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?adverb-unit ?adjective-unit))
               (sem-cat (referent number))
               (args ((scope ?a)
                      (target ?e)))
               (boundaries ((left ?state-np-left)
                            (right ?adv-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?c)))
               (modified no)
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?f)
                      (target ?b)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adverb-unit
               (args ((scope ?e)
                      (target ?b)
                      (source-1 ?f)
                      (source-2 ?a)))
               --
               (boundaries ((left ?adv-left)
                            (right ?adv-right)))
               (syn-cat adv))
              (?adjective-unit
               (args ((scope ?f)
                      (target ?b)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat adj))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?f ?b ?c)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?dans)
                           (adjacent ?dans ?il-y-a)
                           (adjacent ?il-y-a ?adj-left)
                           (adjacent ?adj-right ?city-left)
                           (adjacent ?city-right ?adv-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,il-y-a\,different\,?city\,?adj\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?adj-right))))
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
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat adj))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?city-left)
                           (adjacent ?city-right ?adj-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,il-y-a\,?city\,?adj\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?adj-right))))
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
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat adj))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           (adjacent ?city-right ?adj-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-country-cxn\(?country-np\,il-y-a\,?adj\,?city\)
             ((?cities-located-in-country-unit
               (subunits (?country-np-unit ?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?country-np-left)
                            (right ?city-right))))
              <-
              (?country-np-unit
               (sem-cat (referent-class country))
               (args ((scope ?e)
                      (target ?b)))
               --
               (sem-cat (referent-class country))
               (syn-cat np)
               (boundaries ((left ?country-np-left)
                            (right ?country-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?cities-located-in-country-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?country-np-right ?il-y-a)
                           (adjacent ?il-y-a ?adj-left)
                           (adjacent ?adj-right ?city-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-country-cxn\(?country-np\,il-y-a\,?adj\,?city\)
             ((?cities-located-in-country-unit
               (subunits (?country-np-unit ?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?country-np-left)
                            (right ?city-right))))
              <-
              (?country-np-unit
               (sem-cat (referent-class country))
               (args ((scope ?e)
                      (target ?b)))
               --
               (sem-cat (referent-class country))
               (syn-cat np)
               (boundaries ((left ?country-np-left)
                            (right ?country-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?cities-located-in-country-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (adjacent ?country-np-right ?il-y-a)
                           (adjacent ?il-y-a ?adj-left)
                           (adjacent ?adj-right ?city-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn city-located-in-state-cxn\(?state-np\,il-y-a\,?city\,different\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit))
               (sem-cat (referent entity))
               (args ((scope ?f)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?different))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?b)))
               (modified no)
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?d ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (adjacent ?state-np-right ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           (adjacent ?city-right ?different)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,un\,?pt\,il-y-a\,?city\,different\,?adj\,ville\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?pt-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?e)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?ville))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (modified no)
               (args ((scope ?f)
                      (target ?b)))
               --
               ;(sem-cat (referent-class state))
               ;(syn-cat np)
               ;(boundaries ((left ?state-np-left)
                            ;(right ?state-np-right)))
               )
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?e)
                      (target ?a)
                      (source ?f)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?f ?a ?b)))
               --
               (HASH form ((right-hand-articulation ?un "")
                           (right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?ville "")
                           (adjacent ?state-np-right ?un)
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           (adjacent ?city-right ?different)
                           (adjacent ?different ?adj-left)
                           (adjacent ?adj-right ?ville)
                           ))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,il-y-a\,different\,?city\,?ds\,?adj\,?pt\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?pt-unit ?adjective-unit ?ds-unit))
               (sem-cat (referent entity))
               (args ((scope ?e)
                      (target ?b)))
               (boundaries ((left ?state-np-left)
                            (right ?pt-unit))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (modified no)
               (args ((scope ?e)
                      (target ?c)))
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right)))
               )
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?b)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?e)
                      (target ?b)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat adj))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
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
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?city-left)
                           (adjacent ?city-right ?ds-left)
                           (adjacent ?ds-right ?adj-left)
                           (adjacent ?adj-right ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-located-in-state-cxn\(?state-np\,dans\,?pt\,il-y-a\,?adj\,?city\)
             ((?cities-located-in-state-unit
               (subunits (?state-np-unit ?city-unit ?pt-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?state-np-left)
                            (right ?city-right))))
              <-
              (?state-np-unit
               (sem-cat (referent-class state)
                        (referent-number sg))
               (modified no)
               (args ((scope ?e)
                      (target ?c)))
               --
               (sem-cat (referent-class state))
               (syn-cat np)
               (boundaries ((left ?state-np-left)
                            (right ?state-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?cities-located-in-state-unit
               (HASH meaning ((loc ?e ?b ?c)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?il-y-a)
                           (adjacent ?il-y-a ?adj-left)
                           (adjacent ?adj-right ?city-left)))))
             :cxn-inventory *geoquery-lsfb*)


(def-fcg-cxn adj-city-located-in-country-cxn\(?country-np\,il-y-a\,?city\,different\,un\,?pt\,?adj\)
             ((?cities-located-in-country-unit
               (subunits (?country-np-unit ?city-unit ?pt-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?country-np-left)
                            (right ?adj-right))))
              <-
              (?country-np-unit
               (sem-cat (referent-class country))
               (args ((scope ?e)
                      (target ?b)))
               --
               (sem-cat (referent-class country))
               (syn-cat np)
               (boundaries ((left ?country-np-left)
                            (right ?country-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number pl))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?cities-located-in-country-unit
               (HASH meaning ((loc ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (right-hand-articulation ?un "")
                           (adjacent ?country-np-right ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           (adjacent ?city-right ?different)
                           (adjacent ?different ?un)
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?adj-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-located-in-country-cxn\(?country-np\,il-y-a\,?city\,?ds\,un\,?pt\,?adj\,ville\)
             ((?cities-located-in-country-unit
               (subunits (?country-np-unit ?city-unit ?pt-unit ?adjective-unit ?ds-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?a)))
               (boundaries ((left ?country-np-left)
                            (right ?ville))))
              <-
              (?country-np-unit
               (sem-cat (referent-class country))
               (args ((scope ?e)
                      (target ?b)))
               --
               (sem-cat (referent-class country))
               (syn-cat np)
               (boundaries ((left ?country-np-left)
                            (right ?country-np-right))))
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?e)
                      (target ?a)))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?d)
                      (target ?a)
                      (source ?e)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?cities-located-in-country-unit
               (HASH meaning ((loc ?e ?b ?c)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?ville "")
                           (adjacent ?country-np-right ?il-y-a)
                           (adjacent ?il-y-a ?city-left)
                           (adjacent ?city-right ?ds-left)
                           (adjacent ?ds-right ?un)
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?adj-left)
                           (adjacent ?adj-right ?ville)
                           ))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-cxn\(?adj\,?city\)
             ((?adj-city-unit
               (subunits (?city-unit ?adjective-unit))
               (sem-cat (referent entity))
               (args ((scope ?c)
                      (target ?a)))
               (boundaries ((left ?adj-left)
                            (right ?city-right))))
              (?city-unit
               (footprints (adj)))
              <-
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?a)))
               (footprints (not adj))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?c)
                      (target ?a)
                      (source ?d)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?adj-city-unit
               --
               (HASH form ((adjacent ?adj-right ?city-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn adj-city-cxn\(il-y-a\,different\,?city\,different\,?adj\,ville\)
             ((?adj-city-unit
               (subunits (?city-unit ?adjective-unit ?pt-unit))
               (sem-cat (referent entity))
               (args ((scope ?d)
                      (target ?b)))
               (boundaries ((left ?il-y-a)
                            (right ?ville))))
              (?city-unit
               (footprints (adj)))
              <-
              (?city-unit
               (sem-cat (referent-class city)
                        (referent-number sg))
               (args ((scope ?d)
                      (target ?a)))
               (footprints (not adj))
               --
               (syn-cat noun)
               (boundaries ((left ?city-left)
                            (right ?city-right))))
              (?adjective-unit
               (args ((scope ?c)
                      (target ?a)
                      (source ?d)))
               --
               (boundaries ((left ?adj-left)
                            (right ?adj-right)))
               (syn-cat comparative-adj))
              (?adj-city-unit
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different-1 "")
                           (two-hand-articulation ?different-2 "")
                           (right-hand-articulation ?ville "")
                           (adjacent ?il-y-a ?different-1)
                           (adjacent ?different-1 ?city-left)
                           (adjacent ?city-right ?different-2)
                           (adjacent ?different-2 ?adj-left)
                           (adjacent ?adj-right ?ville)))))
             :cxn-inventory *geoquery-lsfb*)