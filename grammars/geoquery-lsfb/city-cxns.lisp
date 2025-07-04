(in-package :geoquery-lsfb-grammar)

(def-fcg-cxn cities-in-?x-cxn-1
             ((?cities-in-?x-additional-signs-unit
               (form ((two-hand-articulation ?different "")
                      (adjacent ?ville ?different)
                      (adjacent ?cities-in-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?ville)
                      (right-hand-articulation ?il-y-a ""))))
              (?cities-in-?x-unit
               (subunits (?cities-in-?x\(?x\) ?cities-in-?x-additional-signs-unit))
               (unit-cat cities-in-?x-cat)
               (args ((target ?a)
                      (scope ?f)))
               (boundaries ((left ?cities-in-?x\(?x\)-boundary-left)
                            (right ?different))))
              <-
              (?cities-in-?x-unit
               (HASH meaning ((city ?f ?a)
                              (loc ?f ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?ville ""))))
              (?cities-in-?x\(?x\)
               (unit-cat cities-in-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?b)))
               --
               (unit-cat cities-in-?x\(?x\)-cat)
               (boundaries ((right ?cities-in-?x\(?x\)-boundary-right)
                            (left ?cities-in-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn cities-in-?x-cxn-2
             ((?cities-in-?x-additional-signs-unit
               (form ((two-hand-articulation ?different "")
                      (adjacent ?ville ?different)
                      (adjacent ?cities-in-?x\(?x\)-boundary-right ?il-y-a)
                      (adjacent ?il-y-a ?ville)
                      (right-hand-articulation ?il-y-a ""))))
              (?cities-in-?x-unit
               (subunits (?cities-in-?x\(?x\) ?cities-in-?x-additional-signs-unit))
               (unit-cat cities-in-?x-cat)
               (args ((target ?a)
                      (scope ?f)))
               (boundaries ((left ?cities-in-?x\(?x\)-boundary-left)
                            (right ?different))))
              <-
              (?cities-in-?x-unit
               (HASH meaning ((city ?f ?a)
                              (loc ?f ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?ville ""))))
              (?cities-in-?x\(?x\)
               (unit-cat cities-in-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?b)))
               --
               (unit-cat cities-in-?x\(?x\)-cat)
               (boundaries ((right ?cities-in-?x\(?x\)-boundary-right)
                            (left ?cities-in-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)

(def-fcg-cxn largest-city-in-?x-cxn-1
             ((?largest-city-in-?x-unit
               (subunits (?largest-city-in-?x\(?x\) ?largest-city-in-?x-optional-signs-unit))
               (unit-cat largest-city-in-?x-cat)
               (args ((target ?a)
                      (scope ?e)))
               (boundaries ((left ?largest-city-in-?x\(?x\)-boundary-left)
                            (right ?ville-2))))
              (?largest-city-in-?x-optional-signs-unit
               (form ((adjacent ?largest-city-in-?x\(?x\)-boundary-right ?ville-1))))
              <-
              (?largest-city-in-?x-unit
               (HASH meaning ((largest ?e ?a ?f)
                              (city ?f ?a)
                              (loc ?f ?a ?b)))
               --
               (HASH form ((two-hand-articulation ?ville-1 "")
                           (two-hand-articulation ?different "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?beaucoup "")
                           (two-hand-articulation ?grand "")
                           (two-hand-articulation ?ville-2 "")
                           (adjacent ?ville-1 ?different)
                           (adjacent ?different ?un)
                           (adjacent ?un ?beaucoup)
                           (adjacent ?beaucoup ?grand)
                           (adjacent ?grand ?ville-2))))
              (?largest-city-in-?x\(?x\)
               (unit-cat largest-city-in-?x\(?x\)-cat)
               (args ((scope ?f)
                      (target ?b)))
               --
               (unit-cat largest-city-in-?x\(?x\)-cat)
               (boundaries ((right ?largest-city-in-?x\(?x\)-boundary-right)
                            (left ?largest-city-in-?x\(?x\)-boundary-left)))))
             :cxn-inventory *geoquery-lsfb*)


(loop for city in *geoquery-cities*
      for city-name = (cdr (assoc :name city))
      for city-meaning = (replace-spaces city-name :replacer "_")
      for fingerspelling-input = (replace-spaces city-name :replacer "-")
      for hamnosys = (make-fingerspelling fingerspelling-input)
      for construction-name = (read-from-string (format nil "~a-city-cxn-1" city-meaning))
      for city-fcg-tag = (read-from-string (format nil "?fs-~a" city-meaning))
      for city-unit-name = (read-from-string (format nil "?~a-unit" city-meaning))
      for city-category = (read-from-string (format nil "~a-cat" city-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,city-unit-name
                          (unit-cat cityid-cat)
                          (args ((scope ?f)
                                 (target ?d)))
                          (boundaries ((left ?nom)
                                       (right ,city-fcg-tag))))
                         <-
                         (,city-unit-name
                          (HASH meaning ((const ?f ?d ?g)
                                         (cityid ?g ?h ?_)
                                         (,(read-from-string city-meaning) ?h)))
                          --
                          (HASH form ((two-hand-articulation ?nom "")
                                      (right-hand-articulation ,city-fcg-tag ,hamnosys)
                                      (adjacent ?nom ,city-fcg-tag)))))
                        :cxn-inventory *geoquery-lsfb*)))