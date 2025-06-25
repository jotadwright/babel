(in-package :slp)

(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (cdr (assoc :name state))
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-cxn-1" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((?in-unit
                          (form ((two-hand-articulation ?palm-up "")
                                 (two-hand-articulation ?dans "")
                                 (adjacent ?palm-up ?dans)
                                 (adjacent ?dans ,state-fcg-tag))))
                         (,state-unit-name
                          (subunits (?in-unit))
                          (unit-cat ,state-category)
                          (args ((scope ?e)
                                 (target ?c)))
                          (boundaries ((left ?palm-up)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((const ?e ?c ?f)
                                         (stateid ?f ?g)
                                         (,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb*)))
             