(in-package :geoquery-lsfb-grammar)

#|
(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (replace-spaces (cdr (assoc :name state)))
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-cxn-1" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,additional-signs-unit-name
                          (form ((two-hand-articulation ?palm-up "")
                                 (two-hand-articulation ?dans "")
                                 (adjacent ?palm-up ?dans)
                                 (adjacent ?dans ,state-fcg-tag))))
                         (,state-unit-name
                          (subunits (,additional-signs-unit-name))
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

|#

#|
(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (replace-spaces (cdr (assoc :name state)))
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-cxn-2" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,additional-signs-unit-name
                          (form ((two-hand-articulation ?pays "")
                                 (right-hand-articulation ?state-ds-right-hand "")
                                 (left-hand-articulation ?state-ds-left-hand "")
                                 (adjacent ?pays ,state-fcg-tag)
                                 (adjacent ,state-fcg-tag ?state-ds-right-hand)
                                 (during ?state-ds-right-hand ?state-ds-left-hand))))
                         (,state-unit-name
                          (subunits (,additional-signs-unit-name))
                          (unit-cat ,state-category)
                          (args ((scope ?e)
                                 (target ?c)))
                          (boundaries ((left ?pays)
                                       (right ?state-ds-right-hand))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((const ?e ?c ?f)
                                         (stateid ?f ?g)
                                         (,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb*)))


(loop for state in *geoquery-states-naming-signs*
      for id-gloss = (string-replace (cdr (assoc :id-gloss state)) ":" "\\:")
      for state-meaning = (replace-spaces (cdr (assoc :meaning state)))
      for hamnosys = (cdr (assoc :hamnosys state))
      for construction-name = (read-from-string (format nil "~a-cxn-3" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?~a" id-gloss))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,additional-signs-unit-name
                          (form ((two-hand-articulation ?pays "")
                                 (right-hand-articulation ?state-ds-right-hand "")
                                 (left-hand-articulation ?state-ds-left-hand "")
                                 (adjacent ?pays ,state-fcg-tag)
                                 (adjacent ,state-fcg-tag ?state-ds-right-hand)
                                 (during ?state-ds-right-hand ?state-ds-left-hand))))
                         (,state-unit-name
                          (subunits (,additional-signs-unit-name))
                          (unit-cat ,state-category)
                          (args ((scope ?e)
                                 (target ?c)))
                          (boundaries ((left ?pays)
                                       (right ?state-ds-right-hand))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((const ?e ?c ?f)
                                         (stateid ?f ?g)
                                         (,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb*)))
|#

(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (replace-spaces (cdr (assoc :name state)) :replacer "_")
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-state-cxn-1" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,additional-signs-unit-name
                          (form ((two-hand-articulation ?pays "")
                                 (right-hand-articulation ?state-ds-right-hand "")
                                 (left-hand-articulation ?state-ds-left-hand "")
                                 (right-hand-articulation ?state-pt "")
                                 (adjacent ?pays ?state-ds-right-hand)
                                 (adjacent ?state-ds-right-hand ?state-pt)
                                 (adjacent ?state-pt ,state-fcg-tag)
                                 (during ?state-ds-right-hand ?state-ds-left-hand)
                                 (during ?state-pt ?state-ds-left-hand)
                                 (during ,state-fcg-tag ?state-ds-left-hand))))
                         (,state-unit-name
                          (subunits (,additional-signs-unit-name))
                          (unit-cat stateid-cat)
                          (args ((scope ?e)
                                 (target ?c)))
                          (boundaries ((left ?pays)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((const ?e ?c ?f)
                                         (stateid ?f ?g)
                                         (,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb*)))

(loop for state in *geoquery-states-naming-signs*
      for id-gloss = (string-replace (cdr (assoc :id-gloss state)) ":" "\\:")
      for state-meaning = (replace-spaces (cdr (assoc :meaning state)) :replacer "_")
      for hamnosys = (cdr (assoc :hamnosys state))
      for handedness = (read-from-string (cdr (assoc :handedness state)))
      for construction-name = (read-from-string (format nil "~a-state-cxn-2" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?~a" id-gloss))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,additional-signs-unit-name
                          (form ((two-hand-articulation ?pays "")
                                 (left-hand-articulation ?state-ds-left-hand "")
                                 (adjacent ?pays ,state-fcg-tag)
                                 (adjacent ,state-fcg-tag ?state-ds-left-hand))))
                         (,state-unit-name
                          (subunits (,additional-signs-unit-name))
                          (unit-cat stateid-cat)
                          (args ((scope ?e)
                                 (target ?c)))
                          (boundaries ((left ?pays)
                                       (right ?state-ds-left-hand))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((const ?e ?c ?f)
                                         (stateid ?f ?g)
                                         (,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((,handedness ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb*)))










             