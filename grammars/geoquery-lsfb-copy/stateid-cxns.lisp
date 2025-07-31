(in-package :geoquery-lsfb-grammar-copy)


(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (replace-spaces (cdr (assoc :name state)) :replacer "_")
      for state-abbreviation = (cdr (assoc :abbreviation state))
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-cxn-1" state-meaning))
      for construction-name-2 = (read-from-string (format nil "~a-cxn-2" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      for sem-categories = (append '(state)(determine-additional-categories state-meaning :type 'state))
      for possible-category-len = (length sem-categories)
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,state-unit-name
                          (syn-cat noun)
                          (meaning-pred-type full-name)
                          ,(if (eql possible-category-len 1)
                             `(sem-cat
                               (identified-category state)
                               (identified yes))
                             `(sem-cat
                               (possible-categories ,sem-categories)))
                          (footprints
                           ,(if (eql possible-category-len 1)
                                      `(identified)
                                      `()))
                          (args
                           ,(if (eql possible-category-len 1)
                                      `((target ?g)
                                        (scope ?f))
                                      `((target ?g))))
                          (boundaries ((left ,state-fcg-tag)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning
                                ,(if (eql possible-category-len 1)
                                   `((,(read-from-string state-meaning) ?g)
                                     (stateid ?f ?g))
                                   `((,(read-from-string state-meaning) ?g))))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb-copy*))
         (eval
          `(def-fcg-cxn ,construction-name-2
                        ((,state-unit-name
                          (syn-cat noun)
                          (meaning-pred-type abbreviation)
                          (sem-cat
                           (identified-category state)
                           (identified yes))
                          (footprints
                           (identified))
                          (args
                           ((target ?g)))
                          (boundaries ((left ,state-fcg-tag)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning
                                ((,(read-from-string state-abbreviation) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb-copy*)))
         


(loop for state in *geoquery-states-naming-signs*
      for id-gloss = (string-replace (cdr (assoc :id-gloss state)) ":" "\\:")
      for state-meaning = (replace-spaces (cdr (assoc :meaning state)) :replacer "_")
      for hamnosys = (cdr (assoc :hamnosys state))
      for construction-name = (read-from-string (format nil "~a-cxn-2" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?~a" id-gloss))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      for sem-categories = (append '(state)(determine-additional-categories state-meaning :type 'state))
      for possible-category-len = (length sem-categories)
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,state-unit-name
                          (syn-cat noun)
                          ,(if (eql possible-category-len 1)
                             `(sem-cat
                               (identified-category state)
                               (identified yes))
                             `(sem-cat
                               (possible-categories ,sem-categories)))
                          (meaning-pred-type full-name)
                          (footprints
                           ,(if (eql possible-category-len 1)
                                      `(identified)
                                      `()))
                          (args
                           ,(if (eql possible-category-len 1)
                                      `((target ?g)
                                        (scope ?f))
                                      `((target ?g))))
                          (boundaries ((left ,state-fcg-tag)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning
                                ,(if (eql possible-category-len 1)
                                   `((,(read-from-string state-meaning) ?g)
                                     (stateid ?f ?g))
                                   `((,(read-from-string state-meaning) ?g))))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb-copy*)))


(def-fcg-cxn new-mexico-cxn-2
             ((?new-mexico-unit
               (syn-cat noun)
               (sem-cat (identified-category state)
                        (identified yes))
               (footprints (identified))
               (meaning-pred-type full-name)
               (args ((scope ?f)
                      (target ?g)))
               (boundaries ((left ?nouveau)
                            (right ?ns-mexique))))
              <-
              (?new-mexico-unit
               (HASH meaning ((new_mexico ?g)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?nouveau "")
                           (right-hand-articulation ?ns-mexique "")
                           (adjacent ?nouveau ?ns-mexique)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn usa-cxn-1
             ((?usa-unit
               (syn-cat noun)
               (sem-cat (identified-category country)
                        (identified yes))
               (footprints (identified))
               (meaning-pred-type full-name)
               (args ((scope ?f)
                      (target ?g)))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (HASH meaning ((usa ?g)
                              (countryid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn usa-cxn-2
             ((?usa-unit
               (syn-cat noun)
               (sem-cat (identified-category country)
                        (identified yes))
               (footprints (identified))
               (meaning-pred-type full-name)
               (args ((scope ?f)
                      (target ?g)))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (HASH meaning ((usa ?g)
                              (countryid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn usa-cxn-3
             ((?usa-unit
               (syn-cat noun)
               (meaning-pred-type none)
               (footprints (identified))
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (lex-id amerique)
               --
               (HASH form ((right-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn usa-cxn-4
             ((?usa-unit
               (syn-cat noun)
               (footprints (identified))
               (meaning-pred-type none)
               (boundaries ((left ?ns-amerique)
                            (right ?ns-amerique))))
              <-
              (?usa-unit
               (lex-id amerique)
               --
               (HASH form ((two-hand-articulation ?ns-amerique "")))))
             :cxn-inventory *geoquery-lsfb-copy*)

#|
(def-fcg-cxn des-moines-cxn-2
             ((?des-moines-unit
               (syn-cat noun)
               (sem-cat (state))
               (args ((target ?g)))
               (boundaries ((left ?des)
                            (right ?moine))))
              <-
              (?des-moines-unit
               (HASH meaning ((des_moines ?g)))
               --
               (HASH form ((two-hand-articulation ?des "")
                           (right-hand-articulation ?moine "")
                           (adjacent ?des ?moine)))))
             :cxn-inventory *geoquery-lsfb-copy*)
|#

(def-fcg-cxn stateid-cxn
             ((?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (footprints (identified))
               (args ((target ?g)
                      (scope ?f))))
              <-
              (?state-unit
               (sem-cat (possible-categories (state)))
               (args ((target ?g)))
               (HASH meaning ((stateid ?f ?g)))
               --
               (footprints (not identified))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1

(def-fcg-cxn const-?state-or-country-cxn\(palm-up\,dans\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (adjacent ?palm-up ?dans)
                           (adjacent ?dans ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


; state: 2
; pt:  | 
(def-fcg-cxn const-?state-or-country-cxn\(dans\,?pt\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-or-country-right-boundary))))
               (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 2
;ds: _ | _
;pt:  | 

(def-fcg-cxn const-?state-or-country-cxn\(?ds\,?state-or-country\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?pt-unit))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?ds-right ?state-or-country-left-boundary) ;right of ds with left of state
                           (adjacent ?state-or-country-right-boundary ?pt-unit) ; right of state with left of pt
                           ))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt-1: 
; pt-2: 
; ds: 

(def-fcg-cxn const-?state-or-country-cxn\(dans\,?pt-1\,?ds\,?state-or-country\,?pt-2\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?pt-unit-2 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit-2))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?ds-left)
                           (adjacent ?ds-right ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?pt-unit-2))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign)))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _

(def-fcg-cxn const-?state-or-country-cxn\(?ds\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?ds-right ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


; state: 3

(def-fcg-cxn const-?state-or-country-cxn\(dans\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn const-?state-or-country-cxn\(dans\,?pt\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-or-country-unit)
                           (adjacent ?state-or-country-unit ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 1
;pt-1: 
;ds: _
;pt-2: 

(def-fcg-cxn const-?state-or-country-cxn\(dans\,?pt\,?state-or-country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left)
                            (right ?state-or-country-right))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-or-country-left)
                           (adjacent ?state-or-country-right ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 7
; city: 1
;ds: _ | _ |_
(def-fcg-cxn const-?state-country-or-city-cxn\(dans\,?ds\,?state-country-or-city\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-country-or-city-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-country-or-city-right-boundary))))
              (?state-country-or-city-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-country-or-city-unit
               (sem-cat (identified-category state-country-or-city)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-country-or-city-left-boundary)
                            (right ?state-country-or-city-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?ds-left)
                           (adjacent ?ds-right ?state-country-or-city-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 1
;pt-1: 
;pt-2: 

(def-fcg-cxn const-?state-or-country-cxn\(?pt\,?state-or-country\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?pt-unit-1 ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 6
; pt:  |  |  | 

(def-fcg-cxn const-?state-or-country-cxn\(?pt\,pays\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 3
; ds: _ | _ +  | _

(def-fcg-cxn const-?state-or-country-cxn\(pays\,?ds\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?ds-left)
                           (adjacent ?ds-right ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 4
;pt:  | 

(def-fcg-cxn const-?state-or-country-cxn\(pays\,?state-or-country\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt-1: 
; pt-2: 

(def-fcg-cxn const-?state-or-country-cxn\(?pt\,pays\,?state-or-country\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit-1 ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn const-?state-or-country-cxn\(?pt\,pays\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds:  | 
; pt: 

(def-fcg-cxn const-?state-or-country-cxn\(palm-up\,dans\,pays\,?ds\,?pt\,nom\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?palm-up ?dans)
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?ds-left)
                           (adjacent ?ds-right ?pt-unit)
                           (adjacent ?pt-unit ?nom)
                           (adjacent ?nom ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 2
; ds:  |  |  (lefthand)

(def-fcg-cxn const-?state-or-country-cxn\(pays\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _

(def-fcg-cxn const-?state-or-country-cxn\(dans\,pays\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt: 

(def-fcg-cxn const-?state-or-country-cxn\(dans\,pays\,?state-or-country\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 1

(def-fcg-cxn const-?state-or-country-cxn\(dans\,pays\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _

(def-fcg-cxn const-?state-cxn\(dans\,?ds\,il-y-a\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?dans ?ds-left)
                           (adjacent ?ds-right ?il-y-a)
                           (adjacent ?il-y-a ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt: 

(def-fcg-cxn const-?state-cxn\(dans\,us\,il-y-a\,un\,?pt\,pays\,nom\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?un "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?un)
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?nom)
                           (adjacent ?nom ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1

(def-fcg-cxn const-?state-or-country-cxn\(palm-up\,pays\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((right-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 2
; country: 1

(def-fcg-cxn const-?state-or-country-cxn\(pays\,?state-or-country\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-or-country-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 2
; pt-1: 
; ds: _
; pt: 

(def-fcg-cxn const-?state-or-country-cxn\(il-y-a\,?pt\,pays\,nom\,?state-or-country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-2))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?il-y-a ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?pays)
                           (adjacent ?pays ?nom)
                           (adjacent ?nom ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds-1: _
; pt: 
; pt-2: 

(def-fcg-cxn const-?state-cxn\(il-y-a\,different\,pays\,?ds\,un\,?pt\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit ?ds-unit-1 ?ds-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-2))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit-1
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-1-left)
                            (right ?ds-1-right))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?un "")
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?ds-1-left)
                           (adjacent ?ds-1-right ?un)
                           (adjacent ?un ?pt-unit)
                           (adjacent ?pt-unit ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; country: 2
; ds: _
; pt:  | 

(def-fcg-cxn const-?state-or-country-cxn\(dans\,?state-or-country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?pt-unit-1 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-1))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; country: 9
; ds: _ | _ | _ | _ | _

(def-fcg-cxn const-?state-or-country-cxn\(dans\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; country: 2
; ds: _ | _

(def-fcg-cxn const-?state-or-country-cxn\(?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;country: 2
; ds: _ | _

(def-fcg-cxn const-?state-or-country-cxn\(palm-up\,pays\,?state-or-country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?ds-right))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?state-or-country-left-boundary)
                           (adjacent ?state-or-country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;country: 1
; ds: _
; pt: 

(def-fcg-cxn const-?state-or-country-cxn\(?state-or-country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-country-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?pt-unit))))
              (?state-or-country-unit
                (footprints (const)))
              <-
              (?state-or-country-unit
               (sem-cat (identified-category state-or-country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-country-left-boundary)
                            (right ?state-or-country-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
               (?pt-unit
               --
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?state-or-country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

