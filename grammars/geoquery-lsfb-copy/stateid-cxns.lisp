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

(def-fcg-cxn stateid-cxn
             ((?state-unit
               (footprints (identified))
               (args ((target ?g)
                      (scope ?f))))
              <-
              (?state-unit
               (args ((target ?g)))
               (HASH meaning ((stateid ?f ?g)))
               (sem-cat (identified-category state)
                        (identified yes))
               --
               (footprints (not identified))
               (syn-cat noun)))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1

(def-fcg-cxn const-?state-cxn\(palm-up\,dans\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (syn-cat noun)
               (footprints (not const))
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (adjacent ?palm-up ?dans)
                           (adjacent ?dans ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


; state: 2
; pt:  |  --> done
(def-fcg-cxn const-?state-cxn\(dans\,?pt\,?state\)
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
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation /)
                (palm-orientation )
                (location /)
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 2
;ds: _ | _ --> done
;pt:  |  --> done

(def-fcg-cxn const-?state-cxn\(?ds\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?pt-unit))))
              (?state-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
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
              (?pt-unit
               --
               (phonetic-components
                (handshape /)
                (finger-orientation /)
                (palm-orientation /)
                (location ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?ds-right ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt-1:  --> done
; pt-2:  --> done
; ds:  --> done

(def-fcg-cxn const-?state-cxn\(dans\,?pt-1\,?ds\,?state\,?pt-2\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit-2))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign)
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement )))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right)))
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))))
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
                           (adjacent ?dans ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?ds-left)
                           (adjacent ?ds-right ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign)
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _ --> done

(def-fcg-cxn const-?state-cxn\(?ds\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
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
               (sem-cat (identified-category state)
                        (identified yes))
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?ds-right ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


; state: 3

(def-fcg-cxn const-?state-cxn\(dans\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?state-unit
               (sem-cat (identified-category state)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (sem-cat (identified-category state)
                        (identified yes))
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

#|
(def-fcg-cxn const-?state-cxn\(dans\,?pt\,?state\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit ?ds-unit))
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
                           (adjacent ?pt-unit ?state-unit)
                           (adjacent ?state-unit ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)
|#


;state: 1
;pt-1:  --> done
;ds: _ --> done
;pt-2:  --> done

(def-fcg-cxn const-?state-cxn\(dans\,?pt\,?state\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
               (boundaries ((left ?state-left)
                            (right ?state-right))))
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-left)
                           (adjacent ?state-right ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 7
; city: 1
; ds: _ | _ |_ --> done
; handshape-right: 
; finger-orientation: /
; palm-orientation: /
; location-right: 
; movement-left: 
; handshape-left: 
; location-left: 

(def-fcg-cxn const-?state-or-city-cxn\(dans\,?ds\,?state-or-city\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-or-city-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-or-city-right-boundary))))
              (?state-or-city-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation /)
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation /)
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-or-city-unit
               (sem-cat (identified-category state-or-city)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?state-or-city-left-boundary)
                            (right ?state-or-city-right-boundary))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?ds-left)
                           (adjacent ?ds-right ?state-or-city-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 1
;pt-1: 
;pt-2: 

(def-fcg-cxn const-?state-cxn\(?pt\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?pt-unit-1 ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 6
; pt:  |  |  | 
; handshape: 
; finger-orientation: /
; palm-orientation: /
; location: /
; movement: /

(def-fcg-cxn const-?state-cxn\(?pt\,pays\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation /)
                (palm-orientation /)
                (location /)
                (movement /))
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
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 3
; ds: _ | _ +  | _ --> done
; handshape-right: /
; finger-orientation-right: /
; palm-orientation-right: 
; location-right: /
; movement-right: /
; handshape-left: /
; finger-orientation-left: /
; palm-orientation-left: /
; location-left: /
; movement-left: /

(def-fcg-cxn const-?state-cxn\(pays\,?ds\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (finger-orientation /)
                 (palm-orientation )
                 (location /))
                (non-dominant-hand
                 (palm-orientation /)
                 (location /)))
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
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?ds-left)
                           (adjacent ?ds-right ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 4
;pt:  |  --> done

(def-fcg-cxn const-?state-cxn\(pays\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              (?state-unit
                (footprints (const)))
              <-
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
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt-1:  --> done
; pt-2:  -- done

(def-fcg-cxn const-?state-cxn\(?pt\,pays\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              (?state-unit
                (footprints (const)))
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
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit-1 ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

#|
(def-fcg-cxn const-?state-cxn\(?pt\,pays\,?state\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
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
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

|#

; state: 1
; ds:  |  --> done
; pt:  --> done

(def-fcg-cxn const-?state-cxn\(palm-up\,dans\,pays\,?ds\,?pt\,nom\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?country-unit
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
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?palm-up ?dans)
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?ds-left)
                           (adjacent ?ds-right ?pt-unit)
                           (adjacent ?pt-unit ?nom)
                           (adjacent ?nom ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 2
; ds:  |  |  (lefthand) --> done
; handshape: 
; finger-orientation: /
; palm-orientation: /
; location: /
; movement:/ 

(def-fcg-cxn const-?state-cxn\(pays\,?state\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
              (?state-unit
                (footprints (const)))
              <-
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
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (palm-orientation /)
                 (location /)))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _ --> done

(def-fcg-cxn const-?state-cxn\(dans\,pays\,?state\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?state-unit
                (footprints (const)))
              <-
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
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; pt:  --> done

(def-fcg-cxn const-?state-cxn\(dans\,pays\,?state\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit))))
              (?state-unit
                (footprints (const)))
              <-
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
              (?pt-unit
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;state: 1

(def-fcg-cxn const-?state-cxn\(dans\,pays\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
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
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds: _ --> done

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
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
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
; pt:  --> done

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
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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

(def-fcg-cxn const-?state-cxn\(palm-up\,pays\,?state\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              (?state-unit
                (footprints (const)))
              <-
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
               (HASH form ((right-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
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
; pt-1:  -->done
; ds: _ -->done
; pt:  -->done

(def-fcg-cxn const-?state-cxn\(il-y-a\,?pt\,pays\,nom\,?state\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?state-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-2))))
              (?state-unit
                (footprints (const)))
              <-
              (?pt-unit-1
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
              (?ds-unit
               --
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
                           (adjacent ?nom ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; state: 1
; ds-1: _ --> done
; pt:  --> done
; pt-2:  --> done

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
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
               (phonetic-components
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-1-left)
                            (right ?ds-1-right))))
              (?pt-unit-2
               --
               (phonetic-components
                (handshape )
                (finger-orientation )
                (palm-orientation )
                (location )
                (movement ))
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
; ds: _ --> done
; pt:  |  --> done
; handshape: /
; finger-orientation:  
; palm-orienation: 
; location: /
; movement: /

(def-fcg-cxn const-?country-cxn\(dans\,?country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?country-unit ?pt-unit-1 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-1))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-1
               --
               (phonetic-components
                (handshape /)
                (finger-orientation )
                (palm-orientation )
                (location /))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit-1)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; country: 9
; ds: _ | _ | _ | _ | _ --> done
; handshape-right: 
; finger-orientation-right: /
; palm-orientation-right: /
; location-right: /
; movement-right: /
; handshape-left: 
; finger-orientation-left: /
; palm-orientation-left: /
; location-left: 
; movement-left: 


(def-fcg-cxn const-?country-cxn\(dans\,?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (palm-orientation /))
                (non-dominant-hand
                 (handshape )
                 (palm-orientation /)
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

; country: 2
; ds: _ | _ --> done
; handshape-right: 
; extended-finger-direction-right: /
; palm-orientation-right: 
; location-right: 
; movement-right: /
; handshape-left: 
; extended-finger-direction-left: /
; palm-orientation-left: 
; location-left: 
; movement-left: 

(def-fcg-cxn const-?country-cxn\(?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement /))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;country: 2
; ds: _ | _ --> done
; handshape-right: 
; extended-finger-direction-right: /
; palm-orientation-right: 
; location-right: /
; movement-right: |
; handshape-left: 
; extended-finger-direction-left: /
; palm-orientation-left: 
; location-left: 

(def-fcg-cxn const-?country-cxn\(palm-up\,pays\,?country\,?ds\)
             ((?const-unit
               (syn-cat np)
               (subunits (?country-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?ds-right))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location /)
                 (movement /))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation /)
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?country-left-boundary)
                           (adjacent ?country-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

;country: 1
; ds: _ --> done
; pt:  --> done

(def-fcg-cxn const-?country-cxn\(?country\,?ds\,?pt\)
             ((?const-unit
               (syn-cat np)
               (subunits (?country-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?country-left-boundary)
                            (right ?pt-unit))))
              (?country-unit
                (footprints (const)))
              <-
              (?country-unit
               (sem-cat (identified-category country)
                        (identified yes))
               (args ((scope ?f)))
               (footprints (not const))
               (meaning-pred-type full-name)
               --
               (meaning-pred-type full-name)
               (footprints (not const))
               (syn-cat noun)
               (boundaries ((left ?country-left-boundary)
                            (right ?country-right-boundary))))
              (?ds-unit
               --
               (phonetic-components 
                (dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
                (non-dominant-hand
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement )))
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
               (?pt-unit
               --
               (phonetic-components
                 (handshape )
                 (finger-orientation )
                 (palm-orientation )
                 (location )
                 (movement ))
               (syn-cat pointing-sign))
              (?const-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((adjacent ?country-right-boundary ?ds-left)
                           (adjacent ?ds-right ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

