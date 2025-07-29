(in-package :geoquery-lsfb-grammar-copy)


(loop for state in *geoquery-states*
      for french-state-name = (cdr (assoc :french state))
      for state-meaning = (replace-spaces (cdr (assoc :name state)) :replacer "_")
      for hamnosys = (make-fingerspelling french-state-name)
      for construction-name = (read-from-string (format nil "~a-cxn-1" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?fs-~a" french-state-name))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,state-unit-name
                          ;(unit-cat ,state-category)
                          (syn-cat noun)
                          (sem-cat place)
                          (args ((target ?g)))
                          (boundaries ((left ,state-fcg-tag)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb-copy*))
      (add-category state-category (categorial-network *geoquery-lsfb-copy*))
      (add-link state-category 'stateid-?x\(?x\)-cat (categorial-network *geoquery-lsfb-copy*)))
         


(loop for state in *geoquery-states-naming-signs*
      for id-gloss = (string-replace (cdr (assoc :id-gloss state)) ":" "\\:")
      for state-meaning = (replace-spaces (cdr (assoc :meaning state)) :replacer "_")
      for hamnosys = (cdr (assoc :hamnosys state))
      for construction-name = (read-from-string (format nil "~a-cxn-2" state-meaning))
      for state-fcg-tag = (read-from-string (format nil "?~a" id-gloss))
      for state-unit-name = (read-from-string (format nil "?~a-unit" state-meaning))
      for state-category = (read-from-string (format nil "~a-cat" state-meaning))
      for additional-signs-unit-name = (read-from-string (format nil "?~a-optional-signs-unit" state-meaning))
      do (eval
          `(def-fcg-cxn ,construction-name
                        ((,state-unit-name
                          (syn-cat noun)
                          (sem-cat place)
                          (args ((target ?g)))
                          (boundaries ((left ,state-fcg-tag)
                                       (right ,state-fcg-tag))))
                         <-
                         (,state-unit-name
                          (HASH meaning ((,(read-from-string state-meaning) ?g)))
                          --
                          (HASH form ((right-hand-articulation ,state-fcg-tag ,hamnosys)))))
                        :cxn-inventory *geoquery-lsfb-copy*)))


(def-fcg-cxn new-mexico-cxn-2
             ((?new-mexico-unit
               (syn-cat noun)
               (sem-cat place)
               (args ((target ?g)))
               (boundaries ((left ?nouveau)
                            (right ?ns-mexique))))
              <-
              (?new-mexico-unit
               (HASH meaning ((new_mexico ?g)))
               --
               (HASH form ((two-hand-articulation ?nouveau "")
                           (right-hand-articulation ?ns-mexique "")
                           (adjacent ?nouveau ?ns-mexique)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn des-moines-cxn-2
             ((?des-moines-unit
               (syn-cat noun)
               (sem-cat place)
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

(def-fcg-cxn stateid-cxn\(palm-up\,dans\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat state)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (adjacent  ?dans)
                           (adjacent ?dans ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,pt\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(ds\,state\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?pt-unit))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((adjacent ?ds-right ?state-left-boundary) ;right of ds with left of state
                           (adjacent ?state-right-boundary ?pt-unit) ; right of state with left of pt
                           ))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,pt-1\,ds\,state\,pt-2\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2 ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit-2))))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit-1)
                           (adjacent ?pt-unit-1 ?ds-left)
                           (adjacent ?ds-right ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign)))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(ds\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?ds-left)
                            (right ?state-right-boundary))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((adjacent ?ds-right ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(dans\,pt\,state\,ds\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-unit)
                           (adjacent ?state-unit ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,pt\,state\,ds\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left)
                            (right ?state-right))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?pt-unit)
                           (adjacent ?pt-unit ?state-left)
                           (adjacent ?state-right ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,ds\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?ds-left)
                           (adjacent ?ds-right ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(pt\,state\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit-1)
                            (right ?pt-unit-2))))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((adjacent ?pt-unit-1 ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(pt\,pays\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-unit)
                            (right ?state-right-boundary))))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(pays\,ds\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-right-boundary))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?ds-left)
                           (adjacent ?ds-right ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(pays\,state\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(pt\,pays\,state\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit-1 ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?pt-unit))))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit-1 ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(pt\,pays\,state\,ds\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pt-unit ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(palm-up\,dans\,pays\,ds\,pt\,nom\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
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

(def-fcg-cxn stateid-cxn\(pays\,state\,ds\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?ds-right))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,pays\,state\,ds\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?ds-right))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?ds-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(dans\,pays\,state\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-unit))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)
                           (adjacent ?state-right-boundary ?pt-unit)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,pays\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-cxn\(dans\,ds\,il-y-a\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?ds-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?il-y-a "")
                           (adjacent ?dans ?ds-left)
                           (adjacent ?ds-right ?il-y-a)
                           (adjacent ?il-y-a ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(dans\,us\,il-y-a\,un\,pt\,pays\,nom\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?state-right-boundary))))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
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


(def-fcg-cxn stateid-cxn\(palm-up\,pays\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(pays\,state\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-right-boundary))))
              <-
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?state-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-cxn\(il-y-a\,pt\,pays\,nom\,state\,ds\,pt\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit-1 ?ds-unit ?pt-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-unit-2))))
              <-
              (?pt-unit-1
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-left)
                            (right ?ds-right))))
              (?pt-unit-2
               --
               (syn-cat pointing-sign))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
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


(def-fcg-cxn stateid-cxn\(il-y-a\,different\,pays\,ds\,un\,pt\,state\,ds\)
             ((?stateid-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?state-unit ?pt-unit ?ds-unit-1 ?ds-unit-2))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?ds-2-right))))
              <-
              (?pt-unit
               --
               (syn-cat pointing-sign))
              (?state-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?state-left-boundary)
                            (right ?state-right-boundary))))
              (?ds-unit-1
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-1-left)
                            (right ?ds-1-right))))
              (?ds-unit-2
               --
               (syn-cat depicting-sign)
               (boundaries ((left ?ds-2-left)
                            (right ?ds-2-right))))
              (?stateid-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
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
                           (adjacent ?state-right-boundary ?ds-2-left)))))
             :cxn-inventory *geoquery-lsfb-copy*)

#|

(def-fcg-cxn stateid-?x-cxn-13
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (right-hand-articulation ?state-pt "")
                           (left-hand-articulation ?state-ds "")
                           (adjacent ?pays ?state-ds)
                           (adjacent ?state-pt ?stateid-?x\(?x\)-left-boundary)
                           (during ?state-pt ?state-ds)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-14
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?palm-up "")
                           (two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?state-pt-1 "")
                           (right-hand-articulation ?state-pt-2 "")
                           (left-hand-articulation ?map-ds "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?palm-up ?dans)
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?state-pt-1)
                           (adjacent ?state-pt-1 ?state-pt-2)
                           (during ?state-pt-1 ?map-ds)
                           (during ?state-pt-2 ?map-ds)
                           (adjacent ?state-pt-2 ?nom)
                           (adjacent ?nom ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-15
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?state-ds-1))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (right-hand-articulation ?state-ds-1 "")
                           (left-hand-articulation ?state-ds-2 "")
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?state-ds-1)
                           (during ?state-ds-1 ?state-ds-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-16
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (adjacent ?dans ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-17
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-state)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?pt-state "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?pt-state ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-18
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?pt-state "")
                           (left-hand-articulation ?map-ds "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?pt-state)
                           (during ?pt-state ?map-ds)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-19
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (right-hand-articulation ?state-ds-1 "")
                           (right-hand-articulation ?pt-state "")
                           (left-hand-articulation ?state-ds-2 "")
                           (adjacent ?pays ?state-ds-1)
                           (adjacent ?state-ds-1 ?pt-state)
                           (adjacent ?pt-state ?stateid-?x\(?x\)-left-boundary)
                           (during ?state-ds-1 ?state-ds-2)
                           (during ?pt-state ?state-ds-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-20
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?pt-state "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?pt-state)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-21
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?pt-state))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (right-hand-articulation ?pt-state "")
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?pt-state)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-?x-cxn-22
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?states-ds "")
                           (right-hand-articulation ?il-y-a "")
                           (left-hand-articulation ?map-ds "")
                           (adjacent ?dans ?states-ds)
                           (adjacent ?states-ds ?il-y-a)
                           (adjacent ?il-y-a ?stateid-?x\(?x\)-left-boundary)
                           (during ?states-ds ?map-ds)
                           (during ?il-y-a ?map-ds)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-23
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (left-hand-articulation ?ds-state "")
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?ds-state)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-?x-cxn-24
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?dans ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-25
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pt-state)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?pt-state "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?pt-state ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-26
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (right-hand-articulation ?pt-state "")
                           (left-hand-articulation ?state-ds "")
                           (adjacent ?pays ?pt-state)
                           (adjacent ?pt-state ?stateid-?x\(?x\)-left-boundary)
                           (during ?pt-state ?state-ds)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-27
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (two-hand-articulation ?usa "")
                           (right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (adjacent ?dans ?usa)
                           (adjacent ?usa ?il-y-a)
                           (adjacent ?il-y-a ?un)
                           (adjacent ?un ?pt-state)
                           (adjacent ?pt-state ?pays)
                           (adjacent ?pays ?nom)
                           (adjacent ?nom ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-?x-cxn-28
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?state-pt)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?state-pt "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?state-pt ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-29
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?palm-up)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?palm-up "")
                           (two-hand-articulation ?pays "")
                           (adjacent ?palm-up ?pays)
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-30
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?pays)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?pays "")
                           (adjacent ?pays ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-31
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (right-hand-articulation ?pt-state-1 "")
                           (two-hand-articulation ?pays "")
                           (two-hand-articulation ?nom "")
                           (right-hand-articulation ?ds-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (left-hand-articulation ?ds-state-2 "")
                           (adjacent ?il-y-a ?pt-state-1)
                           (adjacent ?pt-state-1 ?pays)
                           (adjacent ?pays ?nom)
                           (adjacent ?nom ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?ds-state-1)
                           (adjacent ?ds-state-1 ?pt-state-2)
                           (during ?ds-state-1 ?ds-state-2)
                           (during ?pt-state-2 ?ds-state-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)

(def-fcg-cxn stateid-?x-cxn-32
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?il-y-a)
                            (right ?pt-state-2))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?il-y-a "")
                           (two-hand-articulation ?different "")
                           (two-hand-articulation ?pays "")
                           (right-hand-articulation ?ds-state-1 "")
                           (left-hand-articulation ?ds-state-2 "")
                           (right-hand-articulation ?un "")
                           (right-hand-articulation ?pt-state-1 "")
                           (right-hand-articulation ?pt-state-2 "")
                           (adjacent ?il-y-a ?different)
                           (adjacent ?different ?pays)
                           (adjacent ?pays ?ds-state-1)
                           (adjacent ?ds-state-1 ?un)
                           (adjacent ?un ?pt-state-1)
                           (adjacent ?pt-state-1 ?stateid-?x\(?x\)-left-boundary)
                           (adjacent ?stateid-?x\(?x\)-right-boundary ?pt-state-2)
                           (during ?ds-state-1 ?ds-state-2)
                           (during ?un ?ds-state-2)))))
             :cxn-inventory *geoquery-lsfb-copy*)


(def-fcg-cxn stateid-?x-cxn-33
             ((?stateid-?x-unit
               (syn-cat np)
               (sem-cat place)
               (subunits (?stateid-?x\(?x\)-unit))
               (args ((scope ?e)
                      (target ?c)))
               (boundaries ((left ?dans)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              <-
              (?stateid-?x\(?x\)-unit
               (sem-cat place)
               (args ((target ?g)))
               --
               (syn-cat noun)
               (boundaries ((left ?stateid-?x\(?x\)-left-boundary)
                            (right ?stateid-?x\(?x\)-right-boundary))))
              (?stateid-?x-unit
               (HASH meaning ((const ?e ?c ?f)
                              (stateid ?f ?g)))
               --
               (HASH form ((two-hand-articulation ?dans "")
                           (right-hand-articulation ?pt-state "")
                           (adjacent ?dans ?pt-state)
                           (adjacent ?pt-state ?stateid-?x\(?x\)-left-boundary)))))
             :cxn-inventory *geoquery-lsfb-copy*)

|#