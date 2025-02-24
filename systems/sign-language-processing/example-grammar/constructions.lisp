(in-package :slp)



(def-fcg-cxn fs_mississippi-cxn-1
             ((?fs_mississippi-unit
               (sem-class state)
               (sign-class noun)
               (number sg)
               (args ((target ?f))))
              <-
              (?fs_mississippi-unit
               (HASH meaning ((mississippi ?g)
                              (stateid ?f ?g)))
               --
               (HASH form ((right-hand-articulation ?fs_mississippi-unit ""))))))



(def-fcg-cxn ds_bent5_etat-cxn-1
             ((?ds_bent5_etat-unit
               (sem-class state)
               (sign-class classifier)
               (location left)
               (number sg)
               (args ((target ?c)
                      (source ?f)
                      (scope ?e))))
              <-
              (?ds_bent5_etat-unit
               (HASH meaning ((const ?e ?c ?f)))
               --
               (HASH form ((left-hand-articulation ?ds_bent5_etat-unit ""))))))


(def-fcg-cxn noun-classifier-cxn-1
             ((?classifier-np-unit
               (subunits (?classifier-unit ?noun-unit))
               (args ((target ?c)
                      (scope ?e)))
               (phrase-type np)
               (location ?location)
               (sem-class entity)
               (composing-units ((noun-unit ?noun-unit)
                                 (classifier-unit ?classifier-unit))))
              --
              (?classifier-unit
               (sem-class ?class)
               (args ((target ?c)
                      (source ?f)
                      (scope ?e)))
               (number ?number)
               --
               (number ?number)
               (location ?location)
               (sign-class classifier))
              (?noun-unit
               (args ((target ?f)))
               (sem-class ?class)
               --
               (number ?number)
               (sign-class noun))))

(def-fcg-cxn ds_bent5_etat+-np-cxn-1
             ((?ds_bent5_etat+-unit
               (args ((target ?b)
                      (scope ?e)))
               (number pl)
               (sem-class entity)
               (location neutral)
               (phrase-type np))
              <-
              (?ds_bent5_etat+-unit
               (HASH meaning ((state ?e ?b)))
               --
               (HASH form ((right-hand-articulation ?ds_bent5_etat+-unit ""))))))

(def-fcg-cxn figure-adjacent-to-ground-cxn-1
             ((?figure-ground-unit
               (subunits (?figure-classifier-unit ?ground-unit))
               (args ((target ?b)
                      (scope ?e)))
               (phrase-type declarative)
               (boundaries ((rh-left-boundary ?palm-up-unit)
                            (rh-right-boundary ?figure-classifier-unit))))
              <-
              (?figure-ground-unit
               (HASH meaning ((next-to ?e ?b ?c)))
               --
               (HASH form ((two-hand-articulation ?palm-up-unit "")
                           (two-hand-articulation ?dans-unit "")
                           (right-hand-articulation ?il-y-a-unit "")
                           (adjacent ?palm-up-unit ?dans-unit)
                           (adjacent ?dans-unit ?ground-noun-unit)
                           (adjacent ?ground-noun-unit ?il-y-a-unit)
                           (adjacent ?il-y-a-unit ?ground-classifier-unit)
                           (adjacent ?il-y-a-unit ?figure-classifier-unit)
                           (start-coincides ?figure-classifier-unit ?ground-classifier-unit))))
              (?ground-unit
               (args ((target ?c)
                      (scope ?e)))
               (sem-class entity)
               --
               (phrase-type np)
               (location ?ground-location)
               (composing-units ((noun-unit ?ground-noun-unit)
                                 (classifier-unit ?ground-classifier-unit))))
              (?figure-classifier-unit
               (args ((target ?b)
                      (scope ?e)))
               (sem-class entity)
               --
               (location ?figure-location)
               (phrase-type np))))


(def-fcg-cxn pt_loc_1_+-anaphor-cxn
             ((?pt_loc_1_+-unit
               (location neutral)
               (number pl))
              <-
              (?antecedent-unit
               (args ((target ?b)
                      (scope ?e)))
               --
               (phrase-type np)
               (location neutral)
               (number pl))
              (?pt_loc_1_+-unit
               (args ((target ?b)
                      (scope ?e)))
               (sign-class pronoun)
               (sem-class entity)
               --
               (HASH form ((right-hand-articulation ?pt_loc_1_+-unit ""))))))

(def-fcg-cxn haut-cxn
             ((?haut-unit
               (args ((source ?b)
                      (target ?a)
                      (scope ?e)))
               (sign-class adjective)
               (sem-class property))
              <-
              (?haut-unit
               (HASH meaning ((high-point ?e ?b ?a)))
               --
               (HASH form ((right-hand-articulation ?haut-unit ""))))))

(def-fcg-cxn pronoun-property-question-cxn-1
             ((?question-unit
               (args ((source ?b)
                      (scope ?e)))
               (subunits (?pronoun-unit ?property-unit))
               (phrase-type interrogative)
               (boundaries ((rh-left-boundary ?pronoun-unit)
                            (rh-right-boundary ?quoi-unit))))
              (?pronoun-unit
               (args ((target ?b)
                      (scope ?e)))
               (sign-class pronoun)
               (sem-class entity))
              <-
              (?question-unit
               (HASH meaning ((answer ?d ?a ?e)))
               --
               (HASH form ((right-hand-articulation ?quoi-unit "")
                              (adjacent ?pronoun-unit ?property-unit)
                              (adjacent ?property-unit ?quoi-unit))))
              (?pronoun-unit
               --
               (sign-class pronoun))
              (?property-unit
               (args ((source ?b)
                      (target ?a)
                      (scope ?e)))
               --
               (sign-class adjective))))


(def-fcg-cxn theme-question-cxn-1
             ((?theme-question-unit
               (subunits (?theme-unit ?question-unit)))
              <-
              (?theme-question-unit
               --
               (HASH form ((adjacent ?theme-rightmost-unit ?question-leftmost-unit))))
              (?theme-unit
               (args ((target ?b)
                      (scope ?e)))
               (phrase-type declarative)
               --
               (phrase-type declarative)
               (boundaries ((rh-right-boundary ?theme-rightmost-unit))))
              (?question-unit
               (args ((source ?b)
                      (scope ?e)))
               (phrase-type interrogative)
               --
               (phrase-type interrogative)
               (boundaries ((rh-left-boundary ?question-leftmost-unit))))))
               
                     

               
               



