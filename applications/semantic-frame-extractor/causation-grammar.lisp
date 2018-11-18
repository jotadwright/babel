;;;;; Grammatical constructions for the CAUSATION-frames of The Guardian Climate Change Corpus
;;;;; Revised version after switch to hybrid approach, June 2018
;;;;; Katrien Beuls (katrien@ai.vub.ac.be)
;;;;; ----------------------------------------------------------------------------------------

(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General argument structure constructions
;;-----------------------------------------------------------------


(def-fcg-cxn active-transitive-actor-theme-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?subject
               (referent ?x)
               --
               (dependency (edge nsubj))
               (head ?vp))
              (?object
               (referent ?y)
               --
               (dependency (edge dobj))
               (head ?vp))
              (?vp
               --
               (referent ?ev)
               (sem-valence (actor ?x)
                            (theme ?y))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject)
                            (object ?object))
               (footprints (not arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)

(def-fcg-cxn active-transitive-actor-theme-cxn-subject-parataxis
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?vp
               --
               (head ?same-head)
               (referent ?ev)
               (sem-valence (actor ?x)
                            (theme ?y))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject)
                            (object ?object))
               (footprints (not arg-structure)))
              (?subject
               (referent ?x)
               --
               (dependency (edge nsubj))
               (head ?same-head))
              (?object
               (referent ?y)
               --
               (dependency (edge dobj))
               (head ?vp))
              )
             :disable-automatic-footprints t
             :cxn-set unhashed)

(def-fcg-cxn intransitive-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?subject
               (referent ?x)
               (dependency (edge nsubj))
               --
               (dependency (edge nsubj))
               (head ?vp))
              (?vp
               --
               (referent ?ev)
               (sem-valence (actor ?x))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject))
               (footprints (not arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn passive-transitive-cxn
             ((?vp-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?subject-unit
               (referent ?x)
               --
               (dependency (edge nsubjpass))
               (head ?vp-unit))
              (?vp-unit
               --
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (dependency (pos-tag vbn))
               (syn-valence (subject ?subject-unit))
               (footprints (not arg-structure)))
              (?by
               --
               (dependency (edge agent))
               (head ?vp-unit))
              (?oblique-unit
               (referent ?y)
               --
               (dependency (edge pobj))
               (head ?by)))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X is caused by Y")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame specific constructions for linking frame slots to units
;;-----------------------------------------------------------------

(def-fcg-cxn meta-causation=cause-cxn
             ((?vp
               (footprints (frame-cause)))
              (?cause
               (footprints (frame-cause)))
              <-
              (?vp
               --
               (referent ?ev)
               (meaning ((slot cause ?frame ?x)))
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              (?to
               --
               (head ?vp)
               (dependency (pos-tag in))
               (dependents (?cause))
               )
              (?cause
               (referent ?x)
               --
               (footprints (not frame-cause))
               (head ?to)
               (dependency (pos-tag nn))))
             :disable-automatic-footprints t
             :cxn-set unhashed
             )

(def-fcg-cxn X-caused-by-Y
             (
              <-
              (?caused-unit
               (referent ?frame)
               --
               (sem-cat (frame causation))
               (meaning ((slot cause ?frame ?cause)
                         (slot effect ?frame ?effect)))
               (dependency (pos-tag vbn)
                           (edge acl)) 
               (head ?effect-unit)
               (dependents (?by-unit)))
              (?by-unit
               --
               (head ?caused-unit)
               (dependents (?cause-unit))
               (form ((string ?by-unit "by"))))
              (?cause-unit
               (referent ?cause)
               --
               (head ?by-unit)
               (dependency (edge pobj)))
              (?effect-unit
               (referent ?effect)
               --
               (dependents (?caused-unit)))
              )
             :cxn-set unhashed)

