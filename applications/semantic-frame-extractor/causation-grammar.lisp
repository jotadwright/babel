;;;;; Grammatical constructions for the CAUSATION-frames of The Guardian Climate Change Corpus
;;;;; Revised version after switch to hybrid approach, June 2018
;;;;; Katrien Beuls (katrien@ai.vub.ac.be)
;;;;; ----------------------------------------------------------------------------------------

(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General argument structure constructions
;;-----------------------------------------------------------------


(def-fcg-cxn partial-active-actor-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (actor-arg-structure)))
              <-
              (?subject
               (referent ?x)
               --
               (dependency (edge nsubj))
               (head ?vp))
              (?vp
               --
               (referent ?ev)
               (sem-valence (actor ?x))
               (syn-cat (not (voice passive)))
               (syn-valence (subject ?subject))
               (footprints (not actor-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn partial-active-transitive-theme-cxn
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (theme-arg-structure)))
              <-
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
               (footprints (not theme-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed)


(def-fcg-cxn active-transitive-actor-theme-cxn-subject-parataxis
             ((?vp
               (syn-cat (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?conjunctive-unit
               --
               (dependency (pos-tag cc)
                           (edge cc)))
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


(def-fcg-cxn partial-passive-transitive-actor-cxn
             ((?vp-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (actor-arg-structure)))
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
               (footprints (not actor-arg-structure))))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X is caused by Y")


(def-fcg-cxn partial-passive-transitive-theme-cxn
             ((?vp-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (theme-arg-structure)))
              <-
              (?vp-unit
               --
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (dependency (pos-tag vbn))
               (footprints (not theme-arg-structure)))
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


(def-fcg-cxn causative-to-cxn
             ((?vp-unit
               (syn-cat (voice active)
                        (phrase-type vp))
               (footprints (arg-structure)))
              <-
              (?subject-unit
               (referent ?y)
               --
               (dependency (edge nsubj))
               (head ?vp-unit))
              (?vp-unit
               --
               (sem-cat (frame causation))
               (referent ?ev)
               (sem-valence (actor ?y)
                            (theme ?x))
               (syn-cat (lex-class verb))
               (syn-valence (subject ?subject-unit))
               (footprints (not arg-structure)))
              (?effect-unit
               (referent ?x)
                --
               (dependency (pos-tag vb)
                           (edge ccomp))
               (head ?vp-unit)))
             :disable-automatic-footprints t
             :cxn-set unhashed
             :description "Example sentence: X causes [Y to Zverb]")


(def-fcg-cxn perfect-infinitive-passive-cxn
             ((?caused-unit
               (syn-cat (voice passive)
                        (phrase-type vp))
               (footprints (arg-structure)))
               <-
               (?to-unit
                --
                (head ?caused-unit)
                (form ((string ?to-unit "to"))))
               (?have-unit
                --
                (head ?caused-unit)
                (form ((string ?have-unit "have"))))
               (?been-unit
                --
                (head ?caused-unit)
                (form ((string ?been-unit "been"))))
               (?effect-unit
                (referent ?y)
                --
                (dependency (pos-tag nn))
                (head ?unknown-2))
               (?unknown-1
                --
                (head ?unknown-2))
               (?caused-unit
                --
                (head ?unknown-1)
                (referent ?ev)
                (sem-valence (actor ?x)
                             (theme ?y))
                (syn-cat (verb-form participle))
                (dependency (pos-tag vbn)
                            (edge xcomp))
                (syn-valence (subject ?effect-unit))
                (footprints (not arg-structure)))
               (?by-unit
                --
                (head ?caused-unit)
                (dependency (edge agent)))
               (?cause-unit
                (referent ?x)
                --
                (dependency (edge pobj))
                (head ?by-unit)))
             :cxn-set unhashed
             :description "Example sentence: X is likely to have been caused by Y")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame specific constructions for linking frame slots to units
;;-----------------------------------------------------------------


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

(def-fcg-cxn X-event-due-to-Y-v1
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?event-unit
               (referent ?effect)
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?subject))
               --
               ;TODO no way to look for verbal phrase features in spacy output? MAYBE VIA SUBJ???
               )
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?event-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge prep)))
              (?to-unit
               --
               (head ?due-unit)
               (form ((string ?to-unit "to"))))
              (?causal-unit
               (referent ?cause)
               --
               (head ?due-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: X(event) due to Y(obj)")

(def-fcg-cxn X-event-due-to-Y-v2
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?event-unit
               (referent ?effect)
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?subject))
               --
               )
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?event-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge prep)))
              (?to-unit
               --
               (head ?due-unit)
               (form ((string ?to-unit "to"))))
              (?causal-unit
               (referent ?cause)
               --
               (head ?to-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: X(event) due to Y(obj)")

(def-fcg-cxn X-is-due-to-Y-v1
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?effect-unit
               (referent ?effect)
               --
               (head ?event-unit)
               (dependency (edge nsubj)))
              (?event-unit
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?effect-unit))
               --
               (HASH form ((meets ?due-unit ?to-unit ?scope))))
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?event-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge acomp)))
              (?to-unit
               --
               (head ?due-unit)
               (dependency (pos-tag in)))
              (?causal-unit
               (referent ?cause)
               --
               (head ?to-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: X(nsubj) is due to Y(pobj)")

(def-fcg-cxn X-is-due-to-Y-v2
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?effect-unit
               (referent ?effect)
               --
               (head ?event-unit)
               (dependency (edge nsubj)))
              (?event-unit
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?effect-unit))
               --
               )
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?event-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge acomp)))
              (?to-unit
               --
               (head ?due-unit)
               (dependency (pos-tag in)))
              (?causal-unit
               (referent ?cause)
               --
               (head ?event-unit)
               (dependency (edge attr))))
             :cxn-set unhashed
             :description "Example sentence: X(nsubj) is due to Y(attr)")

(def-fcg-cxn X-is-due-to-Y-v3
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?effect-unit
               (referent ?effect)
               --
               (head ?event-unit)
               (dependency (edge nsubj)))
              (?event-unit
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?effect-unit))
               --
               )
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?event-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge acomp)))
              (?to-unit
               --
               (head ?due-unit)
               (dependency (pos-tag in)))
              (?causal-unit
               (referent ?cause)
               --
               (head ?due-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: X(nsubj) is due to Y(pobj)")

;(def-fcg-cxn verb-X-due-to-Y
;             ((?due-unit
;               (referent ?frame)
;               (sem-cat (frame causation))
;               (args (?frame ?cause ?effect)))
;              <-
;              (?effect-unit
;               (referent ?effect)
;               --
;               (head ?event-unit)
;               (dependency (edge dobj)))
;              (?event-unit
;               (syn-cat (phrase-type vp))
;               (syn-valence (subject ?effect-unit))
;               --
;               (HASH form ((meets ?due-unit ?to-unit ?scope))))
;              (?due-unit
;               (HASH meaning ((frame causation due-to ?frame)
;                              (slot cause ?frame ?cause)
;                              (slot effect ?frame ?effect)))
;               --
;               (head ?event-unit)
;               (form ((string ?due-unit "due")))
;               (dependency (edge prep)))
;              (?to-unit
;               --
;               (head ?due-unit)
;               (dependency (pos-tag in)))
;              (?causal-unit
;               (referent ?cause)
;               --
;               (head ?due-unit)
;               ;(HASH form ((precedes ?due-unit ?causal-unit ?scope)))
;               (dependency (edge pobj))))
;             :cxn-set unhashed
;             :description "Example sentence: predicts X(dobj) due to Y(pobj)")

(def-fcg-cxn X1-of-X2-due-to-Y
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?X2-unit)
               (form ((string ?due-unit "due")))
               (dependency (edge amod)))
              (?X2-unit
               --
               (head ?of-unit)
               (dependency (edge pobj)))
              (?of-unit
               --
               (form ((string ?of-unit "of")))
               (head ?X1-unit))
              (?X1-unit
               (referent ?effect)
               --
               (dependency (pos-tag nn)))
              (?to-unit
               --
               (head ?due-unit)
               (dependency (pos-tag in)))
              (?causal-unit
               (referent ?cause)
               --
               (head ?due-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: X1 of X2(pobj) due to Y(pobj)")

(def-fcg-cxn predicative-adj-due-to-Y
             ((?due-unit
               (referent ?frame)
               (sem-cat (frame causation))
               (args (?frame ?cause ?effect)))
              <-
              (?due-unit
               (HASH meaning ((frame causation due-to ?frame)
                              (slot cause ?frame ?cause)
                              (slot effect ?frame ?effect)))
               --
               (head ?adj)
               (form ((string ?due-unit "due")))
               (dependency (edge prep)))
              (?adj
               --
               (head ?event-unit)
               (dependency (pos-tag jj)
                           (edge acomp)))
              (?event-unit
               (syn-cat (phrase-type vp))
               (syn-valence (subject ?subject))
               (referent ?effect)
               --
               )
              (?subject
               --
               (head ?event-unit)
               (dependency (edge nsubj)))
              (?to-unit
               --
               (head ?due-unit)
               (dependency (pos-tag in)))
              (?causal-unit
               (referent ?cause)
               --
               (head ?due-unit)
               (dependency (edge pobj))))
             :cxn-set unhashed
             :description "Example sentence: a is b(adj) due to Y")

