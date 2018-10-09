;; This file contains all definitions of the frames that the grammar covers
(in-package :frame-extractor)

(reset-frame-library)

(def-frame intentionally-act-frame
           ((act) (agent) (place)))

(def-frame achieving-first-frame
           ((cognizer) (new-idea)))

(def-frame predicting-frame
           ((speaker) (eventuality) (evidence) (medium)))

(def-frame expectation-frame
           ((cognizer) (phenomenon)))

(def-frame causation-frame
           ((cause) (effect) (actor) (affected)))

(def-frame destroying-frame
           ((cause) (patient) (destroyer)))

(def-frame obviousness-frame
           ((phenomenon) (degree)))

(def-frame eventive-affecting-frame
           ((entity) (event)))

(def-frame statement-frame
           ((addressee) (message) (speaker) (medium) (topic)))

(def-frame run-risk-frame
           ((protagonist) (asset) (purpose) (action)))

(def-frame omen-frame
           ((predictive-phenomenon) (interested-party) (outcome)))

(def-frame change-position-on-a-scale-frame
           ((item) (difference) (duration)))

   