(in-package :cle)

;; --------------------------------
;; + Comparing OBJECT <-> CONCEPT +
;; --------------------------------

(defmethod weighted-similarity ((object cle-object) (concept concept))
  "Compute the weighted similarity between an object and a concept."
  (loop for prototype in (prototypes concept)
        for observation = (get-channel-val object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        collect (* (weight prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))

;; ----------------------------------
;; + Comparing OBJECT <-> PROTOTYPE +
;; ----------------------------------
(defmethod observation-similarity ((observation number) (prototype prototype) &key (max-z-score 1)) ;; TODO: MAJOR QUESTION (see obsidian)
  "Similarity on the level of a single prototype."
  ;; similarity measure between [-inf,1]
  (let* ((distribution (distribution prototype))
         (st-dev (st-dev distribution))
         (z-score (if (not (zerop st-dev))
                    ;; z-score formula + absolute value
                    (abs (/ (- observation (mean distribution)) st-dev))
                    0))
         (sim (/ (- max-z-score z-score) max-z-score)))
    sim))

;; -------------------------------
;; + Similarity between CONCEPTS +
;; -------------------------------
(defmethod similar-concepts ((concept1 concept) (concept2 concept) (mode (eql :times)) &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for proto in (prototypes concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (prototypes concept2) sum (weight proto))
        for proto1 in (prototypes concept1)
        for proto2 in (prototypes concept2)
        for avg-weight = (/ (+ (/ (weight proto1) concept1-weight-sum)
                               (/ (weight proto2) concept2-weight-sum))
                            2)
        for weight-similarity = (- 1 (abs (- (weight proto1) (weight proto2))))
        for prototype-similarity = (- 1 (f-divergence (distribution proto1) (distribution proto2) :hellinger))
        for sim-score = (* avg-weight prototype-similarity weight-similarity)
        sum sim-score))
