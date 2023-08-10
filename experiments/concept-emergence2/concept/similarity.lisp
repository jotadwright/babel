(in-package :cle)

;; --------------------------------
;; + Comparing OBJECT <-> CONCEPT +
;; --------------------------------

(defmethod weighted-similarity ((agent cle-agent) (object cle-object) (concept concept))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if similarity
          sum (* (/ (weight prototype) ledger) similarity)))

;; ----------------------------------
;; + Comparing OBJECT <-> PROTOTYPE +
;; ----------------------------------
(defmethod observation-similarity ((observation null) (prototype prototype))
  "Similarity on the level of a single prototype."
  ;; similarity measure between [0,1]
  nil)

(defmethod observation-similarity ((observation number) (prototype prototype))
  "Similarity on the level of a single prototype."
  ;; similarity measure between [0,1]
  (let* ((distribution (distribution prototype))
         (st-dev (st-dev distribution))
         ;; z-score [-inf, + inf], by taking abs: [0, +inf]
         (z-score (if (not (zerop st-dev))
                    ;; z-score formula then absolute value
                    (abs (/ (- observation (mean distribution)) st-dev))
                    0))
         ;; exp(-x) maps [0, +inf] to [0, 1]
         ;; IMPORTANT: func is a design choice
         ;;            can use any func that maps to [0, 1]
         (sim (exp (- z-score)))) 
    sim))

;; -------------------------------
;; + Similarity between CONCEPTS +
;; -------------------------------
(defmethod similar-concepts ((agent cle-agent) (concept1 concept) (concept2 concept))
  (loop with concept1-weight-sum = (loop for proto in (get-available-prototypes agent concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (get-available-prototypes agent concept2) sum (weight proto))
        for proto1 in (get-available-prototypes agent concept1)
        for proto2 = (gethash (channel proto1) (prototypes concept2))
        if proto2
          sum (similar-prototypes proto1 proto2 concept1-weight-sum concept2-weight-sum)))

(defmethod similar-prototypes ((proto1 prototype) (proto2 prototype) (ledger1 number) (ledger2 number))
  (let (;; take the average weight
        (avg-weight (/ (+ (/ (weight proto1) ledger1)
                          (/ (weight proto2) ledger2))
                       2))
        ;; similarity of the weights
        (weight-similarity (- 1 (abs (- (weight proto1) (weight proto2)))))
        ;; invert distance (1-h) so that it becomes a similarity metric
        (prototype-similarity (- 1 (f-divergence (distribution proto1) (distribution proto2) :hellinger))))
    ;; multiple all three
    (* avg-weight weight-similarity prototype-similarity)))