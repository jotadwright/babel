(in-package :cle)

;; --------------------------------
;; + Comparing OBJECT <-> CONCEPT +
;; --------------------------------

(defmethod weighted-similarity ((agent cle-agent) (object cle-object) (concept concept))
  (similarity agent
              object
              concept
              (get-configuration agent :similarity-config)))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :paper)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (/ (weight prototype) ledger)
                 (exp (- (abs similarity))))
            into mahalanobis
        finally (return mahalanobis)))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :univariate-2)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (weight prototype)
                 (exp (- (abs similarity))))
            into mahalanobis
        finally (return mahalanobis)))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :univariate-3)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
        ;; note: ledger could be factored out
          sum (* (/ (weight prototype) ledger)
                 (exp (- (* 1/2 (abs similarity)))))
            into mahalanobis
        finally (return mahalanobis)))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :multivariate-1)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (expt (/ (weight prototype) ledger) 2)
                 (expt similarity 2))
            into mahalanobis
        finally (return (exp (- mahalanobis)))))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :multivariate-2)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (/ (weight prototype) ledger)
                 (expt similarity 2))
            into mahalanobis
        finally (return (exp (- mahalanobis)))))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :multivariate-3)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (expt (/ (weight prototype) ledger) 2)
                 (expt similarity 2))
            into mahalanobis
        finally (return (exp (* 1/2 (- mahalanobis))))))

(defmethod similarity ((agent cle-agent) (object cle-object) (concept concept) (mode (eql :multivariate-4)))
  "Compute the weighted similarity between an object and a concept."
  (loop with prototypes = (get-available-prototypes agent concept)
        with ledger = (loop for prototype in prototypes sum (weight prototype))
        for prototype in prototypes
        for observation = (perceive-object-val agent object (channel prototype))
        for similarity = (observation-similarity observation prototype)
        if (and similarity (not (zerop ledger)))
          ;; note: ledger could be factored out
          sum (* (/ (weight prototype) ledger)
                 (abs similarity))
            into mahalanobis
        finally (return (exp (- mahalanobis)))))

;; ----------------------------------
;; + Comparing OBJECT <-> PROTOTYPE +
;; ----------------------------------
(defmethod observation-similarity ((observation null) (prototype prototype))
  "Similarity is nil if no observation is available."
  nil)

(defmethod observation-similarity ((observation number) (prototype prototype))
  "Similarity [0,1] on the level of a single prototype.
   
   The similarity is computed by comparing the observation to the prototype's
   distribution. The similarity is the probability of the observation given the
   prototype's distribution."
  (let* ((distribution (distribution prototype))
         (mean (mean distribution))
         (st-dev (st-dev distribution))
         (z-score (if (not (zerop st-dev))
                    (/ (- observation mean) st-dev)
                    0)))
    z-score))


;; -------------------------------
;; + Similarity between CONCEPTS +
;; -------------------------------
(defmethod similar-concepts ((agent cle-agent) (concept1 concept) (concept2 concept))
  "Compute the similarity between two concepts.
   
   The overall similarity is computed by measuring the weighted similarity between
    all pairs of prototypes from the two concepts. The weights of each prototype are
    normalised by the sum of all weights of the prototypes in the concept."
  (loop with ledger1 = (loop for proto in (get-available-prototypes agent concept1) sum (weight proto))
        with ledger2 = (loop for proto in (get-available-prototypes agent concept2) sum (weight proto))
        for proto1 in (get-available-prototypes agent concept1)
        for proto2 = (gethash (channel proto1) (prototypes concept2))
        if (and proto2 (not (zerop ledger1)) (not (zerop ledger2)))
          sum (similar-prototypes proto1 proto2 ledger1 ledger2 (get-configuration agent :hellinger-config))))

(defmethod similar-prototypes ((proto1 prototype) (proto2 prototype) (ledger1 number) (ledger2 number) (sim-mode symbol))
  "Calculates the similarity between two prototypes.
   
   The similarity corresponds to a product t-norm of
    1. the average weight of the two prototypes
    2. the similarity of the weights
    3. the complement of the hellinger distance between the two prototypes' distributions."
  (let (;; take the average weight
        (avg-weight (/ (+ (/ (weight proto1) ledger1)
                          (/ (weight proto2) ledger2))
                       2))
        ;; similarity of the weights
        (weight-similarity (- 1 (abs (- (/ (weight proto1) ledger1) (/ (weight proto2) ledger2)))))
        ;; take complement of distance (1-h) so that it becomes a similarity metric
        (prototype-similarity (- 1 (f-divergence (distribution proto1) (distribution proto2) sim-mode))))
    ;; multiple all three
    (* avg-weight weight-similarity prototype-similarity)))