(in-package :concept-representations)

;; --------------------------------
;; + Comparing CONCEPT <-> ENTITY +
;; --------------------------------

(defmethod concept-entity-similarity ((concept weighted-multivariate-distribution-concept) (entity entity))
  (loop with sum-of-weights = (calculate-sum-of-weights concept)
        for weighted-distribution in (get-weighted-distributions concept)
        for distribution = (distribution weighted-distribution)
        for feature-value = (get-feature-value entity (feature-name weighted-distribution))
        for distance = (distribution-feature-distance distribution feature-value)
        if (and distance (not (zerop sum-of-weights)))
          ;; note: sum-of-weights could be factored out
          sum (* (expt (/ (weight prototype) sum-of-weights) 2)
                 (expt distance 2))
            into mahalanobis
        finally (return (exp (* 1/2 (- mahalanobis))))))

;; --------------------------------------
;; + Comparing FEATURE <-> DISTRIBUTION +
;; --------------------------------------

(defgeneric distribution-feature-distance (distribution feature-value &key &allow-other-keys)
  (:documentation "Measures the distance between an feature-value and a distribution."))

(defmethod distribution-feature-distance ((distribution gaussian) (feature-value number) &key &allow-other-keys)
  "Measures the distance between a feature value (a number) and a gaussian distribution."
  (let* ((distribution (distribution prototype))
         (mean (mean distribution))
         (st-dev (st-dev distribution))
         (z-score (if (not (zerop st-dev))
                    (/ (- observation mean) st-dev)
                    0)))
    z-score))

(defmethod distribution-feature-distance ((distribution bernoulli) (feature-value symbol) &key &allow-other-keys)
  "Measures the distance between a feature value (a category) and a bernoulli distribution."
  (let* ((observed-frequency (gethash feature-value (frequencies distribution)))
         (total-samples (nr-of-samples distribution))
         (number-of-categories (number-of-categories distribution))
         (expected-frequency (/ total-samples num-categories))
         (distance (if (zerop expected-frequency)
                      0
                      (/ (expt (- observed-frequency expected-frequency) 2)
                         expected-frequency))))
    distance))

;; ---------------------------------
;; + Comparing CONCEPT <-> CONCEPT +
;; ---------------------------------
(defmethod concept-similarity ((concept1 weighted-multivariate-distribution-concept) (concept2 weighted-multivariate-distribution-concept))
  "Compute the similarity between two concepts.
   
   The overall similarity is computed by summing the weighted similarity between
    all pairs of weighted distributions from the two concepts."
  (loop with sum-of-weights1 = (calculate-sum-of-weights concept1)
        with sum-of-weights2 = (calculate-sum-of-weights concept2)
        for wd1 in (get-weighted-distributions concept1)
        for wd2 = (gethash (feature-name wd1) (get-weighted-distributions concept2))
        if (and (not (zerop sum-of-weights1)) (not (zerop sum-of-weights1)))
          sum (weighted-distribution-similarity wd1 wd2 sum-of-weights1 sum-of-weights1)))

(defmethod weighted-distribution-similarity ((wd1 weighted-distribution) (wd2 weighted-distribution) (sum-of-weights1 number) (sum-of-weights2 number))
  "Calculates the similarity between two weighted distributions.
   
   The similarity corresponds to a product t-norm of
    1. the average of the normalised weight
    2. the similarity of the normalised weights
    3. the complement of the f-divergence between the distributions (i.e. the similarity)"
  (let (;; take the average weight
        (average-weight (/ (+ (/ (weight wd1) sum-of-weights1)
                              (/ (weight wd2) sum-of-weights2))
                           2))
        ;; similarity of the weights
        (weight-similarity (- 1 (abs (- (/ (weight wd1) sum-of-weights1) (/ (weight wd2) sum-of-weights2)))))
        ;; take complement of distance (1-f) so that it becomes a similarity metric
        (distribution-similarity (- 1 (f-divergence (distribution wd1) (distribution wd2)))))
    ;; multiple all three
    (* average-weight weight-similarity distribution-similarity)))
