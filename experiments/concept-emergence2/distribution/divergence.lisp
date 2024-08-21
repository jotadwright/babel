(in-package :cle)

;; ----------------
;; + F-Divergence +
;; ----------------

(defgeneric f-divergence (distribution1 distribution2 &key &allow-other-keys)
  (:documentation "Returns the f-divergence between two distributions."))

(defmethod f-divergence ((distribution1 gaussian)
                         (distribution2 gaussian)
                         &key
                         &allow-other-keys)
  "Quantifies the hellinger distance between two probability distributions.

   It forms a bounded metric on the space of probability distributions
     over a given probability space. Maximum distance 1 is achieved when
     P assigns probability zero to every set to which Q assigns a positive probability,
     and vice versa."
  (let ((mu1 (mean distribution1))
        (sigma1 (st-dev distribution1))
        (mu2 (mean distribution2))
        (sigma2 (st-dev distribution2)))
    (if (and (zerop sigma1)
             (zerop sigma2))
      ;; if both distributions are Dirac distributions with zero sigma, return maximal distance of 1
      1.0
      ;; otherwise perform distance calculation
      (realpart (sqrt (- 1
                         (*
                          (sqrt (/ (* 2 sigma1 sigma2)
                                   (+ (expt sigma1 2) (expt sigma2 2))))
                          (exp (* -1/4 (/ (expt (- mu1 mu2) 2)
                                          (+ (expt sigma1 2) (expt sigma2 2))))))))))))

(defmethod f-divergence ((distribution1 categorical)
                         (distribution2 categorical)
                         &key
                         &allow-other-keys)
  ;; step 1: synchronisation
  ;;   due to disabling of channels, two distributions could have different observations over channels
  ;;   therefore the first step is to synchronise the two
  (synchronize-hash-tables distribution1 distribution2)
  ;; step 2: normalisation to ensure its a valid probability distribution that sums to 1
  (let ((p (normalise distribution1))
        (q (normalise distribution2)))
    (loop for (key1 . count1) in p
          for count2 = (assqv key1 q :test #'equal)
          sum (expt (- (sqrt count1) (sqrt count2)) 2) into total
          finally (return (* (/ 1 (sqrt 2)) (sqrt total))))))
