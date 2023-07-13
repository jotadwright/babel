(in-package :cle)

;; ----------------
;; + F-Divergence +
;; ----------------

(defgeneric f-divergence (distribution1 distribution2 mode &key &allow-other-keys)
  (:documentation "Returns the f-divergence between two distributions."))

(defmethod f-divergence ((distribution1 gaussian)
                         (distribution2 gaussian)
                         (mode (eql :hellinger))
                         &key
                         &allow-other-keys)
  (hellinger (mean distribution1)
             (st-dev distribution1)
             (mean distribution2)
             (st-dev distribution2)))

;; --------------------
;; + Helper functions +
;; --------------------
(defun hellinger (mu1 sigma1 mu2 sigma2)
  "Quantifies the hellinger distance between two probability distributions.

   It forms a bounded metric on the space of probability distributions
     over a given probability space. Maximum distance 1 is achieved when
     P assigns probability zero to every set to which Q assigns a positive probability,
     and vice versa."
  (if (and (zerop sigma1)
           (zerop sigma2))
    ;; if both distributions are Dirac distributions with zero sigma, return maximal distance of 1
    1.0
    ;; otherwise perform distance calculation
    (realpart (sqrt (- 1
             (sqrt (*
                    (/ (* 2 sigma1 sigma2)
                       (+ (expt sigma1 2) (expt sigma2 2)))
                    (exp (* -1/2 (/ (expt (- mu1 mu2) 2)
                                    (+ (expt sigma1 2) (expt sigma2 2))))))))))))
