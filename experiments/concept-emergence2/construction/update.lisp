(in-package :cle)

;; -----------------
;; + Update scores +
;; -----------------

;; adjust entrenchment
(defmethod update-score-cxn (agent cxn delta &key
                                   (upper-bound 1.0)
                                   (lower-bound 0.0)
                                   (forget-cxns nil))
  ;; update the score
  (setf (score cxn) (+ (score cxn) delta))
  ;; check the upper boundary
  (when (> (score cxn) upper-bound)
    (setf (score cxn) upper-bound))
  ;; check the lower boundary + forget if needed
  (when (<= (score cxn) lower-bound)
    (setf (score cxn) lower-bound)
    (when forget-cxns
      (setf (lexicon agent) (remove cxn (lexicon agent))))))
