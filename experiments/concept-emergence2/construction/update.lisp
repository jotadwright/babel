(in-package :cle)

;; -----------------
;; + Update scores +
;; -----------------

(defmethod update-score-cxn (agent cxn delta &key
                                   (upper-bound 1.0)
                                   (lower-bound 0.0))
  "Updates the entrenchment score of a cxn."
  ;; update the score
  (setf (score cxn) (+ (score cxn) delta))
  ;; check the upper boundary
  (when (> (score cxn) upper-bound)
    (setf (score cxn) upper-bound))
  (when (< (score cxn) lower-bound)
    (setf (score cxn) lower-bound))
  (update-lexicon-inventory (lexicon agent) cxn))

(defmethod update-history (agent cxn)
  "Keeps track how many times and when the cxn is used."
  (let ((scene-idx (index (current-scene (world (experiment agent)))))
        (interaction-number (interaction-number (current-interaction (experiment agent)))))
    (setf (history cxn) (cons (cons interaction-number scene-idx) (history cxn)))))
