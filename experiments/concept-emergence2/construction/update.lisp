(in-package :cle)

;; -----------------
;; + Update scores +
;; -----------------

;; adjust entrenchment
(defmethod update-score-cxn (agent cxn delta &key
                                   (upper-bound 1.0)
                                   (lower-bound 0.0))
  ;; update the score
  (setf (score cxn) (+ (score cxn) delta))
  ;; check the upper boundary
  (when (> (score cxn) upper-bound)
    (setf (score cxn) upper-bound))
  ;; check the lower boundary + forget if needed
  (when (and (get-configuration (experiment agent) :trash-concepts)
            (<= (- (score cxn) delta) lower-bound))
    (push cxn (lexicon agent))
    (setf (trash agent) (remove cxn (trash agent))))
  (when (<= (score cxn) lower-bound)
    (setf (score cxn) lower-bound)
    (when (get-configuration (experiment agent) :trash-concepts)
      (push cxn (trash agent))
      (setf (lexicon agent) (remove cxn (lexicon agent))))))

(defmethod update-history (agent cxn)
  "Keeps track how many times and when the cxn is used."
  (let ((scene-idx (index (current-scene (world (experiment agent)))))
        (interaction-number (interaction-number (current-interaction (experiment agent)))))
    (setf (history cxn) (cons (cons interaction-number scene-idx) (history cxn)))))
