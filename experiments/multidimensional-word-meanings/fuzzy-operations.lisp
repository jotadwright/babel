(in-package :mwm)

;; ------------------------
;; + Fuzzy Set Operations +
;; ------------------------

(defun fuzzy-set-channels (set)
  (mapcar #'first set))

(defun fuzzy-set-values (set)
  (mapcar #'second set))

(defun fuzzy-set-certainties (set)
  (mapcar #'third set))

(defun fuzzy-cardinality (set)
  (reduce #'+ set :key #'third :initial-value 0))

(defun fuzzy-union (a b)
  (loop with result
        for channel in (union (fuzzy-set-channels a)
                              (fuzzy-set-channels b))
        for a-entry = (find channel a :key #'first)
        for b-entry = (find channel b :key #'first)
        do (push
            (cond
             ((and a-entry b-entry
                   (> (third a-entry) (third b-entry)))
              a-entry)
             ((and a-entry b-entry
                   (> (third b-entry) (third a-entry)))
              b-entry)
             (a-entry a-entry)
             (b-entry b-entry))
            result)
        finally
        (return result)))

(defun fuzzy-intersection (a b)
  (loop with channel-intersection = (intersection (fuzzy-set-channels a)
                                                  (fuzzy-set-channels b))
        for channel in channel-intersection
        for a-entry = (find channel a :key #'first)
        collect a-entry))

(defun fuzzy-difference (a b)
  (set-difference a b :key #'first))

(defun overlap (combined-meaning object-categories)
  "Compute the overlap between the combined meaning
   of an utterance and the fuzzy-set representation
   of an object"
  (float (/ (- (* (fuzzy-cardinality (fuzzy-intersection combined-meaning object-categories))
                  (fuzzy-cardinality (fuzzy-intersection object-categories combined-meaning)))
               (* (fuzzy-cardinality (fuzzy-difference combined-meaning object-categories))
                  (fuzzy-cardinality (fuzzy-difference object-categories combined-meaning))))
            (* (fuzzy-cardinality combined-meaning) (fuzzy-cardinality object-categories)))))