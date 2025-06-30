(in-package :concept-representations)

;; TODO: needs improvement
(defun random-float (&key (base 10000))
  (float (/ (random base) base)))