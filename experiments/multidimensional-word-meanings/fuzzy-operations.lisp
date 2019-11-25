(in-package :mwm)

;; ------------------------
;; + Fuzzy Set Operations +
;; ------------------------

(defun fuzzy-set-channels (set)
  (mapcar #'attribute (mapcar #'car set)))

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
        for a-entry = (find channel a
                            :key #'(lambda (entry)
                                     (attribute (car entry))))
        for b-entry = (find channel b
                            :key #'(lambda (entry)
                                     (attribute (car entry))))
        do (push
            (cond
             ;; if both are present with the same certainty
             ;; choose random
             ((and a-entry b-entry
                   (= (cdr a-entry) (cdr b-entry)))
              (random-elt (list a-entry b-entry)))
             ;; if both are present with different certainty
             ;; take the one with highest certainty
             ((and a-entry b-entry
                   (/= (cdr a-entry) (cdr b-entry)))
              (the-biggest #'cdr (list a-entry b-entry)))
             ;; if only one is present, take that one
             (t (or a-entry b-entry)))
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
