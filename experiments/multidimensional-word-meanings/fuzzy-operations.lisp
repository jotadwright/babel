(in-package :mwm)

;; ------------------------
;; + Fuzzy Set Operations +
;; ------------------------

;(defun fuzzy-set-channels (set)
;  (mapcar #'first set))

(defun fuzzy-set-channels (set)
  (mapcar #'attributes (mapcar #'car set)))

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
             ((and a-entry b-entry
                   (> (cdr a-entry) (cdr b-entry)))
              a-entry)
             ((and a-entry b-entry
                   (> (cdr b-entry) (cdr a-entry)))
              b-entry)
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