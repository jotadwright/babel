(in-package :ecl)

;; -------------------------------
;; + Similarity between CONCEPTS +
;; -------------------------------

(defgeneric similar-concepts-p (concept1 concept2 mode &key &allow-other-keys)
  (:documentation "Returns true iff the two concepts are similar."))

(defmethod similar-concepts-p ((concept1 concept) (concept2 concept) (mode (eql :hellinger))
                               &key
                               (activation 0.5)
                               &allow-other-keys)
  "True iff if the average pair-wise similarity between prototypes is higher than some threshold."
  (let ((similarity-score (similar-concepts concept1 concept2 :hellinger)))
    (>= similarity-score activation)))

(defgeneric similar-concepts (concept1 concept2 mode &key &allow-other-keys)
  (:documentation "Returns true iff the two concepts are similar."))

(defmethod similar-concepts ((concept1 concept) (concept2 concept) (mode (eql :hellinger)) &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for proto in (meaning concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (meaning concept2) sum (weight proto))
        for proto1 in (meaning concept1)
        for proto2 in (meaning concept2)
        for avg-weight = (/ (+ (/ (weight proto1) concept1-weight-sum)
                                  (/ (weight proto2) concept2-weight-sum))
                               2)
        for prototype-similarity = (- 1 (prototype-distance proto1 proto2 mode))
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

;; ----------------------------------
;; + Comparing CONCEPT <-> CONCEPT  +
;; ----------------------------------

;; main algorithm - TODO fix readability
(defun find-duplicate-concept-sets (concepts &key activation)
  "Filters a list of concepts based on similarity and entrenchement."
  (let* ((clean-concepts (loop for concept in concepts collect (assqv :concept concept)))
         ;; find the tuples
         (found-sets (multiple-value-bind
                              (uniques duplicate-tuples)
                              (find-duplicate-concepts clean-concepts :activation activation)
                           (let* ((duplicate-sets (find-duplicate-sets duplicate-tuples))
                                  (unique-sets (loop for unique in uniques collect (list unique))))
                             (append unique-sets duplicate-sets))))
         ;; add previous removed info back to set information
         (res (loop for set in found-sets
                    for new-set = (loop for el in set collect (find el
                                                                    concepts
                                                                    :test #'(lambda (x other) (equal x (assqv :concept other)))))
                    collect new-set)))
    res))

;; step 1 - calculate distances and flag duplicates as tuples
(defun find-duplicate-concepts (lst &key (acc '()) (flagged '()) activation)
  "Given a list of concepts, returns all unique concepts.
   Duplicate concepts are filtered based on the entrenchment score."
  (if (not lst)
    (values acc flagged)
    (let* ((first (first lst))
           (rest (rest lst))
           (unique-p (loop for second in rest
                           if (similar-concepts-p first second :hellinger :activation activation)
                             do (progn
                                  (setf flagged (cons (cons first second) flagged))
                                  (return nil))
                           finally
                             (return t))))
      (let ((not-flagged-p (loop for (one . two) in flagged
                                 never (or (equal first one) (equal first two)))))
        (if (and unique-p not-flagged-p)
          (find-duplicate-concepts rest :acc (cons first acc) :flagged flagged :activation activation) ;; here check to skip
          (find-duplicate-concepts rest :acc acc :flagged flagged :activation activation))))))

;; step 2 - merge common tuples into sets
(defun find-duplicate-sets (tuples)
  "Merges a list of tuples into a list of sets.

   For example,
       input: ((a b) (b c) (g e) (f c) (g d))
       output: ((a b c f) (g e d))."
  (let* ((duplicate-sets '())) ;; initialize first set
      (loop for (c1 . c2) in tuples ;; iterate over rest
            for change = nil
            do (loop named inner
                     for set in duplicate-sets and idx from 0 do
                       (let ((mem-c1-p (if (member c1 set) t nil))
                              (mem-c2-p (if (member c2 set) t nil)))
                          (when (and mem-c1-p (not mem-c2-p))
                            (setf (nth idx duplicate-sets) (cons c2 (nth idx duplicate-sets)))
                            (setf change t)
                            (return-from inner))
                          (when (and (not mem-c1-p) mem-c2-p)
                            (setf (nth idx duplicate-sets) (cons c1 (nth idx duplicate-sets)))
                            (setf change t)
                            (return-from inner))))
            when (not change)
              do (setf duplicate-sets (cons (list c1 c2) duplicate-sets)))
      duplicate-sets))

;; step 3 - select the best entrenched concept within a set 
(defun select-best-entrenched-concept (duplicate-set)
  "Given a set of concepts, returns the best entrenched concept."
  (loop with best-val = -1
        with best-triple = nil
        for triple in duplicate-set
        for concept = (assqv :concept triple)
        do (when (or (not best-triple) (< best-val (score concept)))
             (setf best-val (score concept)
                   best-triple triple))
        finally
          (return best-triple)))

;; helper functions for finding meaning competitors of the hearer
(defun find-duplicate-concept (agent concept &key activation)
  "Find all concepts in the lexicon of an agent similar to a given concept."
  (loop for other-concept in (lexicon agent)
        if (and (not (equal concept other-concept))
                (similar-concepts-p concept other-concept :hellinger :activation activation))
          collect other-concept))

(defun filter-discriminative-concepts (agent other-options)
  "Discriminately conceptualise the topic relative to the context."
  (let ((topic (get-data agent 'topic))
        (context (objects (get-data agent 'context)))
        (discriminating-concepts '()))
    (loop for concept in other-options
          for topic-similarity = (weighted-similarity topic concept)
          for best-other-similarity
            = (loop for object in (remove topic context)
                    maximize (weighted-similarity object concept))
          when (> topic-similarity best-other-similarity)
            do (progn
                 (setf discriminating-concepts (cons concept discriminating-concepts))))
    discriminating-concepts))