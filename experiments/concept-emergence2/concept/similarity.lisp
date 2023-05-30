(in-package :cle)

;; --------------------------------
;; + Comparing OBJECT <-> CONCEPT +
;; --------------------------------

(defmethod weighted-similarity ((object cle-object) (concept concept))
  "Compute the weighted similarity between an object and a concept."
  (loop for prototype in (prototypes concept)
        for exemplar = (get-channel-val object (channel prototype))
        for similarity = (exemplar-similarity exemplar prototype)
        collect (* (weight prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))

;; ----------------------------------
;; + Comparing OBJECT <-> PROTOTYPE +
;; ----------------------------------

(defmethod exemplar-similarity ((exemplar number) (prototype prototype))
  "Similarity on the level of a single prototype."
  ;; similarity measure between [-inf,1]
  (let* ((distribution (distribution prototype))
         (st-dev (st-dev distribution))
         (z-score (if (not (eq st-dev 0.0))
                    ;; z-score formula + absolute value
                    (abs (/ (- exemplar (mean distribution)) st-dev))
                    0))
         (max-z-score 2)
         (sim (/ (- max-z-score z-score) max-z-score)))
    sim))

;; -------------------------------
;; + Similarity between CONCEPTS +
;; -------------------------------

(defmethod similar-concepts-p ((concept1 concept) (concept2 concept) (mode (eql :standard))
                               &key
                               activation
                               &allow-other-keys)
  "True iff if the average pair-wise similarity between prototypes is higher than some threshold."
  (let ((similarity-score (similar-concepts concept1 concept2 :standard)))
    (>= similarity-score activation)))

(defmethod similar-concepts ((concept1 concept) (concept2 concept) (mode (eql :standard)) &key &allow-other-keys)
  "Pairwise similarity of two concepts using prototypes."
  (loop with concept1-weight-sum = (loop for proto in (prototypes concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (prototypes concept2) sum (weight proto))
        for proto1 in (prototypes concept1)
        for proto2 in (prototypes concept2)
        for avg-weight = (/ (+ (/ (weight proto1) concept1-weight-sum)
                               (/ (weight proto2) concept2-weight-sum))
                            2)
        for prototype-similarity = (- 1 (f-divergence (distribution proto1) (distribution proto2) :hellinger))
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))

;; ----------------------------------
;; + Comparing CONCEPT <-> CONCEPT  +
;; ----------------------------------

;; main algorithm - TODO fix readability
(defun find-similar-concepts-into-sets (cxns &key activation)
  "Filters a list of concepts based on similarity and entrenchement."
  (let* ((clean-concepts (loop for cxn in cxns collect (assqv :cxn cxn)))
         ;; find the tuples
         (found-sets (multiple-value-bind
                              (uniques similar-tuples)
                              (find-similar-concepts clean-concepts :activation activation)
                           (let* ((similar-sets (find-similar-sets similar-tuples))
                                  (unique-sets (loop for unique in uniques collect (list unique))))
                             (append unique-sets similar-sets))))
         ;; add previous removed info back to set information
         (res (loop for set in found-sets
                    for new-set = (loop for el in set collect (find el
                                                                    cxns
                                                                    :test #'(lambda (x other) (equal x (assqv :cxn other)))))
                    collect new-set)))
    res))

;; step 1 - calculate distances and flag similars as tuples
(defun find-similar-concepts (lst &key (acc '()) (flagged '()) activation)
  "Given a list of concepts, returns all unique concepts.
   similar concepts are filtered based on the entrenchment score."
  (if (not lst)
    (values acc flagged)
    (let* ((first (first lst))
           (rest (rest lst))
           (unique-p (loop for second in rest
                           if (similar-concepts-p (meaning first) (meaning second) :standard :activation activation)
                             do (progn
                                  (setf flagged (cons (cons first second) flagged))
                                  (return nil))
                           finally
                             (return t))))
      (let ((not-flagged-p (loop for (one . two) in flagged
                                 never (or (equal first one) (equal first two)))))
        (if (and unique-p not-flagged-p)
          (find-similar-concepts rest :acc (cons first acc) :flagged flagged :activation activation) ;; here check to skip
          (find-similar-concepts rest :acc acc :flagged flagged :activation activation))))))

;; step 2 - merge common tuples into sets
(defun find-similar-sets (tuples)
  "Merges a list of tuples into a list of sets.

   For example,
       input: ((a b) (b c) (g e) (f c) (g d))
       output: ((a b c f) (g e d))."
  (let* ((similar-sets '())) ;; initialize first set
      (loop for (c1 . c2) in tuples ;; iterate over rest
            for change = nil
            do (loop named inner
                     for set in similar-sets and idx from 0 do
                       (let ((mem-c1-p (if (member c1 set) t nil))
                              (mem-c2-p (if (member c2 set) t nil)))
                          (when (and mem-c1-p (not mem-c2-p))
                            (setf (nth idx similar-sets) (cons c2 (nth idx similar-sets)))
                            (setf change t)
                            (return-from inner))
                          (when (and (not mem-c1-p) mem-c2-p)
                            (setf (nth idx similar-sets) (cons c1 (nth idx similar-sets)))
                            (setf change t)
                            (return-from inner))))
            when (not change)
              do (setf similar-sets (cons (list c1 c2) similar-sets)))
      similar-sets))

;; step 3 - select the best entrenched concept within a set 
(defun select-best-entrenched-concept (similar-set)
  "Given a set of concepts, returns the best entrenched concept."
  (loop with best-val = -1
        with best-triple = nil
        for triple in similar-set
        for concept = (assqv :cxn triple)
        do (when (or (not best-triple) (< best-val (score concept)))
             (setf best-val (score concept)
                   best-triple triple))
        finally
          (return best-triple)))

;; Alternative

(defmethod similar-concepts ((concept1 concept) (concept2 concept) (mode (eql :times)) &key &allow-other-keys)
  (loop with concept1-weight-sum = (loop for proto in (prototypes concept1) sum (weight proto))
        with concept2-weight-sum = (loop for proto in (prototypes concept2) sum (weight proto))
        for proto1 in (prototypes concept1)
        for proto2 in (prototypes concept2)
        for avg-weight = (/ (+ (/ (weight proto1) concept1-weight-sum)
                               (/ (weight proto2) concept2-weight-sum))
                            2)
        for prototype-similarity = (- 1 (f-divergence (distribution proto1) (distribution proto2) :hellinger))
        for sim-score = (* avg-weight prototype-similarity)
        sum sim-score))
