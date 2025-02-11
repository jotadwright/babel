(in-package :concept-representations)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod update-concept ((concept concept) (entity entity))
    (update-representation (representation concept) entity))

(defmethod update-representation ((concept weighted-multivariate-distribution-concept) (entity entity) (context list) &key weight-incf weight-decf)
  "Update a concept based on a new observed entity.

  Updates 1. each distribution
          2. the weights of distributions positively and others negatively."

    ;; 1. update the prototypical values
    (loop for weighted-distribution in (get-weighted-distributions concept)
          for distribution = (distribution (weighted-distribution))
          for feature-name = (feature-name weighted-distribution)
          for feature-value = (get-feature-value entity feature-name)
          do (update-distribution distribution feature-value))
  
    ;; 2. determine which attributes should get an increase
    ;;    in weight, and which should get a decrease.
    (let* ((similarity-table (make-similarity-table concept))
           (discriminating-attributes (find-discriminating-attributes concept entity context similarity-table))
           (subsets-to-consider (get-all-subsets concept discriminating-attributes))
           (best-subset (find-most-discriminating-subset subsets-to-consider entity similarity-table)))

      ;; when no subset is found, use all weighted-distributions 
      (when (null best-subset)
        (setf best-subset (get-weighted-distributions concept)))

      ;; 3. actually update the weight scores
      (loop for weighted-distribution in weighted-distributions
            for feature-name = (feature-name weighted-distribution)
            ;; if part of the contributing prototypes -> reward
            if (member feature-name best-subset :key #'feature-name)
              do (update-weight weighted-distribution weight-incf)
              ;; otherwise -> punish
            else
              do (update-weight weighted-distribution weight-decf))))

;; -------------------------------------
;; + Step 1: make the similarity table +
;; -------------------------------------
(defmethod make-similarity-table ((concept weighted-multivariate-distribution-concept) (entities list))
  "Compute the similarities between the concept and a set of entities, 
   for all features of the concept and store them/re-use 
   them to compute the discriminative
   attributes and to find the most discriminative subset.
                   
   Saves tons in computation by only calculating it only once."
  (loop with feature-hash = (make-hash-table)
        with sum-of-weights = (calculate-sum-of-weights concept)
        for weighted-distribution in (get-weighted-distributions concept)
        for feature-name = (feature-name weighted-distribution)
        for entities-hash = (loop with hash = (make-hash-table)
                                 for entity in entities
                                 for distribution = (distribution weighted-distribution) 
                                 for feature-value = (get-feature-value entity feature-name)
                                 for distance = (distribution-feature-distance distribution feature-value)
                                 for similarity = (calculate-similarity-s distance)
                                 for weighted-similarity = (if (not (zerop sum-of-weights))
                                                             (calculate-similarity-ws distance (/ (weight prototype) sum-of-weights))
                                                             0)
                                 do (setf (gethash (id entity) hash) (cons similarity weighted-similarity))
                                 finally (return hash))
        do (setf (gethash feature-name attribute-hash) entities-hash)
        finally (return feature-hash)))

(defun get-s (entity feature-name table)
  "Retrieve the similarity for the given entity-feature combination."
  (first (gethash (id entity) (gethash feature-name table))))

(defun get-ws (entity feature-name table)
  "Retrieve the weighted similarity for the given entity-feature combination."
  (rest (gethash (id entity) (gethash feature-name table))))

(defmethod calculate-similarity-s ((distance number))
  (exp (- (* 1/2 (expt distance 2)))))

(defmethod calculate-similarity-ws ((distance number) (weight number))
  (* (expt weight 2) (expt distance 2)))

;; --------------------------------------------
;; + Step 2: find the discriminating features +
;; --------------------------------------------
(defmethod find-discriminating-attributes ((concept weighted-multivariate-distribution-concept) (entity entity) (context list) (similarity-table hash-table))
  "..."
  (loop with other-entitys = (remove entity context)
        with discriminating-attributes = nil
        for weighted-distribution in (get-weighted-distributions concept)
        for feature-name = (feature-name weighted-distribution)
        for topic-similarity = (get-s entity feature-name sim-table)
        for best-other-similarity = (loop for entity in other-entitys
                                   maximize (get-s entity feature-name sim-table))
        when (> topic-similarity best-other-similarity)
          do (push feature-name discriminating-attributes)
        finally (return discriminating-attributes)))

;; ---------------------------------
;; + Step 3: Generate the powerset +
;; ---------------------------------
(defun get-all-subsets (concept subset-attr)
  "Given a set of attributes and a subset of that set, returns all
   subsets of the complete set that contain the subset."
  
  (let* ((all-weighted-distributions (get-weighted-distributions concept))
         (rest-attr (loop for el in all-weighted-distributions
                          if (not (find (feature-name el) subset-attr))
                            collect el)))
    (if (length> rest-attr 6) ;; TODO HEURISTIC TO AVOID COMBINATORIAL EXPLOSION
      ;; use all attributes
      (list (loop for el in all-weighted-distributions
                  if (find (feature-name el) subset-attr)
                    collect el))
      ;; otherwise, generate the powerset
      (let* ((all-subsets-of-rest (cons '() (all-subsets rest-attr)))
             (subset-attr-values (loop for el in all-weighted-distributions
                                       if (find (feature-name el) subset-attr)
                                         collect el))
             (all-subsets (loop for el in all-subsets-of-rest
                                collect (append subset-attr-values el))))
        all-subsets))))

;; ------------------------------------------
;; + Step 4: Find discriminating attributes +
;; ------------------------------------------
(defun weighted-similarity-with-table (subset-of-weighted-distributions entity similarity-table)
  "Compute the weighted similarity between the entity and the
   list of prototypes, using the given similarity table."
  (loop for weighted-distribution in subset-of-weighted-distributions
        for feature-name = (feature-name weighted-distribution)
        for ws = (get-ws entity feature-name similarity-table)
        collect ws into mahalanobis
        finally (return (exp (* 1/2 (- (sum mahalanobis)))))))

;; ----------------------------------
;; + Find discriminating attributes +
;; ----------------------------------

(defun find-most-discriminating-subset (subsets entity context similarity-table)
  "Find the subset that maximizes the difference in similarity
   between the entity and the best other entity."
  (loop with other-entities = (remove entity context)
        with best-subset = nil
        with best-score = 0
        for subset in subsets
        for topic-similarity = (weighted-similarity-with-table subset topic similarity-table)
        for best-other-similarity = (calculate-max-similarity-in-context-using-subsets other-entities topic-similarity subset similarity-table)
        for discriminative-power = (abs (- topic-similarity best-other-similarity))
        when (and (> topic-similarity best-other-similarity) 
                  (> discriminative-power best-score))
          do (setf best-subset subset
                   best-score discriminative-power)
        finally (return best-subset)))

(defun calculate-max-similarity-in-context-using-subsets (other-entities topic-similarity subset similarity-table)
  """Calculates the maximim similarity between the given concept and all other entities in the context."
  (loop named lazy-loop
        for entity in other-entities
        for other-similarity = (weighted-similarity-with-table subset entity similarity-table)
        when (<= topic-similarity other-similarity)
          ;; lazy stopping
          do (return-from lazy-loop other-similarity)
        maximize other-similarity))

