(in-package :cle)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod shift-concept ((agent cle-agent) (topic cle-object) (concept concept))
  "Shift a concept towards the given topic.

  Shifts 1. the prototype of each feature channel
         2. the certainties of salient channel positively and others negatively."
  ;; 1. update the prototypical values
  (loop for prototype in (prototypes concept) ;; assumes prototype
        for interaction-number = (interaction-number (current-interaction (experiment agent)))
        do (update-prototype interaction-number prototype topic (get-configuration agent :distribution)))
  ;; 2. determine which attributes should get an increase
  ;;    in weight, and which should get a decrease.
  (let* ((similarity-table (make-similarity-table agent concept))
         (discriminating-attributes (find-discriminating-attributes agent
                                                                    concept
                                                                    topic
                                                                    similarity-table))
         (all-attribute-subsets (all-subsets concept))
         (subsets-to-consider (filter-subsets all-attribute-subsets discriminating-attributes))
         (best-subset (find-most-discriminating-subset agent
                                                       subsets-to-consider
                                                       topic
                                                       similarity-table)))
    (when (null best-subset)
      ;; when best-subset returns NIL
      ;; reward all attributes...
      (setf best-subset (meaning concept)))
    ;; 3. actually update the weight scores
    (loop with rewarded-attributes = nil
          with punished-attributes = nil
          for prototype in (meaning concept)
          ;; if part of the contributing prototypes -> reward
          if (member (attribute prototype) best-subset :key #'attribute)
            do (progn (push (attribute prototype) rewarded-attributes)
                 (update-weight concept
                                (attribute prototype)
                                (get-configuration agent :weight-incf)
                                (get-configuration agent :weight-update-strategy)))
            ;; otherwise -> punish
          else
            do (progn (push (attribute prototype) punished-attributes)
                 (update-weight concept
                                (attribute prototype)
                                (get-configuration agent :weight-decf)
                                (get-configuration agent :weight-update-strategy))))))

;; -----------------------
;; + Utils for alignment +
;; -----------------------

;; events
(define-event found-discriminating-attributes (attributes list))
(define-event found-subset-to-reward (subset list))

;; --------------------
;; + Similarity table +
;; --------------------
(defun make-similarity-table (agent concept)
  "Compute the (weighted) similarities between the concept
   and all objects in the scene, for all attributes of the concept
   and store them/re-use them to compute the discriminative
   attributes and to find the most discriminative subset.
                   
   Saves tons in computation by only calculating it only once."
  (loop with attribute-hash = (make-hash-table)
        for prototype in (meaning concept)
        for attribute = (attribute prototype)
        for objects-hash
          = (loop with hash = (make-hash-table)
                  for object in (objects (get-data agent 'context))
                  for exemplar = (get-channel-val object (attribute prototype))
                  for s = (exemplar-similarity exemplar prototype)
                  for ws = (* (weight prototype) s)
                  do (setf (gethash (id object) hash) (cons s ws))
                  finally (return hash))
        do (setf (gethash attribute attribute-hash) objects-hash)
        finally (return attribute-hash)))

(defun get-s (object attribute table)
  "Retrieve the similarity for the given object-attribute combination."
  (first (gethash (id object) (gethash attribute table))))

(defun get-ws (object attribute table)
  "Retrieve the weighted similarity for the given object-attribute combination."
  (rest (gethash (id object) (gethash attribute table))))

(defun find-discriminating-attributes (agent concept topic similarity-table)
  "Find all attributes that are discriminating for the topic."
  (let ((context (remove topic (objects (get-data agent 'context)))))
    (loop with discriminating-attributes = nil
          for prototype in (meaning concept)
          for attribute = (attribute prototype)
          for topic-similarity = (get-s topic attribute similarity-table)
          for best-other-similarity
            = (when (> topic-similarity 0)
                (loop for object in context
                      maximize (get-s object attribute similarity-table)))
          when (and topic-similarity best-other-similarity
                    (> topic-similarity best-other-similarity))
            do (push attribute discriminating-attributes)
          finally
            (progn
              (return discriminating-attributes)))))

(defmethod filter-subsets (all-subsets discriminating-attributes)
  "Filter all subsets with the discriminating attributes, only
   keeping those subsets where all discriminating attributes occur in."
  (loop with applicable-subsets = nil
        for subset in all-subsets
        for subset-attributes = (mapcar #'attribute subset)
        when (null (set-difference discriminating-attributes subset-attributes))
          do (push subset applicable-subsets)
        finally
          (return applicable-subsets)))

;; -----------------------------
;; + Weighted Similarity table +
;; -----------------------------
(defun weighted-similarity-with-table (object list-of-prototypes table)
  (loop for prototype in list-of-prototypes
        for attribute = (attribute prototype)
        for ws = (get-ws object attribute table)
        collect ws into weighted-similarities
        finally (return (average weighted-similarities))))

;; ----------------------------------
;; + Find discriminating attributes +
;; ----------------------------------
(defun find-most-discriminating-subset (agent subsets topic similarity-table)
  "Find the subset that maximizes the difference in similarity
   between the topic and the best other object."
  (let ((context (remove topic (objects (get-data agent 'context))))
        (best-subset nil)
        (largest-diff 0)
        (best-similarity 0))
    (dolist (subset subsets)
      (let ((topic-similarity (weighted-similarity-with-table topic subset similarity-table)))
        (when (> topic-similarity 0)
          (let* ((best-other-similarity
                  (loop for object in context
                        maximize (weighted-similarity-with-table object subset similarity-table)))
                 (diff (- topic-similarity best-other-similarity)))
            (when (and (> topic-similarity best-other-similarity) 
                       (> diff largest-diff)
                       (> topic-similarity best-similarity))
              (setf best-subset subset
                    largest-diff diff
                    best-similarity topic-similarity))))))
    best-subset))

