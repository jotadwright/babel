(in-package :cle)

;; ---------------------
;; + Shifting concepts +
;; ---------------------

(defmethod shift-concept ((agent cle-agent) (topic cle-object) (concept concept))
  "Shift a concept towards the given topic.

  Shifts 1. the prototype of each feature channel
         2. the certainties of salient channel positively and others negatively."
  (let ((prototypes (get-available-prototypes agent concept)))
    ;; 1. update the prototypical values
    (loop for prototype in prototypes ;; assumes prototype
          for interaction-number = (interaction-number (current-interaction (experiment agent)))
          for new-observation = (perceive-object-val agent topic (channel prototype))
          do (update-prototype new-observation
                               interaction-number
                               prototype
                               :save-distribution-history (get-configuration (experiment agent) :save-distribution-history)))
  
    ;; 2. determine which attributes should get an increase
    ;;    in weight, and which should get a decrease.
    (let* ((similarity-table (make-similarity-table agent concept prototypes))
           (discriminating-attributes (find-discriminating-attributes agent
                                                                      concept
                                                                      topic
                                                                      similarity-table
                                                                      prototypes))
           (subsets-to-consider (get-all-subsets prototypes discriminating-attributes))
           (best-subset (find-most-discriminating-subset agent
                                                         subsets-to-consider
                                                         topic
                                                         similarity-table)))
      (when (null best-subset)
        ;; when best-subset returns NIL
        ;; reward all attributes...
        (setf best-subset prototypes))
      ;; 3. actually update the weight scores
      (loop for prototype in prototypes
            ;; if part of the contributing prototypes -> reward
            if (member (channel prototype) best-subset :key #'channel)
              do (progn
                 ;(update-history-weight agent prototype (get-configuration (experiment agent) :weight-incf))        
                   (update-weight prototype
                                  (get-configuration (experiment agent) :weight-incf)
                                  (get-configuration (experiment agent) :weight-update-strategy)))
              ;; otherwise -> punish
            else
              do (progn
                 ;(update-history-weight agent prototype (get-configuration (experiment agent) :weight-decf))        
                   (update-weight prototype
                                  (get-configuration (experiment agent) :weight-decf)
                                  (get-configuration (experiment agent) :weight-update-strategy)))))))

;; -----------------------
;; + Utils for alignment +
;; -----------------------

;; events
(define-event event-found-discriminating-attributes (attributes list))
(define-event event-found-subset-to-reward (subset list))

;; --------------------
;; + Similarity table +
;; --------------------
(defun make-similarity-table (agent concept prototypes)
  "Compute the (weighted) similarities between the concept
   and all objects in the scene, for all attributes of the concept
   and store them/re-use them to compute the discriminative
   attributes and to find the most discriminative subset.
                   
   Saves tons in computation by only calculating it only once."
  (loop with attribute-hash = (make-hash-table)
        for prototype in prototypes
        for ledger = (loop for prototype in prototypes sum (weight prototype))
        for channel = (channel prototype)
        for objects-hash = (loop with hash = (make-hash-table)
                                 for object in (objects (get-data agent 'context))
                                 for observation = (perceive-object-val agent object channel)
                                 for similarity = (observation-similarity observation prototype)
                                 for weighted-similarity = (if (and (not (zerop ledger)) similarity)
                                                             (* (/ (weight prototype) ledger) similarity)
                                                             0)
                                 do (setf (gethash (id object) hash) (cons similarity weighted-similarity))
                                 finally (return hash))
        do (setf (gethash channel attribute-hash) objects-hash)
        finally (return attribute-hash)))

(defun get-all-subsets (all-attr subset-attr)
  "Given a set of attributes and a subset of that set, returns all
   subsets of the complete set that contain the subset."
  
  (let* ((rest-attr (loop for el in all-attr
                          if (not (find (channel el) subset-attr))
                            collect el)))
    (if (length> rest-attr 6)
      (list (loop for el in all-attr
                  if (find (channel el) subset-attr)
                    collect el))
      (let* ((all-subsets-of-rest (cons '() (all-subsets rest-attr)))
             (subset-attr-values (loop for el in all-attr
                                       if (find (channel el) subset-attr)
                                         collect el))
             (all-subsets (loop for el in all-subsets-of-rest
                                collect (append subset-attr-values el))))
        all-subsets))))

(defun get-s (object channel table)
  "Retrieve the similarity for the given object-attribute combination."
  (first (gethash (id object) (gethash channel table))))

(defun get-ws (object channel table)
  "Retrieve the weighted similarity for the given object-attribute combination."
  (rest (gethash (id object) (gethash channel table))))

(defun find-discriminating-attributes (agent concept topic similarity-table prototypes)
  "Find all attributes that are discriminating for the topic."
  (loop with context = (remove topic (objects (get-data agent 'context)))
        with threshold = (get-configuration (experiment agent) :similarity-threshold)
        with discriminating-attributes = nil
        for prototype in prototypes
        for channel = (channel prototype)
        for topic-similarity = (get-s topic channel similarity-table)
        for best-other-similarity = (loop for object in context
                                          maximize (get-s object channel similarity-table))
        when (> topic-similarity (+ best-other-similarity threshold))
          do (push channel discriminating-attributes)
        finally
          (progn
            (notify event-found-discriminating-attributes discriminating-attributes)
            (return discriminating-attributes))))


;; -----------------------------
;; + Weighted Similarity table +
;; -----------------------------
(defun weighted-similarity-with-table (object list-of-prototypes table)
  (loop for prototype in list-of-prototypes
        for channel = (channel prototype)
        for ws = (get-ws object channel table)
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
    (notify event-found-subset-to-reward best-subset)
    best-subset))
