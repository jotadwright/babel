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
           (best-subset (find-discriminating-attributes agent
                                                        concept
                                                        topic
                                                        similarity-table
                                                        prototypes)))
      (when (null best-subset)
        ;; when best-subset returns NIL
        ;; reward all attributes...
        (setf best-subset prototypes))
      ;; 3. actually update the weight scores
      (loop for prototype in prototypes
            ;; if part of the contributing prototypes -> reward
            if (member (channel prototype) best-subset :key #'channel)
              do (update-weight prototype
                                (get-configuration (experiment agent) :weight-incf)
                                (get-configuration (experiment agent) :weight-update-strategy))
              ;; otherwise -> punish
            else
              do (update-weight prototype
                                (get-configuration (experiment agent) :weight-decf)
                                (get-configuration (experiment agent) :weight-update-strategy))))))

;; -------------------------------------
;; + Step 1: make the similarity table +
;; -------------------------------------
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
                                 for similarity = (distribution-feature-similarity observation prototype)
                                 for weighted-similarity = (if (not (zerop ledger))
                                                             (* (/ (weight prototype) ledger) similarity)
                                                             0)
                                 do (setf (gethash (id object) hash) (cons similarity weighted-similarity))
                                 finally (return hash))
        do (setf (gethash channel attribute-hash) objects-hash)
        finally (return attribute-hash)))

(defun get-s (object channel table)
  "Retrieve the similarity for the given object-attribute combination."
  (first (gethash (id object) (gethash channel table))))

(defun get-ws (object channel table)
  "Retrieve the weighted similarity for the given object-attribute combination."
  (rest (gethash (id object) (gethash channel table))))

;; --------------------------------------------
;; + Step 2: find the discriminating channels +
;; --------------------------------------------
(defun find-discriminating-attributes (agent concept topic sim-table prototypes)
  "Find all attributes that are discriminating for the topic."
  (loop with context = (remove topic (objects (get-data agent 'context)))
        with discriminating-attributes = nil
        for prototype in prototypes
        for channel = (channel prototype)
        for topic-sim = (get-s topic channel sim-table)
        for best-other-sim = (loop for object in context
                                   maximize (get-s object channel sim-table))
        when (> topic-sim best-other-sim)
          do (push channel discriminating-attributes)
        finally (return discriminating-attributes)))
