(in-package :mwm)

;;;; Adopting unknown words
(define-event new-concept-added (concept concept))

(defgeneric adopt-concept (agent topic word)
  (:documentation "Adopt an unknown word by making a concept
   with this word + all feature-values of the topic object.
   Each feature receives an initial certainty value. The
   newly created concept is added to the agent's lexicon."))

(defmethod adopt-concept ((agent mwm-agent) (topic mwm-object) word)                
  (let ((new-concept
         (make-concept word (attributes topic)
                       (get-configuration agent :initial-certainty))))
    (push new-concept (lexicon agent))
    (notify new-concept-added new-concept)
    new-concept))




;;;; Aligning known concept
(define-event scores-updated (concept concept) (rewarded-attrs list) (punished-attrs list))
(define-event found-discriminating-attributes (attributes list))
(define-event found-subset-to-reward (subset list))

;; compute the (weighted) similarities between the concept
;; and all objects in the scene, for all attributes of the concept
;; and store them/re-use them to compute the discriminative
;; attributes and to find the most discriminative subset
;; this saves a lot of computations!!
(defun make-similarity-table (agent concept)
  (loop with attribute-hash = (make-hash-table)
        for prototype in (meaning concept)
        for attribute = (attribute prototype)
        for objects-hash
        = (loop with hash = (make-hash-table)
                for object in (objects (get-data agent 'context))
                for s = (similarity object prototype)
                for ws = (* (certainty prototype) s)
                do (setf (gethash (id object) hash) (cons s ws))
                finally (return hash))
        do (setf (gethash attribute attribute-hash) objects-hash)
        finally (return attribute-hash)))

;; retrieve the similarity for the given object-attribute combination
(defun get-s (object attribute table)
  (car (gethash (id object) (gethash attribute table))))

;; retrieve the weighted similarity for the given object-attribute combination
(defun get-ws (object attribute table)
  (cdr (gethash (id object) (gethash attribute table))))

(defun find-discriminating-attributes (agent concept topic similarity-table)
  "Find all attributes that are discriminating for the topic"
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
          (progn (notify found-discriminating-attributes discriminating-attributes)
            (return discriminating-attributes)))))


(defmethod filter-subsets (all-subsets discriminating-attributes (mode (eql :none)))
  (declare (ignorable discriminating-attributes))
  all-subsets)

(defmethod filter-subsets (all-subsets discriminating-attributes (mode (eql :all)))
  "Filter all subsets with the discriminating attributes, only
   keeping those subsets where all discriminating attributes occur in"
  (loop with applicable-subsets = nil
        for subset in all-subsets
        for subset-attributes
        = (mapcar #'attribute subset)
        when (null (set-difference discriminating-attributes subset-attributes))
        do (push subset applicable-subsets)
        finally
        (return applicable-subsets)))

(defmethod filter-subsets (all-subsets discriminating-attributes (mode (eql :at-least-one)))
  "Filter all subsets with the discriminating attributes, only
   keeping those subsets where at least one discriminating attribute occurs in"
  (loop with applicable-subsets = nil
        for subset in all-subsets
        for subset-attributes
        = (mapcar #'attribute subset)
        unless (null (intersection discriminating-attributes subset-attributes))
        do (push subset applicable-subsets)
        finally
        (return applicable-subsets)))


(defun weighted-similarity-with-table (object list-of-prototypes table)
  (loop for prototype in list-of-prototypes
        for attribute = (attribute prototype)
        for ws = (get-ws object attribute table)
        collect ws into weighted-similarities
        finally (return (average weighted-similarities))))


(defun find-most-discriminating-subset (agent subsets topic similarity-table)
  "Find the subset that maximizes the difference in similarity
   between the topic and the best other object"
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
                       (< best-other-similarity 0)
                       (> diff largest-diff)
                       (> topic-similarity best-similarity))
              (setf best-subset subset
                    largest-diff diff
                    best-similarity topic-similarity))))))
    (notify found-subset-to-reward best-subset)
    best-subset))


(defgeneric align-concept (agent topic concept)
  (:documentation "Align the concept that was used during the
   game so that it better fits with the topic object of the
   game."))


(defmethod align-concept ((agent mwm-agent) (topic mwm-object) concept)
  ;; 1. update the prototypical values
  (loop for prototype in (meaning concept)
        do (update-prototype prototype topic))
  ;; 2. determine which attributes should get an increase
  ;;    in certainty, and which should get a decrease.
  (let* ((similarity-table
          (make-similarity-table agent concept))
         (discriminating-attributes
          (find-discriminating-attributes
           agent concept topic similarity-table))
         (all-attribute-subsets
          (all-subsets (meaning concept)))
         (subsets-to-consider
          (filter-subsets
           all-attribute-subsets discriminating-attributes
           (get-configuration agent :alignment-filter)))
         (best-subset
          (find-most-discriminating-subset
           agent subsets-to-consider topic similarity-table)))
    (add-to-concept-history agent concept)
    ;; 3. actually update the certainty scores
    (loop with rewarded-attributes = nil
          with punished-attributes = nil
          for prototype in (meaning concept)
          if (member (attribute prototype) best-subset :key #'attribute)
          do (progn (push (attribute prototype) rewarded-attributes)
               (adjust-certainty agent concept (attribute prototype)
                                 (get-configuration agent :certainty-incf)))
          else
          do (progn (push (attribute prototype) punished-attributes)
               (adjust-certainty agent concept (attribute prototype)
                                 (get-configuration agent :certainty-decf)
                                 :remove-on-lower-bound
                                 (get-configuration agent :remove-on-lower-bound)))
          finally
          (notify scores-updated concept rewarded-attributes punished-attributes))))


;; -------------
;; + Alignment +
;; -------------
(defgeneric alignment (agent topic applied-concept)
  (:documentation "Do alignment"))

(define-event align-concept-started (word string))
(define-event adopt-concept-started (word string))

(defmethod alignment ((agent mwm-agent) (topic mwm-object) applied-concept)
  ;; applied-concept can be NIL
  (if applied-concept
    (progn (notify align-concept-started (form applied-concept))
      (align-concept agent topic applied-concept))
    (progn (notify adopt-concept-started (utterance agent))
      (adopt-concept agent topic (utterance agent)))))
        
  