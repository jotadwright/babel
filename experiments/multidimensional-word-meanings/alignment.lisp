(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

;;;; Adopting unknown words
(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-words (agent topic words category-representation)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-words ((agent mwm-agent) (topic mwm-object) words category-representation)
  (loop for word in words
        for new-meaning
        = (loop with initial-certainty = (get-configuration agent :initial-certainty)
                for (attr . val) in (attributes topic)
                collect (cons (make-category attr val category-representation) initial-certainty))
        for new-cxn = (add-lex-cxn agent word new-meaning)
        do (add-to-cxn-history agent new-cxn)
        do (notify new-cxn-added new-cxn)))

;;;; Aligning known words
(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))
(define-event found-discriminating-attributes
  (attributes list))
(define-event found-subset-to-reward
  (subset list))

(defun find-discriminating-attributes (agent concept topic)
  "Find all attributes that are discriminating for the topic"
  (let ((context (remove topic (objects (context agent)))))
    (loop with discriminating-attributes = nil
          for (category . certainty) in concept
          for topic-similarity = (similarity topic category)
          for best-other-similarity
          = (loop for object in context
                  maximize (similarity object category))
          when (> topic-similarity best-other-similarity)
          do (push (attribute category) discriminating-attributes)
          finally
          (progn (notify found-discriminating-attributes discriminating-attributes)
            (return discriminating-attributes)))))


(defun filter-subsets (all-subsets discriminating-attributes)
  "Filter all subsets with the discriminating attributes, only
   keeping those subsets where all discriminating attributes
   occur in"
  (loop with applicable-subsets = nil
        for subset in all-subsets
        for subset-attributes
        = (mapcar #'attribute (mapcar #'car subset))
        when (null (set-difference discriminating-attributes subset-attributes))
        do (push subset applicable-subsets)
        finally
        (return applicable-subsets)))
        
                      
(defun find-most-discriminating-subset (agent subsets topic)
  "Find the subset that maximizes the difference in similarity
   between the topic and the best other object"
  (let ((context (remove topic (objects (context agent))))
        best-subset largest-diff)
    (dolist (subset subsets)
      (let ((topic-similarity (weighted-similarity topic subset)))
        (when (> topic-similarity 0)
          (let* ((best-other-similarity
                  (loop for object in context
                        maximize (weighted-similarity object subset)))
                 (diff (- topic-similarity best-other-similarity)))
            (when (or (null best-subset)
                      (> diff largest-diff))
              (setf best-subset subset
                    largest-diff diff))))))
    (notify found-subset-to-reward best-subset)
    best-subset))
          

(defgeneric align-known-words (agent topic words categories)
  (:documentation "Align known words"))

;; find the set of all attributes that are discriminating (S)
;; compute all subsets of all attributes (A)
;; only consider the subsets of which the set-difference
;; between S and A is empty

(defmethod align-known-words ((agent mwm-agent) (topic mwm-object)
                              words category-representation)
  (declare (ignorable category-representation))
  ;; update prototype
  (loop for (category . certainty) in (parsed-meaning agent)
        do (update-category category topic
                            :success (communicated-successfully agent)
                            :interpreted-object (topic agent)))
  ;; update certainties
  (let* ((discriminating-attributes
          (find-discriminating-attributes agent (parsed-meaning agent) topic))
         (all-subsets (all-subsets (parsed-meaning agent)))
         (subsets-to-consider
          (filter-subsets all-subsets discriminating-attributes))
         (best-subset
          (find-most-discriminating-subset agent subsets-to-consider topic)))
    ;; store the rewarded and punished attributes per cxn
    (loop with rewarded = (make-blackboard)
          with punished = (make-blackboard)
          for (category . certainty) in (parsed-meaning agent)
          if (member category best-subset :key #'car)
          do (let ((cxn (construction category)))
               (push-data rewarded (name cxn) (attribute category))
               (add-to-cxn-history agent cxn)
               (adjust-certainty agent cxn (attribute category)
                                 (get-configuration agent :certainty-incf)
                                 :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))
          else
          do (let ((cxn (construction category)))
               (push-data punished (name cxn) (attribute category))
               (add-to-cxn-history agent cxn)
               (adjust-certainty agent cxn (attribute category)
                                 (get-configuration agent :certainty-decf)
                                 :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))
          finally
          (loop for cxn in (applied-cxns agent)
                for rewarded-attrs = (find-data rewarded (name cxn))
                for punished-attrs = (find-data punished (name cxn))
                when (or rewarded-attrs punished-attrs)
                do (notify scores-updated cxn rewarded-attrs punished-attrs))))) 
        
 
;;;; Align Agent        
(defgeneric align-agent (agent topic)
  (:documentation ""))

(define-event alignment-started (agent mwm-agent))
(define-event adopting-words (words list))
(define-event aligning-words (words list))

;; When all unknown words; adopt them
;; When all known words; align them
;; When both known and unknown words; combine them?

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let* ((known-words
          (find-all-if #'(lambda (form)
                           (find-cxn-with-form agent form))
                       (utterance agent)))
         (unknown-words
          (set-difference (utterance agent) known-words))
         (category-representation
          (get-configuration agent :category-representation)))
    (notify alignment-started agent)
    (cond
     ((and (null known-words) unknown-words)
      (notify adopting-words (utterance agent))
      (adopt-unknown-words agent topic unknown-words category-representation))
     ((and known-words (null unknown-words))
      (notify aligning-words (utterance agent))
      (align-known-words agent topic known-words category-representation))
     (t ;; first adopt the unknown words
        (notify adopting-words unknown-words)
        (adopt-unknown-words agent topic unknown-words category-representation)
        ;; align the known words 
        (notify aligning-words known-words)
        (align-known-words agent topic known-words category-representation)
        ;; find out which attributes were rewarded for the known words
        ;; and also reward these for the new, unknown words
        ))))
        
  