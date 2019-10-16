(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

;;;; Adopting unknown words
(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-words (agent topic category-representation)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-words ((agent mwm-agent) (topic mwm-object) category-representation)
  (let* ((meaning
          (loop with initial-certainty = (get-configuration agent :initial-certainty)
                for (attr . val) in (attributes topic)
                collect (cons (make-category attr val category-representation) initial-certainty)))
        (new-cxn (add-lex-cxn agent (first (utterance agent)) meaning)))
    (add-to-cxn-history agent new-cxn)
    (notify new-cxn-added new-cxn)))

;;;; Aligning known words
(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

(defun filter-uncertain-attributes (concept)
  ;; optimization attempt 1
  (remove-if #'(lambda (certainty) (< certainty 0.1))
             concept :key #'cdr))
                      
(defun find-most-discriminating-subset (agent concept topic)
  "Find the subset that maximizes the difference in similarity
   between the topic and the best other object"
  (let ((subsets (all-subsets (filter-uncertain-attributes concept)))
        (context (remove topic (objects (context agent))))
        best-subset largest-diff)
    (dolist (subset subsets)
      (let ((topic-similarity (weighted-similarity topic subset)))
        (when (> topic-similarity 0) ;; optimization attempt 2
          (let* ((best-other-similarity
                  (loop for object in context
                        maximize (weighted-similarity object subset)))
                 (diff (- topic-similarity best-other-similarity)))
            (when (or (null best-subset) (> diff largest-diff))
              (setf best-subset subset
                    largest-diff diff))))))
    best-subset))
          

(defgeneric align-known-words (agent topic categories)
  (:documentation "Align known words"))

(defmethod align-known-words ((agent mwm-agent) (topic mwm-object)
                              category-representation)
  (declare (ignorable category-representation))
  (let* ((cxn (first (applied-cxns agent)))
         (applied-concept (attr-val cxn :meaning))
         (best-subset
          (find-most-discriminating-subset agent applied-concept topic)))
    (add-to-cxn-history agent cxn)
    (loop for (category . certainty) in applied-concept
          do (update-category category topic
                              :success (communicated-successfully agent)
                              :interpreted-object (topic agent)))
    (loop with rewarded = nil
          with punished = nil
          for (category . certainty) in applied-concept
          if (member category best-subset :key #'car)
          do (progn (push (attribute category) rewarded)
               (adjust-certainty agent cxn (attribute category)
                                 (get-configuration agent :certainty-incf)
                                 :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))
          else
          do (progn (push (attribute category) punished)
               (adjust-certainty agent cxn (attribute category)
                                 (get-configuration agent :certainty-decf)
                                 :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))
          finally
          (notify scores-updated cxn rewarded punished))))
    
        
 
;;;; Align Agent        
(defgeneric align-agent (agent topic)
  (:documentation ""))

(define-event alignment-started (agent mwm-agent))
(define-event adopting-words (words list))
(define-event aligning-words (words list))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let ((utterance-known-p
         (find-cxn-with-form agent (first (utterance agent))))
        (category-representation (get-configuration agent :category-representation)))
    (notify alignment-started agent)
    (if utterance-known-p
      (progn (notify aligning-words (utterance agent))
        (align-known-words agent topic category-representation))
      (progn (notify adopting-words (utterance agent))
        (adopt-unknown-words agent topic category-representation)))))
  