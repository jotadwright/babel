(in-package :robot-concept-learning)

;; -------------
;; + Alignment +
;; -------------

;;;; Adopting unknown words
(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-word (agent topic category-representation)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-word ((agent mwm-agent) (topic mwm-object) category-representation)
  (declare (ignorable category-representation))
  (let* ((meaning
          (loop with initial-certainty = (get-configuration agent :initial-certainty)
                with category-representation = (get-configuration agent :category-representation)
                for (attr . val) in (attributes topic)
                collect (cons (make-category attr val category-representation) initial-certainty)))
         (new-cxn (add-lex-cxn agent (utterance agent) meaning)))
    (add-to-cxn-history agent new-cxn)
    (notify new-cxn-added new-cxn)))

;;;; Aligning known words
(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

(defgeneric align-known-word (agent topic category-representation)
  (:documentation "Align known words"))

(defun discriminatingp (agent category topic)
  "A category is discriminating for the topic if its similarity
   to the topic is higher than for any other object in the context"
  (let* ((context (remove topic (objects (context agent))))
         (topic-sim (similarity topic category))
         (best-object-sim (apply #'max
                                 (loop for obj in context
                                       collect (similarity obj category)))))
    (> topic-sim best-object-sim)))

(defmethod align-known-word ((agent mwm-agent) (topic mwm-object)
                              category-representation)
  (declare (ignorable category-representation))
  (let ((meaning (attr-val (applied-cxn agent) :meaning)))
    (loop with punished = nil
          with rewarded = nil
          for (category . certainty) in meaning
          for attr = (attribute category)
          if (discriminatingp agent category topic)
          do (progn (push attr rewarded)
               (adjust-certainty agent (applied-cxn agent) attr (get-configuration agent :certainty-incf)))
          else
          do (progn (push attr punished)
               (adjust-certainty agent (applied-cxn agent) attr (get-configuration agent :certainty-decf)))
          finally
          (notify scores-updated (applied-cxn agent) rewarded punished))
    (loop for (category . certainty) in meaning
          do (update-category category topic
                              :success (communicated-successfully agent)
                              :interpreted-object (topic agent)))
    (add-to-cxn-history agent (applied-cxn agent))))    
        
 
;;;; Align Agent        
(defgeneric align-agent (agent topic)
  (:documentation ""))

(define-event alignment-started (agent mwm-agent))
(define-event update-known-word (agent mwm-agent))
(define-event adopt-unknown-word (agent mwm-agent))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let ((utterance-known-p
         (find (utterance agent) (grammar agent)
               :key #'(lambda (cxn) (attr-val cxn :form))
               :test #'string=))
        (category-representation
         (get-configuration agent :category-representation)))
    (notify alignment-started agent)
    (if utterance-known-p
      (progn (notify update-known-word agent)
        (align-known-words agent topic category-representation))
      (progn (notify adopt-unknown-word agent)
        (adopt-unknown-word agent topic category-representation)))))
  