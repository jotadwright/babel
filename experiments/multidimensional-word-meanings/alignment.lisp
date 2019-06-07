(in-package :mwm)

;; -------------
;; + Alignment +
;; -------------

;;;; Adopting unknown words
(define-event new-cxn-added (cxn fcg-construction))

(defgeneric adopt-unknown-words (agent topic words)
  (:documentation "Adopt unknown words"))

(defmethod adopt-unknown-words ((agent mwm-agent) (topic mwm-object) words)
  (let ((meaning
         (loop with initial-certainty = (get-configuration agent :initial-certainty)
               with category-representation = (get-configuration agent :category-representation)
               for (attr . val) in (attributes topic)
               collect (cons (make-category attr val category-representation) initial-certainty))))
    (loop for word in words
          for new-cxn = (add-lex-cxn agent word meaning)
          do (notify new-cxn-added new-cxn))))

;;;; Aligning known words
(define-event scores-updated (cxn fcg-construction)
  (rewarded-attrs list)
  (punished-attrs list))

(define-event re-introduced-meaning (cxn fcg-construction)
  (attrs list))

(defun get-cxn-from-category (agent category)
  "Find the cxn that the category belongs to"
  (let ((found (find-all category (applied-cxns agent)
                         :key #'(lambda (cxn)
                                  (mapcar #'car (attr-val cxn :meaning)))
                         :test #'member)))
    (if (length= found 1)
      (first found)
      (the-biggest #'(lambda (cxn)
                       (cdr (find category (attr-val cxn :meaning) :key #'car)))
                   found))))

(defgeneric align-known-words (agent topic categories)
  (:documentation "Align known words"))

;; for the min-max strategy
(defmethod align-known-words ((agent mwm-agent) (topic mwm-object)
                              (categories (eql :min-max)))
  (loop with rewarded
        with punished
        for (category . certainty) in (parsed-meaning agent)
        for cxn = (get-cxn-from-category agent category)
        for attr = (attribute category)
        ;; update the prototype
        do (update-category category topic
                            :success (communicated-successfully agent)
                            :interpreted-object (topic agent))
        ;; punish or reward based on similarity
        do (let ((sim (similarity topic category)))
             (if (>= sim 0)
               (progn (push attr rewarded)
                 (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf)
                                   :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))
               (progn (push attr punished)
                 (adjust-certainty agent cxn attr (get-configuration agent :certainty-decf)
                                   :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound)))))
        ;; notify
        finally
        (notify scores-updated cxn rewarded punished)))

;; for the other strategies
(defun discriminatingp (agent category topic)
  "A category is discriminating for the topic if its similarity
   to the topic is higher than for any other object in the context"
  (let* ((context (remove topic (objects (context agent))))
         (topic-sim (similarity topic category))
         (best-object-sim (apply #'max
                                 (loop for obj in context
                                       collect (similarity obj category)))))
    (> topic-sim best-object-sim)))

(defmethod align-known-words ((agent mwm-agent) (topic mwm-object)
                              categories)
  (declare (ignorable categories))
  ;; compute for each attribute in the meaning the cxn
  ;; where it came from
  (let ((cxns-with-categories
         (if (length= (applied-cxns agent) 1)
           (list
            (cons (first (applied-cxns agent))
                  (mapcar #'car (parsed-meaning agent))))
           (loop with result = nil
                 for (category . certainty) in (parsed-meaning agent)
                 for cxn = (get-cxn-from-category agent category)
                 if (assoc cxn result)
                 do (push category (cdr (assoc cxn result)))
                 else
                 do (push (cons cxn (list category)) result)
                 finally
                 (return result)))))
    ;; loop over all cxns with their categories
    (loop for (cxn . categories) in cxns-with-categories
          for punished = nil
          for rewarded = nil
          ;; for each category, reward if it is discriminating
          ;; and otherwise punish. The value is always updated
          do (loop for category in categories
                   for attr = (attribute category)
                   if (discriminatingp agent category topic)
                   do (progn (push attr rewarded)
                        (adjust-certainty agent cxn attr (get-configuration agent :certainty-incf)
                                          :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound))
                        (update-category category topic
                                         :success (communicated-successfully agent)
                                         :interpreted-object (topic agent)))
                   else
                   do (progn (push attr punished)
                        (adjust-certainty agent cxn attr (get-configuration agent :certainty-decf)
                                          :remove-on-lower-bound (get-configuration agent :remove-on-lower-bound))
                        (update-category category topic
                                         :success (communicated-successfully agent)
                                         :interpreted-object (topic agent))))
          do (notify scores-updated cxn rewarded punished))))
    
        
 
;;;; Align Agent        
(defgeneric align-agent (agent topic)
  (:documentation
   "The alignment procedure can be split in 2 cases:
   1. Adopting unknown words. The meaning of these unknown words
      will be a cloud containing all attributes of the object. The
      value for each attribute will be the exact value of the
      topic (pointed to by the tutor). These values are linked with
      the new word using an initial certainty score.
   2. Aligning known words. For each word in the utterance, the
      agent will reward and punish certain attributes, based on
      the similarity measure. The attributes with positive similarity
      will not only be rewarded; their value will also be shifted
      towards the topic."))

(define-event alignment-started (agent mwm-agent))
(define-event adopting-words (words list))
(define-event aligning-words (words list))

(defmethod align-agent ((agent mwm-agent) (topic mwm-object))
  (let* ((known-words
          (loop for form in (utterance agent)
                when (find-cxn-with-form agent form)
                collect form))
         (unknown-words
          (set-difference (utterance agent) known-words :test #'string=))
         (category-representation (get-configuration agent :category-representation)))
    (notify alignment-started agent)
    (when unknown-words
      (notify adopting-words unknown-words)
      (adopt-unknown-words agent topic unknown-words))
    (when known-words
      (notify aligning-words known-words)
      (align-known-words agent topic category-representation))))
  