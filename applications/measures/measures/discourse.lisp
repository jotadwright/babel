(in-package :cooking-bot-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Measures regarding Discourse ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Measure: Record questions introduced by irl -- this means the accessible entities

(define-event new-accessible-entities
  (aes list))

(define-monitor questions-introduced-by-discourse
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-introduced-by-discourse new-accessible-entities)
  (multiple-value-bind (questions vars)
      (calculate-questions-introduced-by-discourse aes)
    (record-value monitor questions)
    (setf (slot-value monitor 'values) (cons (list questions vars) (slot-value monitor 'values)))))
                    
(defun calculate-questions-introduced-by-discourse (aes)
  (values (list (length aes)) aes))
 
(define-monitor questions-solved-by-discourse
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-discourse fcg::cip-finished)
  "Event when construction application is finished, monitor for questions solved by discourse,
assumes there is only one solution"
  (multiple-value-bind (number-of-questions vars)
      (calculate-questions-solved-by-discourse cip)
    (record-value monitor number-of-questions)                      
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions vars) (slot-value monitor 'values)))))

(defun calculate-questions-solved-by-discourse (cipn)
  "For every node, calculate the number of accessible entities solved in that node."
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           question-list vars-list)
      (multiple-value-bind (succeeded-node-questions vars)
          (calculate-questions-solved-by-discourse-in-node succeeded-node)
      (push succeeded-node-questions question-list)
      (push vars vars-list)
      (loop for p in (all-parents succeeded-node)
            for (questions vars) = (multiple-value-list (calculate-questions-solved-by-discourse-in-node p))
            do (push questions question-list)
            do (push vars vars-list))
      (values question-list vars-list)))))

(defun calculate-questions-solved-by-discourse-in-node (node)
  "Count number of times that binding-variable is used as a feature in applied construction,
when cxn applied that has binding-variable as feature, a accessible entity is bound"
  (if node
    (if (not (equal (car-status (cipn-car node)) 'fcg::initial))
      (let* ((applied-cxn (original-cxn (car-applied-cxn (cipn-car node))))
             (cond-part (conditional-part applied-cxn))
             (number-of-questions 
              (* 2 (loop for cond-unit in cond-part
                         for compr-lock = (comprehension-lock cond-unit)
                         for binding-vars = (loop for feature in compr-lock
                                                 if (equal (first feature) 'binding-variable)
                                                   collect (second feature))
                         sum (length binding-vars)
                         ;count (unit-feature-value compr-lock 'binding-variable))))
                           )))
             (vars 
              (loop for cond-unit in cond-part
                    for compr-lock = (comprehension-lock cond-unit)
                    collect (unit-feature-value compr-lock 'binding-variable))))
        (values number-of-questions vars))
      (values 0 nil))))