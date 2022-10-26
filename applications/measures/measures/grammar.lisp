(in-package :cooking-bot-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Measures regarding grammar ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monitor questions introduced by grammar
(define-monitor questions-introduced-by-grammar
                :class 'data-recorder
                :average-window 1)

(define-event-handler (questions-introduced-by-grammar fcg::cip-finished)
  "Event when cip is finished to record questions introduced by grammar.
Assumes there is only one solution"
  (multiple-value-bind (number-of-questions question-vars)
      (calculate-questions-introduced-by-grammar cip)
    (record-value monitor number-of-questions)                      
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions question-vars) (slot-value monitor 'values)))))

(defun calculate-questions-introduced-by-grammar (cipn)
  "Calculate number of questions introduced by grammar for every node in succeeded nodes"
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           questions
           vars)
      (multiple-value-bind
          (questions-succeeded-node vars-succeeded-node) (calculate-questions-introduced-in-node succeeded-node)
      (push questions-succeeded-node questions)
      (push vars-succeeded-node vars)
      (loop for p in (all-parents succeeded-node)
            for (q v) = (multiple-value-list (calculate-questions-introduced-in-node p))
            do (push q questions)
            do (push v vars))
      (values questions vars)))))

(defun calculate-questions-introduced-in-node (node)
  "returns number of questions that are introduced in that node"
  "e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 7"
  (if node 
    (let* ((extracted-meanings-result (extract-meanings
                                       (pole-structure
                                        (left-pole
                                         (car-resulting-cfs
                                          (cipn-car node))))))
           (extracted-meanings-source (extract-meanings
                                       (pole-structure
                                        (left-pole
                                         (car-source-cfs
                                          (cipn-car node))))))
           (prims (set-difference extracted-meanings-result extracted-meanings-source :test #'unify-irl-programs))
           (vars (loop for prim in prims
                       append (rest prim)))
           (vars (loop for var in vars
                       if (variable-p var)
                         collect var
                       else 
                         collect (make-var var))))
      (values (length vars) vars))))


;; Monitor questions solved by grammar
(define-monitor questions-solved-by-grammar
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-grammar fcg::cip-finished)
  "Event when cip is finished to record questions solved by grammar.
Assumes there is only one solution"
  (multiple-value-bind (number-of-answers answer-vars)
      (calculate-questions-solved-by-grammar cip)
    (record-value monitor number-of-answers)                      
    (setf (slot-value monitor 'values)
          (cons (list number-of-answers answer-vars) (slot-value monitor 'values)))))

(defun calculate-questions-solved-by-grammar (cipn)
  "Calculate number of questions solved by grammar for every node in succeeded nodes"
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           answers
           vars)
      (multiple-value-bind (number-of-answers answer-vars)
          (calculate-questions-solved-in-node succeeded-node)
      (push number-of-answers answers)
      (push answer-vars vars)
      (loop for p in (all-parents succeeded-node)
            for (number-of-answers answer-vars) = (multiple-value-list (calculate-questions-solved-in-node p))
            do (push number-of-answers answers)
            do (push answer-vars vars))
      (values answers vars )))))

(defun calculate-questions-solved-in-node (node)
  "returns number of questions that are solved in that node
e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 3
there are two cases in which vars are considered 'solved', either they are no vars or the var is used in multiple meaning predicates."
  (if node 
    (let* ((extracted-meanings-result (extract-meanings
                                       (pole-structure
                                        (left-pole
                                         (car-resulting-cfs
                                          (cipn-car node))))))
           (extracted-meanings-source (extract-meanings
                                       (pole-structure
                                        (left-pole
                                         (car-source-cfs
                                          (cipn-car node))))))
           (resulting-prims (set-difference extracted-meanings-result extracted-meanings-source :test #'unify-irl-programs))
           (resulting-vars (loop for meaning in resulting-prims
                                 append (rest meaning)))
           (source-vars (loop for meaning in extracted-meanings-source
                                 append (rest meaning)))
           (solved-vars (intersection resulting-vars source-vars))
           (constants (loop for el in resulting-vars
                            if (not (variable-p el))
                              collect el))
           (duplicate-vars-in-resulting-meaning (remove-duplicates (remove-unique-vars resulting-vars))))
      (values 
       (+ (length solved-vars) (length constants) (length duplicate-vars-in-resulting-meaning))
       (append solved-vars constants duplicate-vars-in-resulting-meaning))
      )))

(defun remove-unique-vars (lst)
  "remove vars that are unique"
  (remove-if
   #'(lambda (x)
       (and (= 1 (count x lst))))
   lst))



