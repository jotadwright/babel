(in-package :visual-dialog)

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
  (multiple-value-bind (number-of-questions question-vars meanings)
      (calculate-questions-introduced-by-grammar cip)
    
    (loop for meaning in (preprocess-meaning-networks meanings) 
           do (do-stuffs meaning))
    (record-value monitor number-of-questions)                      
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions question-vars) (slot-value monitor 'values)))))

(defun do-stuffs (meaning)
  (MP:current-process-pause 2)
  (multiple-value-bind (nodes-to-be-removed edges-to-be-removed)
      (remove-nodes *visual-dialog-inn*)
    (vis-remove-edge (wi::vis-format-many edges-to-be-removed))
    (vis-remove-node (wi::vis-format-many nodes-to-be-removed)))
  (identify-network-updates (make-semantic-network :primitives meaning) *visual-dialog-inn*) 
  (multiple-value-bind (to-add to-update)
      (handle-node-updates *visual-dialog-inn*)
    (vis-add-node (wi::vis-format-many to-add))
    (vis-update-node (wi::vis-format-many to-update)))
  (vis-add-edge (wi::vis-format-many (collect-new-edges *visual-dialog-inn*))))

(defun preprocess-meaning-networks (meanings)
  (let ((table nil)
        (filter-counter 1))
    (loop for meaning in meanings
          collect (loop for predicate in meaning
                   if (equal (first predicate) 'filter-by-attribute)
                     collect (if (assoc (last-elt predicate) table)
                               (substitute (intern (format nil "FILTER-BY-ATTRIBUTE-~a"
                                                           (rest (assoc (last-elt predicate) table))))
                                           'filter-by-attribute
                                           predicate)
                               (progn (push (cons (last-elt predicate) filter-counter) table)
                                 (incf filter-counter)
                                 (substitute (intern (format nil "FILTER-BY-ATTRIBUTE-~a"
                                                           (rest (assoc (last-elt predicate) table))))
                                           'filter-by-attribute
                                           predicate)))
                   else collect predicate))))

                     


(defun calculate-questions-introduced-by-grammar (cipn)
  "Calculate number of questions introduced by grammar for every node in succeeded nodes"
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           questions meanings
           vars)
      (multiple-value-bind
          (questions-succeeded-node vars-succeeded-node meaning) (calculate-questions-introduced-in-node succeeded-node)
      (push questions-succeeded-node questions)
      (push vars-succeeded-node vars)
      (push meaning meanings)
      (loop for p in (all-parents succeeded-node)
            for (q v m) = (multiple-value-list (calculate-questions-introduced-in-node p))
            do (push q questions)
            do (push v vars)
            do (push m meanings))
      (values questions vars meanings)))))

(defun calculate-questions-introduced-in-node (node)
  "returns number of questions that are introduced in that node"
  "e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 7"
  (if node 
    (let* ((extracted-meanings-result (instantiate-meaning-network (extract-meanings ;instantiate the meaning network to get rid of the bind statements
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-resulting-cfs
                                                                       (cipn-car node)))))))
           (extracted-meanings-source (instantiate-meaning-network (extract-meanings
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-source-cfs
                                                                       (cipn-car node)))))))
           (uninstantiated-meanings-results (extract-meanings 
                                             (pole-structure
                                              (left-pole
                                               (car-resulting-cfs
                                                (cipn-car node))))))
           (prims (set-difference extracted-meanings-result extracted-meanings-source :test #'unify-irl-programs))
           (vars (loop for prim in prims
                       if (not (equal (first prim) 'bind))
                       append (rest prim)))
           (vars (loop for var in vars
                       if (variable-p var)
                         collect var
                       else 
                         collect (make-var var)
                         )))
      (values (length vars) vars uninstantiated-meanings-results))))





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
      (values answers vars)))))

#|;; TODO: compare node with previous node, now i look at primitives that are new in that node, but sometimes the primitives already exist and 
(defun calculate-questions-solved-in-node (node)
  "returns number of questions that are solved in that node
e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 3
there are two cases in which vars are considered 'solved', either they are no vars or the var is used in multiple meaning predicates."
  (if node 
    (let* ((extracted-meanings-result (instantiate-meaning-network (extract-meanings
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-resulting-cfs
                                                                       (cipn-car node)))))))
           (extracted-meanings-source (instantiate-meaning-network (extract-meanings
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-source-cfs
                                                                       (cipn-car node)))))))
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
       (append solved-vars constants duplicate-vars-in-resulting-meaning)))))

|#

(defun calculate-questions-solved-in-node (node)
  "returns number of questions that are solved in that node
e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 3
there are two cases in which vars are considered 'solved', either they are no vars or the var is used in multiple meaning predicates."
  (if node 
    (let* ((extracted-meanings-result (instantiate-meaning-network (extract-meanings
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-resulting-cfs
                                                                       (cipn-car node)))))))
           (extracted-meanings-source (instantiate-meaning-network (extract-meanings
                                                                    (pole-structure
                                                                     (left-pole
                                                                      (car-source-cfs
                                                                       (cipn-car node)))))))
           (vars-result (collect-vars extracted-meanings-result))
           (vars-source (collect-vars extracted-meanings-source))
           (unique-vars-result (remove-duplicates vars-result :test #'equal))
           (constants-result (loop for var in vars-result
                                   if (not (variable-p var))
                                     collect var))
           (constants-source (loop for var in vars-source
                                   if (not (variable-p var))
                                     collect var))
           (new-constants (set-difference  constants-result constants-source))
           (new-links-and-vars 
            (loop for var in unique-vars-result
                  for x = (count var vars-result)
                  for y = (count var vars-source)
                  for new-links = (if (= y 0)
                                    (- x 1)
                                    (- (- x 1) (- y 1)))
                  when (and (> x 1) (> x y))
                    sum new-links into num-new-links
                    and append (make-list new-links :initial-element var) into new-link-vars
                  finally (return (list num-new-links new-link-vars)))))
      (values
       (+ (length new-constants) (first new-links-and-vars))
       (append (second new-links-and-vars) new-constants)))))

(defun collect-vars (meaning)
  (loop for prim in meaning
        when (not (equal (first prim) 'bind))
        append (rest prim)))


(defun remove-unique-vars (lst)
  "remove vars that are unique"
  (remove-if
   #'(lambda (x)
       (and (= 1 (count x lst))))
   lst))


;; utils
 
(defun total-questions-introduced-by-grammar ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-introduced-by-grammar) 'values)
        sum (loop for num in (first line)
                      sum num)))


(defun total-questions-solved-by-grammar ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-grammar) 'values)
        sum (loop for num in (first line)
                      sum num)))

(defun instantiate-meaning-network (meaning)
  (let* ((binds (find-all 'bind meaning :key #'first))
         (meaning-without-binds (set-difference meaning binds :test #'unify-irl-programs)))
    (loop for prim in meaning-without-binds
          for potential-bind-var = (last-elt prim)
          for potential-bind-statement = (find potential-bind-var meaning :key #'third)
          when potential-bind-statement
            collect (subst (last-elt potential-bind-statement) potential-bind-var prim))))