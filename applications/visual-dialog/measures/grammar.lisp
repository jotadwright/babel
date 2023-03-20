(in-package :visual-dialog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Measures regarding grammar ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Monitor that calculates and plots the variables that are introduced by grammar.


(define-monitor questions-introduced-by-grammar
                :class 'data-recorder
                :average-window 1)

(define-event-handler (questions-introduced-by-grammar fcg::cip-finished)
  "Event when cip is finished to record questions introduced by grammar.
Assumes there is only one solution"
  (multiple-value-bind (number-of-questions question-vars meanings resulting-bindings)
      (calculate-questions-introduced-by-grammar cip)
    (let ((processed-meaning
           (preprocess-meaning-networks meanings resulting-bindings)))
      (loop for meaning in processed-meaning
            for binding in resulting-bindings
            do (visualise-link binding)
            do (visualise-meaning meaning))
      (record-value monitor number-of-questions)                      
      (setf (slot-value monitor 'values)
            (cons (list number-of-questions question-vars) (slot-value monitor 'values))))))
  
            
(defun calculate-questions-introduced-by-grammar (cipn)
  "Calculate number of questions introduced by grammar for every node in succeeded nodes"
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           questions meanings vars)
      (multiple-value-bind
          (questions-succeeded-node vars-succeeded-node meaning resulting-bindings)
          (calculate-questions-introduced-in-node succeeded-node)
        (setf resulting-bindings (list resulting-bindings))
        (push questions-succeeded-node questions)
        (push vars-succeeded-node vars)
        (push meaning meanings)
        (loop for p in (all-parents succeeded-node)
              for (q v m b) = (multiple-value-list (calculate-questions-introduced-in-node p))
              do (push q questions)
              do (push v vars)
              do (push m meanings)
              do (push b resulting-bindings))
        (values questions vars meanings resulting-bindings)))))


(defun calculate-questions-introduced-in-node (node)
  "returns number of questions that are introduced in that node"
  "e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 7"
  (if node 
    (let* ((extracted-meanings-result (extract-meanings ;instantiate the meaning network to get rid of the bind statements
                                       (pole-structure
                                        (left-pole
                                         (car-resulting-cfs
                                          (cipn-car node))))))
           (extracted-meanings-source (extract-meanings
                                       (pole-structure
                                        (left-pole
                                         (car-source-cfs
                                          (cipn-car node))))))
           (uninstantiated-meanings-results (extract-meanings 
                                             (pole-structure
                                              (left-pole
                                               (car-resulting-cfs
                                                (cipn-car node))))))
           (resulting-bindings (car-second-merge-bindings (cipn-car node)))
           (result-vars (collect-irl-vars extracted-meanings-result))
           (source-vars (collect-irl-vars extracted-meanings-source))
           ;(unique-source-vars (remove-duplicates source-vars))
           ; replace vars with resulting-bindings
           (resulting-source-vars (replace-vars source-vars resulting-bindings))
           (new-vars (multiset-difference resulting-source-vars result-vars))
           (used-resulting-bindings (collect-resulting-bindings source-vars resulting-bindings)))
      (values (length new-vars) new-vars uninstantiated-meanings-results used-resulting-bindings))))




;; Monitor that calculates and plots the variables that are solved by grammar.
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


(defun calculate-questions-solved-in-node (node)
  "returns number of questions that are solved in that node
   e.g., (fetch-and-prop ?ingr ?ks-out ?ks-in ?target butter 226 g)  --> 3
   there are two cases in which vars are considered 'solved',
   either they are no vars or the var is used in multiple meaning predicates."
  (when node 
    (let* ((extracted-meanings-result  (extract-meanings
                                        (pole-structure
                                         (left-pole
                                          (car-resulting-cfs
                                           (cipn-car node))))))
           (extracted-meanings-source  (extract-meanings
                                        (pole-structure
                                         (left-pole
                                          (car-source-cfs
                                           (cipn-car node))))))
           (vars-result (collect-irl-vars extracted-meanings-result))
           (vars-source (collect-irl-vars extracted-meanings-source))
           (resulting-bindings (car-second-merge-bindings (cipn-car node)))
           (unique-vars-result (remove-duplicates vars-result :test #'equal))
           (constants-result (loop for var in vars-result
                                   if (not (variable-p var))
                                     collect var))
           (constants-source (loop for var in vars-source
                                   if (not (variable-p var))
                                     collect var))
           (new-constants (set-difference constants-result constants-source))
           (new-links-and-vars ;; vergelijk voor elke variabele hoe vaak die voorkomt in het result en in de source
                               ;; maar hou rekening met de renamings van de variabelen in de source
                               ;; (- (- x 1) (- y 1)) --> vb x = 4, y = 3 > 1 opgeloste vraag,
                               ;; want er is 1 variabele bijgekomen die onmiddellijk gebonden wordt
                               ;; dus ofwel komen er primitieven bij met variabelen die gevonden worden aan een bestaande variabele,
                               ;; ofwel zorgen cxns ervoor dat variabelen in primitieven gebonden worden (resulting-bindings),
                               ;; als een variabele al meerdere keren voorkomt in de source, hou daar dan rekening mee
                               ;; in case of verwarring --> vraag jens
            (loop for var in unique-vars-result 
                  for x = (count var vars-result)
                  for y = (max (count var vars-source)
                               (count (first (rassoc var resulting-bindings)) vars-source))
                  for new-links = (if (= y 0)
                                    (- x 1)
                                    (- (- x 1) (- y 1)))
                  when (and (> x 1) (> x y))
                    sum new-links into num-new-links
                    and append (make-list new-links :initial-element var) into new-link-vars
                  finally (return (list num-new-links new-link-vars)))))
      (values
       (first new-links-and-vars)
       (append (second new-links-and-vars) new-constants)))))



