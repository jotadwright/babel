(in-package :visual-dialog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Measures regarding Perception ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *inference-primitives* (list 'count-objects 'exist-or-count 'exist 'find-in-context 'more-than-1 'select-one 'set-diff 'unique)) ;; inference ipv reasoning
(setf *discourse-primitives* (list 'get-last-attribute-category 'get-last-topic 'get-penultimate-topic))
(setf *perception-primitives* (list 'extreme-relate 'immediate-relate 'query 'relate 'segment-scene))
(setf *discourse-and-perception-primitives* (list 'filter-by-attribute))



;;;;; Measure: Record questions solved by irl (perception)

#|(define-event perception-used
  (binds list))

(define-monitor questions-solved-by-perception
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-perception perception-used)
  (multiple-value-bind (questions vars)
      (calculate-questions-solved-by-perception binds)
    (record-value monitor questions)
    (setf (slot-value monitor 'values) (cons (list questions vars) (slot-value monitor 'values)))))
                    
(defun calculate-questions-solved-by-perception (binds)
  (values (list (length binds)) binds))
 
(define-monitor questions-solved-by-perception
                :class 'data-recorder :average-window 1)|#


(define-monitor questions-solved-by-mental-simulation
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-mental-simulation irl::evaluate-irl-program-finished)
  (multiple-value-bind (number-of-questions answer-vars)
      (calculate-questions-solved-by-mental-simulation irl::succeeded-nodes)
   ; (multiple-value-bind (answers vars)
   ;   (find-solved-slots-by-simulation (calculate-set-slots irl::solution-nodes *persistent-id-table-solved*))
   ;   (let ((total-number-of-questions (combine number-of-questions answers)))
    (test answer-vars)
    (loop for answer-var in answer-vars
            do (loop for iets in answer-var
                       with binding = (last-elt (last-elt iets))
                       do (do-more-stuffs binding)))
    (record-value monitor number-of-questions)
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions answer-vars) (slot-value monitor 'values)))))


(defun test (answer-vars)
  (loop for answer-var in answer-vars
        do (loop for var in answer-var
                 for binding = (when (not (equal var :no-new-bindings)) (last-elt var))
                 when binding
                   do (do-more-stuffs binding))))
  

(defun do-more-stuffs (binding)
  (MP:current-process-pause 2)
  (identify-network-updates binding *visual-dialog-inn*) 
  (multiple-value-bind (to-add to-update)
      (handle-node-updates *visual-dialog-inn*)
    (vis-add-node (wi::vis-format-many to-add))
    (vis-update-node (wi::vis-format-many to-update)))
  (vis-add-edge (wi::vis-format-many (collect-new-edges *visual-dialog-inn*))))



(defun calculate-questions-solved-by-mental-simulation (solution-nodes)
  (if solution-nodes
    (let* ((bs (irl::bindings (first solution-nodes)))
           (bs-previous (irl::bindings (first (reverse (irl::all-parents (first solution-nodes))))))
           (primitive-under-evaluation (irl::primitive-under-evaluation (first solution-nodes)))
           (measures-list 
            (list (calculate-questions-solved-by-mental-simulation-in-node primitive-under-evaluation bs bs-previous)))
           length-list)
      (loop for p in (reverse (irl::all-parents (first solution-nodes)))
            for p-previous = (reverse (irl::all-parents p))
            for primitive-under-evaluation = (irl::primitive-under-evaluation p)
            do (if p-previous
                 (push (calculate-questions-solved-by-mental-simulation-in-node primitive-under-evaluation (irl::bindings p)
                                                                              (irl::bindings (first p-previous))) measures-list)
                 (push (calculate-questions-solved-by-mental-simulation-in-initial-node primitive-under-evaluation (irl::bindings p)) measures-list)))
    (format t "~a~%" measures-list)
    (setf length-list (loop for binding in measures-list
                            if (not (eq (first binding) :no-new-bindings))
                              collect (loop for b in binding count b)
                            else
                              collect 0))
    (values length-list measures-list))))


(defun calculate-questions-solved-by-mental-simulation-in-node (primitive-under-evaluation bindings previous-bindings)
  (let* ((bound-bindings (loop for b in bindings if (value b) collect b))
         (bound-bindings-previous (loop for b in previous-bindings if (value b) collect b))
         (new-bound-bindings  (set-difference bound-bindings bound-bindings-previous))
         (primitive-name (first primitive-under-evaluation))
         (primitive-type (primitive-type? primitive-name)))
    (if (equal primitive-type :multiple)
      (loop for binding in new-bound-bindings
            for val = (value binding)
            if (equal (id val) 'context)
              ;collect (list :perception (list (var binding) (value binding)))
              collect (list :perception  binding)
            else
             ; collect (list :discourse (list (var binding) (value binding))))
              collect (list :discourse  binding))
      (if new-bound-bindings
        (loop for binding in new-bound-bindings
            ;  collect (list primitive-type (list (var binding) (list binding))))
              collect (list primitive-type  binding))
        (cons :no-new-bindings nil)))))

(defun calculate-questions-solved-by-mental-simulation-in-initial-node (primitive-under-evaluation bindings)
  (let* ((bound-bindings
          (loop for b in bindings
                if (and
                    (value b)
                    (or (string= (symbol-name (var b)) "?SCENE") (equal (symbol-name (var b)) "?MEMORY")))
                  collect b)))
    (loop for binding in bound-bindings
          ;collect (list :initial (list (var binding) (value binding)))
          collect (list :initial  binding)
          )))
   

(defun primitive-type? (primitive)
  (cond ((member primitive *inference-primitives*)
         :inference)
        ((member primitive *discourse-primitives*)
         :discourse)
        ((member primitive *perception-primitives*)
         :perception)
        ((member primitive *discourse-and-perception-primitives*)
         :multiple)))
    
;; utils

(defun total-questions-solved-by-perception ()
  (loop for line in (slot-value (monitors::get-monitor 'questions-solved-by-mental-simulation) 'values)
        sum (loop for num in (first line)
                      sum num)))