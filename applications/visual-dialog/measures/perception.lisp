(in-package :visual-dialog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Measures regarding Perception ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Monitor that measures and plots the variables that are solved during mental simulation.

(setf *inference-primitives* (list 'count-objects 'exist-or-count 'exist 'find-in-context 'more-than-1 'select-one 'set-diff 'unique))
(setf *discourse-primitives* (list 'get-last-attribute-category 'get-last-topic 'get-penultimate-topic))
(setf *perception-primitives* (list 'extreme-relate 'immediate-relate 'query 'relate 'segment-scene))
(setf *discourse-and-perception-primitives* (list 'filter-by-attribute))


(define-monitor questions-solved-by-mental-simulation
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-mental-simulation irl::evaluate-irl-program-finished)
  (multiple-value-bind (number-of-questions answer-vars)
      (calculate-questions-solved-by-mental-simulation irl::succeeded-nodes)
    (visualise-bindings answer-vars)
    (record-value monitor number-of-questions)
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions answer-vars) (slot-value monitor 'values)))))


(defun calculate-questions-solved-by-mental-simulation (solution-nodes)
  "Calculate questions that are solved by mental simulation at each node in the solution nodes by comparing the previous bindings with the new bindings in each node."
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
    (setf length-list (loop for binding in measures-list
                            if (not (eq (first binding) :no-new-bindings))
                              collect (loop for b in binding count b)
                            else
                              collect 0))
    (values length-list measures-list))))


(defun calculate-questions-solved-by-mental-simulation-in-node (primitive-under-evaluation bindings previous-bindings)
  "Calculate the questions that are solved in a node by comparing the previous bindings with the new bindings.
   Also find the knowledge source which was responsible for solving the variable."
  (let* ((bound-bindings (loop for b in bindings if (value b) collect b))
         (bound-bindings-previous (loop for b in previous-bindings if (value b) collect b))
         (new-bound-bindings (set-difference bound-bindings bound-bindings-previous))
         (primitive-name (first primitive-under-evaluation))
         (primitive-type (primitive-type? primitive-name)))
    (if (equal primitive-type :multiple)
      (loop for binding in new-bound-bindings
            for val = (value binding)
            if (equal (id val) 'context)
              collect (list :perception  binding)
            else
              collect (list :discourse  binding))
      (if new-bound-bindings
        (loop for binding in new-bound-bindings
              collect (list primitive-type  binding))
        (cons :no-new-bindings nil)))))

(defun calculate-questions-solved-by-mental-simulation-in-initial-node (primitive-under-evaluation bindings)
  "Calculate the bindings in the initial node."
  (let* ((bound-bindings (loop for b in bindings
                               if (value b)
                                 collect b)))
    (loop for binding in bound-bindings
          collect (list :initial binding))))
   
    
;; utils
(defun primitive-type? (primitive)
  (cond ((member primitive *inference-primitives*)
         :inference)
        ((member primitive *discourse-primitives*)
         :discourse)
        ((member primitive *perception-primitives*)
         :perception)
        ((member primitive *discourse-and-perception-primitives*)
         :multiple)))

