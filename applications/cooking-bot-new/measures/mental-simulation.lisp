(in-package :cooking-bot-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Measures regarding Mental Simulation ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-monitor questions-solved-by-mental-simulation
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-solved-by-mental-simulation irl::evaluate-irl-program-finished)
  (multiple-value-bind (number-of-questions answer-vars)
      (calculate-questions-solved-by-mental-simulation irl::solution-nodes)
    (multiple-value-bind (answers vars)
      (find-solved-slots-by-simulation (calculate-set-slots irl::solution-nodes *persistent-id-table-solved*))
      (let ((total-number-of-questions (combine number-of-questions answers)))
    (setf (slot-value monitor 'values)
          (cons (list total-number-of-questions (append answer-vars vars)) (slot-value monitor 'values)))))))


(defun combine (answers more-answers)
  (let ((more-answers (if (not more-answers) (make-list (length answers) :initial-element 0) more-answers)))
  (loop for q in answers
        for q-ont in more-answers
        collect (+ q q-ont))))

(defun calculate-questions-solved-by-mental-simulation (solution-nodes)
  (if solution-nodes
    (let* ((bs (irl::bindings (first solution-nodes)))
           (bs-previous (irl::bindings (first (irl::all-parents (first solution-nodes)))))
           anchoring-list vars-list)
      (multiple-value-bind (anchoring-last-node vars)
          (calculate-questions-solved-by-mental-simulation-in-node bs bs-previous)
      (push anchoring-last-node anchoring-list)
      (push vars vars-list)
      (loop for p in (irl::all-parents (first solution-nodes))
            for p-previous = (irl::all-parents p)
            do (if p-previous
                 (multiple-value-bind (anchoring vars) 
                  (calculate-questions-solved-by-mental-simulation-in-node (irl::bindings p) (irl::bindings (first p-previous)))
                   (push anchoring anchoring-list)
                   (push vars vars-list))
                 (progn
                   (push 0 anchoring-list)
                   (push nil vars-list))))
      (values anchoring-list vars-list)))))

(defun calculate-questions-solved-by-mental-simulation-in-node (bindings previous-bindings)
  (let* ((bound-bindings (loop for b in bindings if (value b) collect b))
         (bound-bindings-previous (loop for b in previous-bindings if (value b) collect b))
         (new-bound-bindings (set-difference bound-bindings bound-bindings-previous))
         (new-bound-bindings-length (length new-bound-bindings)))
    (values new-bound-bindings-length new-bound-bindings)))



(define-monitor questions-introduced-by-mental-simulation
                :class 'data-recorder :average-window 1)

(define-event-handler (questions-introduced-by-mental-simulation irl::evaluate-irl-program-finished)
  (multiple-value-bind (number-of-questions question-vars)
      (calculate-questions-introduced-by-mental-simulation irl::solution-nodes)
    (record-value monitor number-of-questions)
    (setf (slot-value monitor 'values)
          (cons (list number-of-questions question-vars) (slot-value monitor 'values)))))

(defun calculate-questions-introduced-by-mental-simulation (solution-nodes)
  (if solution-nodes
    (let* ((primitive (first (irl::primitive-under-evaluation (first solution-nodes))))
          ; (primitive-name (first primitive)))
      )
      (if (equal primitive 'get-kitchen)
        (values (list 0 1) (list nil '?kitchen-state))
        (values (make-list (+ (length (irl::all-parents (first solution-nodes))) 1) :initial-element 0)
                (make-list (+ (length (irl::all-parents (first solution-nodes))) 1) :initial-element nil))
        ))))


(defun calculate-questions-solved-by-mental-simulation-in-node (bindings previous-bindings)
  (let* ((bound-bindings (loop for b in bindings if (value b) collect b))
         (bound-bindings-previous (loop for b in previous-bindings if (value b) collect b))
         (new-bound-bindings (set-difference bound-bindings bound-bindings-previous))
         (new-bound-bindings-length (length new-bound-bindings)))
    (values new-bound-bindings-length new-bound-bindings)))

    