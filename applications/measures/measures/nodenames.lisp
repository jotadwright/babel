(in-package :cooking-bot-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collect nodenames of applied cxns and primitives ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node names FCG

(define-monitor node-names-fcg
                :class 'data-recorder :average-window 1)

(define-event-handler (node-names-fcg fcg::cip-finished)
  ;;assumes there is only one solution
  (let ((node-names-fcg (find-node-names cip)))
    (record-value monitor node-names-fcg)                      
    (setf (slot-value monitor 'values)
          (cons node-names-fcg (slot-value monitor 'values)))))

(defun find-node-names (cipn)
  (if (succeeded-nodes cipn)
    (let* ((succeeded-node (first (succeeded-nodes cipn)))
           (node-name (find-node-name succeeded-node))
           names)
      (push node-name names)
      (loop for p in (all-parents succeeded-node)
            do (push (find-node-name p) names))
      names)))

(defun find-node-name (node)
  "returns node name of applied cxn"
  (if node
    (if (equal (car-status (cipn-car node)) 'fcg::initial)
      "initial"
      (let* ((name (symbol-name
                    (name
                     (car-applied-cxn
                      (cipn-car node))))))
         name))))


;; Node names IRL

(define-monitor node-names-irl
                :class 'data-recorder :average-window 1)

(define-event-handler (node-names-irl irl::evaluate-irl-program-finished)
  ;;assumes there is only one solution
  (let ((node-names (find-node-names-irl irl::solution-nodes)))
    (record-value monitor node-names-irl)                      
    (setf (slot-value monitor 'values)
          (cons node-names (slot-value monitor 'values)))))

(defun find-node-names-irl (solution-nodes)
  (if solution-nodes
    (let* ((succeeded-node (first solution-nodes))
           (node-name (find-node-name-irl succeeded-node))
           names)
      (push node-name names)
      (loop for p in (irl::all-parents succeeded-node)
            do (push (find-node-name-irl p) names))
      names)))

(defun find-node-name-irl (node)
  "returns node name of applied cxn"
  (if node
    (if (equal (status node) 'irl::initial)
      "initial"
      (symbol-name (first (irl::primitive-under-evaluation node))))))


