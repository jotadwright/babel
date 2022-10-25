(in-package :irl)

(export '(evaluate-irl-program))

;; ############################################################################
;; evaluate-irl-program
;; ----------------------------------------------------------------------------

(define-event evaluate-irl-program-started
  (irl-program list) (ontology blackboard)
  (primitive-inventory primitive-inventory))

(define-event evaluate-irl-program-finished
  (succeeded-nodes t) (pip primitive-inventory-processor))
               
(defun evaluate-irl-program (irl-program ontology
                             &key silent n
                             (primitive-inventory *irl-primitives*))
  "This is the main entry point for evaluating irl programs.
   Provide an irl program, an ontology (blackboard or subclass),
   silent (for notificiatons), n (the number of solutions) and
   a primitive inventory."
  ;; The irl program should be a list
  (assert (listp irl-program))
  ;; If no ontology is provided, create an empty one
  (when (null ontology)
    (setf ontology (make-blackboard)))
  ;; Notify when requested
  (unless silent
    (notify evaluate-irl-program-started irl-program
            ontology primitive-inventory))
  ;; Run the search (typically exhaustively)
  (multiple-value-bind (solutions pip)
      (if n
        (primitive-apply-with-n-solutions irl-program ontology primitive-inventory n :notify (not silent))
        (primitive-apply-exhaustively irl-program ontology primitive-inventory :notify (not silent)))
    ;; only keep good solutions
    (setf solutions
          (remove 'goal-test-failed solutions
                  :key #'statuses :test #'member))
    ;; notify when requested
    (unless silent
      (notify evaluate-irl-program-finished solutions pip))
    ;; return solutions bindings, solution nodes
    ;; and primitive-inventory-processor
    (values (mapcar #'bindings solutions) solutions pip)))
