(in-package :cooking-bot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; This file contains everything that is neede for evaluating       ;;
;; the procedural semantics programs.                               ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; bind predicates ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind (var value)
  (:documentation "Makes a binding between var and value."))

(defmethod bind ((var var) (value standard-object))
  (make-instance 'binding :binding-var var :binding-value value))

(defmethod bind ((var var) (value number))
    (make-instance 'binding :binding-var var :binding-value value))


;; evaluate-predicate-network ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric evaluate-predicate-network (predicate-network &optional bindings-list)
  (:documentation "evaluates a predicate network and returns the resulting bindings."))

(defmethod evaluate-predicate-network ((predicate-network predicate-network) &optional bindings-list)
  (let* ((bind-predicates (remove-if-not #'bind-predicate-p (predicates predicate-network)))
         (primitive-predicates (remove-if #'bind-predicate-p (predicates predicate-network)))
         (initial-bindings-list (loop for p in bind-predicates collect (eval p))))
    initial-bindings-list
  ))

(defmethod evaluate-predicate-network ((predicate-network list) &optional bindings-list)
  (evaluate-predicate-network (do-def-predicate-network predicate-network)))





;;(evaluate-predicate-network '((bind ?milk-concept milk) (bind ?milk-amount 500) (bind ?mik-unit ml) (get-kitchen ?kitchen) (fetch-ingredient ?ingredient ?new-kitchen ?kitchen ?milk-concept ?milk-amount ?milk-unit)))














(defun bind-predicate-p (predicate)
  (eql (first predicate) 'bind))

(defun extend-bindings-list (binding bindings-list)
  
  )




