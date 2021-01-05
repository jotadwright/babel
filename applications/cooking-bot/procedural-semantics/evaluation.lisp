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
         (initial-bindings-list (evaluate-bind-predicates bind-predicates bindings-list))
         (queue (list (make-instance 'node
                                     :bindings-list initial-bindings-list
                                     :remaining-predicates (shuffle primitive-predicates)))))

    (loop while queue
          for node = (pop queue)
          if (remaining-predicates node)
          do (loop for remaining-predicate in (remaining-predicates node)
                   for new-bindings-list = (evaluate-primitive-predicate remaining-predicate (copy-object (bindings-list node)))
                   when (and new-bindings-list (succeeded-p new-bindings-list))
                   do (push (make-instance 'node
                                           :bindings-list new-bindings-list
                                           :remaining-predicates (remove remaining-predicate (remaining-predicates node))
                                           :all-parents (append (list node) (all-parents node)))
                            queue))
          else
          return (all-parents node))))

(defmethod evaluate-predicate-network ((predicate-network list) &optional bindings-list)
  (evaluate-predicate-network (do-def-predicate-network predicate-network)))

;;(evaluate-predicate-network '((bind ?milk-concept milk) (bind ?milk-amount 500) (bind ?milk-unit ml) (get-kitchen ?kitchen) (fetch-ingredient ?ingredient ?new-kitchen ?kitchen ?milk-concept ?milk-amount ?milk-unit)))

(defun evaluate-primitive-predicate (predicate &optional bindings-list)
  (let* ((bindings-list (or bindings-list (make-instance 'bindings-list)))
         (predicate-functor (first predicate))
         (predicate-args (loop for arg in (rest predicate)
                               when (lookup-binding arg bindings-list)
                               collect (binding-value (lookup-binding arg bindings-list))
                               else collect arg))
         (new-bindings (listify (apply predicate-functor predicate-args))))

    (when new-bindings
      (loop for new-binding in new-bindings
            unless (eq t new-binding)
            do
            (extend-bindings-list new-binding bindings-list)
            finally return bindings-list))))


(defun evaluate-bind-predicates (bind-predicates &optional bindings-list)
    (loop with bindings-list = (or bindings-list (make-instance 'bindings-list))
          while (succeeded-p bindings-list)
          for bp in bind-predicates
          for new-binding = (eval bp)
          do (extend-bindings-list new-binding bindings-list)
          finally return bindings-list))








(defun bind-predicate-p (predicate)
  (eql (first predicate) 'bind))




(defclass node ()
  ((bindings-list :type bindings-list :accessor bindings-list :initarg :bindings-list :initform (make-instance 'bindings-list))
   (all-parents :type list :accessor all-parents :initarg :all-parents :initform nil)
   (remaining-predicates :type list :accessor remaining-predicates :initarg :remaining-predicates :initform nil)))
