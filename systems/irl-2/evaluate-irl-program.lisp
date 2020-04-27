(in-package :irl-2)

(export '(evaluate-irl-program))

(define-event evaluate-irl-program-started
  (irl-program list) (primitive-inventory primitive-inventory)
  (ontology blackboard))

(define-event evaluate-irl-program-finished
  (solutions list) (solution-nodes list)
  (processor irl-program-processor)
  (primitive-inventory primitive-inventory))


(defun make-child-node (parent processor next-primitive &optional result)
  "Create a child node for the parent node with the specified next-primitive"
  (make-instance 'irl-program-processor-node :status 'not-evaluated
                 :bindings (if result result (bindings parent))
                 :primitive-under-evaluation next-primitive
                 :primitives-evaluated (remove nil (cons (primitive-under-evaluation parent)
                                                         (primitives-evaluated parent)))
                 :primitives-remaining (remove next-primitive (primitives-remaining parent))
                 :processor processor
                 :node-depth (1+ (node-depth parent))
                 :node-number (incf (node-counter processor))))


(defun make-node-from-result (node result)
  "Create a new from an existing node and evaluation result"
  (make-instance 'irl-program-processor-node
                 :status 'evaluated :bindings result
                 :primitive-under-evaluation (primitive-under-evaluation node)
                 :primitives-evaluated (primitives-evaluated node)
                 :primitives-remaining (primitives-remaining node)
                 :processor (processor node)
                 :node-number (incf (node-counter (processor node)))
                 :node-depth (node-depth node)))


(defmethod order-by-priority (list-of-nodes (processor irl-program-processor)
                              (mode (eql :random)))
  (shuffle list-of-nodes))


(defmethod enqueue-ippn-nodes (list-of-nodes (processor irl-program-processor)
                               (mode (eql :depth-first)))
  (setf (queue processor)
        (append list-of-nodes (queue processor))))


(defun expand-node (node processor primitive-inventory &optional result)
  ;; create the child nodes
  (let ((child-nodes
         (loop for primitive in (primitives-remaining node)
               collect (make-child-node node processor primitive result))))
    (when child-nodes
      ;; add them to the search tree
      (loop for child in child-nodes
            do (add-node processor child :parent node))
      ;; add them to the queue, taking into account queue-mode and priority-mode...
      (let ((prioritized-child-nodes
             (order-by-priority child-nodes processor
                                (get-configuration primitive-inventory :priority-mode))))
        (enqueue-ippn-nodes prioritized-child-nodes processor
                           (get-configuration primitive-inventory :queue-mode))))))


(defun evaluate-irl-program (irl-program ontology
                             &key silent n
                             (primitive-inventory *irl-primitives*))
  ;; check if a valid option was given for n
  (unless (or (null n) (numberp n))
    (error "Invalid option for the keyword argument :n. Expected a number or nil. Got ~s" n))
  ;; check if there is an ontology to work with
  ;; if not, create an empty blackboard
  (unless (fields ontology)
    (setf ontology (make-blackboard)))
  ;; replace all non-variables with variables
  ;; and introduce bind-statements for all of them
  (let ((irl-program
         (loop for item in irl-program
               if (eq (first item) 'bind)
               collect item ;; bind statement
               else if (apply #'always (mapcar #'variable-p (cdr item)))
               collect item ;; no non-variables
               else
               append (loop with new-item = (list (car item))
                            with bind-statements
                            for parameter in (cdr item)
                            if (variable-p parameter)
                            do (push parameter new-item)
                            else
                            do (let ((var (make-var 'var))
                                     (value (if (symbolp parameter)
                                              (find-entity-by-id ontology parameter)
                                              parameter)))
                                 (push var new-item)
                                 (push
                                  `(bind ,(type-of value) ,var ,value)
                                  bind-statements))
                            finally
                            (return (cons (reverse new-item) bind-statements))))))
    ;; when set, check the irl program for mistakes before evaluating it
    (when (get-configuration primitive-inventory :check-irl-program-before-evaluation)
      (check-irl-program irl-program ontology primitive-inventory))
    (let* ((processor
            (make-irl-program-processor irl-program ontology primitive-inventory))
           (all-variables
            (remove-duplicates (all-variables irl-program)))
           (bind-statements (all-bind-statements irl-program))
           (irl-program-w/o-bind-statements 
            (set-difference irl-program bind-statements))
           (bindings-through-bind-statements
            (evaluate-bind-statements bind-statements ontology))
           (bindings-for-unbound-variables 
            (loop for var in (set-difference all-variables 
                                             (mapcar #'var bindings-through-bind-statements))
                  collect (make-instance 'binding :var var)))
           (bindings (append bindings-for-unbound-variables
                             bindings-through-bind-statements))
           (initial-node (make-instance 'irl-program-processor-node
                          :status 'initial :bindings bindings :processor processor
                          :primitive-under-evaluation nil
                          :primitives-evaluated nil
                          :primitives-remaining irl-program-w/o-bind-statements
                          :node-number 0 :node-depth 0))
           (solution-nodes nil))

      ;; notify the start of processing
      (unless silent
        (notify evaluate-irl-program-started irl-program
                primitive-inventory ontology))
      
      ;; push the initial node on the search tree and on the queue
      (add-node processor initial-node)
      (push initial-node (queue processor))

      ;; pick the first node from the queue and evaluate it
      ;; - if result = inconsistent; change the status to inconsistent and add the node to the tree
      ;; - if result = nil, change the status to evaluate-w/o-result and add the node to the tree
      ;; - if results, change the status to evaluated, expand it (once for each result, adding nodes to the qeue)
      ;;   and add node(s) to the tree (one for each result)

      (when (queue processor)
        (loop named queue-loop
         ;; pop the next node and evaluate it
         for current-node = (pop (queue processor))
         for current-primitive = (primitive-under-evaluation current-node)
         for evaluation-results = (when current-primitive
                                    (evaluate-primitive-in-program current-primitive
                                                                   (bindings current-node)
                                                                   ontology
                                                                   primitive-inventory))
         ;; print the processor
         ;;do (add-element (make-html processor))
         ;; check the evaluation-results
         do (cond ((eq evaluation-results 'inconsistent)
                   ;; if inconsistent, change the status and stop
                   (setf (status current-node) 'inconsistent))

                  ((null evaluation-results)
                   ;; if no results, change the status and stop
                   ;; (except for initial node; expand it)
                   (if (eq (status current-node) 'initial)
                     (expand-node current-node processor primitive-inventory)
                     (setf (status current-node) 'evaluated-w/o-result)))

                  ((and (listp evaluation-results) evaluation-results)
                   ;; if results, modify current node into new version(s)
                   ;; namely, one for each result
                   ;; run the node-tests and goal-tests
                   ;; and expand the current node further, if necessary
                   (let ((solution-found-p nil)) 
                     (loop for result in evaluation-results
                           for expand-node-p = t
                           for new-node = (make-node-from-result current-node result)
                           do (cond ((not (run-node-tests new-node primitive-inventory))
                                     (setf expand-node-p nil))
                                    ((run-goal-tests new-node primitive-inventory)
                                     (when (or (null n) (< (length (solutions processor)) n))
                                       (setf (status new-node) 'solution)
                                       (push (bindings new-node) (solutions processor))
                                       (push new-node solution-nodes)
                                       (setf expand-node-p nil)
                                       (setf solution-found-p t))))
                           do (add-node processor new-node :parent (parent current-node))
                           do (when expand-node-p
                                (expand-node new-node processor primitive-inventory result)))
                     (cut-node processor current-node)
                     (when (and solution-found-p (numberp n)
                                (>= (length (solutions processor)) n))
                       (return-from queue-loop)))))
         while (queue processor)))
                         
      ;; clean the solutions
      (loop for solution in (solutions processor)
            for node in solution-nodes
            when solution
            collect solution into valid-solutions
            collect node into valid-nodes
            finally
            do (setf (solutions processor) valid-solutions
                     solution-nodes valid-nodes))

      ;; notify the end of processing
      (unless silent
        (notify evaluate-irl-program-finished
                (solutions processor)
                solution-nodes
                processor
                primitive-inventory))

      ;; return solutions and solution nodes
      (values (solutions processor) solution-nodes))))
           

;; ############################################################################
;; helper functions
;; ----------------------------------------------------------------------------

(export '(irl-program-connected? irl-program-p))

(defun irl-program-connected? (irl-program)
  "Checks whether an irl program is connected. Returns t if so, the
   number of sub graphs and the sub graphs themselves"
  (loop with classes = nil
        with sub-networks = nil
        for x in irl-program
        for variables = (find-all-if #'variable-p x)
        for (cs subs) = (multiple-value-list
                         (loop for class in classes
                               for sub-network in sub-networks
                               when (loop for var in variables
                                          thereis (member var class))
                               collect class into cs
                               and
                               collect sub-network into subs
                               finally (return (values cs subs))))
        if cs
        do
        (loop for class in cs do (setf classes (remove class classes)))
        (push (remove-duplicates (reduce #'append (cons variables cs))) classes)
        (loop for sub in subs do (setf sub-networks (remove sub sub-networks)))
        (push (cons x (reduce #'append subs)) sub-networks)
        else
        do
        (push variables classes)
        (push (list x) sub-networks)
        finally
        (return (values
                 (< (length classes) 2)
                 (length classes)
                 sub-networks))))

(defun irl-program-p (thing &key (primitive-inventory *irl-primitives*))
  "returns t if thing conforms to the basic syntax of irl-programs
   list of bind statements (bind ...)  and irl-primitives (primitive ..)"
  (and (listp thing)
       (loop for s in thing
             always (and (listp s)
                         (or (eq (first s) 'bind)
                             (find-primitive (first s) primitive-inventory))))))
      