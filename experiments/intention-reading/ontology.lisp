;;;; ontology.lisp

(in-package :intention-reading)

(defparameter *challenge-level-primitive-dict*
  '((1 count! exist filter get-context query unique)
    (2 count! exist filter get-context query unique relate same)
    (3 count! exist filter get-context query unique relate same
       equal? intersect union! equal-integer less-than greater-than)))

(define-event challenge-level-primitives-set (level number))

(defun set-primitives-for-current-challenge-level (agent mode)
  (let (;; create a primitive inventory
        (primitive-inventory
         (def-irl-primitives clevr-learning-primitives
           :primitive-inventory *clevr-learning-primitives*))
        ;; get the primitives for the current challenge level
        (available-primitives
         (rest (assoc (get-configuration agent :current-challenge-level)
                      *challenge-level-primitive-dict*))))
    ;; add them to the new primitive inventory
    (loop with source-inventory = *clevr-primitives*
          for p in available-primitives
          do (add-primitive
              (find-primitive p source-inventory)
              primitive-inventory))
    ;; store them in the agent
    (setf (available-primitives agent) primitive-inventory)
    (notify challenge-level-primitives-set
            (get-configuration agent :current-challenge-level))))

(defun update-composer-chunks-w-primitive-inventory (agent)
  (loop for p in (irl::primitives (available-primitives agent))
        unless (find (irl::id p) (composer-chunks agent) :key #'id)
        do (push (create-chunk-from-primitive p)
                 (composer-chunks agent))))
    

(defun find-clevr-entity (answer ontology)
  (let ((all-categories
         (loop for field in (fields ontology)
               for field-data = (get-data ontology field)
               when (listp field-data)
               append field-data)))
    (cond
     ;; a number
     ((numberp answer) answer)
     ;; NIL
     ((null answer) (find 'no all-categories :key #'id))
     ;; T
     ((eql answer t) (find 'yes all-categories :key #'id))
     ;; otherwise, its an ID of an entity
     (t (find answer all-categories :key #'id)))))


(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))


(defun all-linked-predicates (predicate var irl-program)
  "Find the next predicate, given a variable"
  (when (member var predicate)
    (remove predicate (find-all var irl-program :test #'member) :test #'equal)))

(defun linked-bind-statement (predicate irl-program)
  "Get the bind-predicate linked to the given predicate"
  (let* ((var (binding-var predicate))
         (all-linked (all-linked-predicates predicate var irl-program))
         (binding-list (remove-if-not #'(lambda (pred)
                                          (eql (first pred) 'bind))
                                      all-linked)))
    (when binding-list
      (first binding-list))))

(defun binding-var (predicate)
  "Get the binding variable of a predicate"
  (unless (eql (first predicate) 'bind)
    (when (member (first predicate) '(filter query same equal? relate))
      (last-elt predicate))))


(defun predicates->chunk-id (predicates)
  (list-of-strings->string
   (mapcar #'downcase
           (mapcar #'mkstr
                   (mapcar #'first predicates)))
   :separator "+"))

(defun add-composer-chunk (agent irl-program)
  (let* ((all-predicates
          (remove 'bind irl-program :key #'first))
         (chunk-predicates
          (remove 'get-context all-predicates :key #'first))
         (chunk-exists-p
          (loop for chunk in (composer-chunks agent)
                thereis (equivalent-irl-programs? chunk-predicates (irl-program chunk)))))
    (unless chunk-exists-p
      (let ((new-chunk
             (create-chunk-from-irl-program
              chunk-predicates :id (predicates->chunk-id (reverse chunk-predicates))
              :target-var (get-target-var chunk-predicates)
              :primitive-inventory (available-primitives agent))))
        (push new-chunk (composer-chunks agent))
        new-chunk))))



(defun inc-chunk-score (chunk &key (delta 0.1)
                               (upper-bound 1.0))
  (incf (score chunk) delta)
  (when (> (score chunk) upper-bound)
    (setf (score chunk) upper-bound))
  (score chunk))

(defun dec-chunk-score (chunk &key (delta 0.1)
                               (lower-bound 0.1))
  (decf (score chunk) delta)
  (when (< (score chunk) lower-bound)
    (setf (score chunk) lower-bound))
  (score chunk))


(defun variablify-program (irl-program)
  (let* ((all-arguments
          (loop for predicate in irl-program
                if (eql (first predicate) 'bind)
                append (unless (variable-p (third predicate))
                         (list (third predicate)))
                else
                append (loop for arg in (subseq predicate 1)
                             unless (variable-p arg)
                             collect arg)))
         (unique-arguments
          (remove-duplicates all-arguments))
         (mappings
          (loop for arg in unique-arguments
                collect (cons arg (make-var arg)))))
    (loop for predicate in irl-program
          collect (loop for sym in predicate
                        if (assoc sym mappings)
                        append (list (cdr (assoc sym mappings)))
                        else append (list sym)))))