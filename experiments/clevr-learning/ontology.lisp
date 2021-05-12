;;;; ontology.lisp

(in-package :clevr-learning)

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
    (loop with source-inventory
          = (case mode
              (:symbolic *clevr-primitives*)
              (:hybrid *hybrid-primitives*))
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


(defun get-composer-chunk (agent chunk-id)
  (find chunk-id (composer-chunks agent) :key #'id))



(defun equivalent-open-vars? (vars-1 vars-2)
  (loop with remaining-vars = vars-2
        for (var . var-type) in vars-1
        for matching-var
        = (loop for (other-var . other-var-type) in remaining-vars
                when (eql var-type other-var-type)
                return other-var)
        when matching-var
        do (setf remaining-vars
                 (remove matching-var remaining-vars :key #'car))
        finally (return (null remaining-vars))))


(defun equivalent-chunk? (chunk-1 chunk-2)
  (and (equivalent-irl-programs?
        (irl-program chunk-1) (irl-program chunk-2))
       (equivalent-open-vars?
        (irl::open-vars chunk-1) (irl::open-vars chunk-2))))


(defun add-composer-chunk (agent irl-program)
  (let* ((all-bind-statements
          (find-all 'bind irl-program :key #'first))
         (all-predicates
          (set-difference irl-program all-bind-statements
                          :test #'equal))
         (new-chunk-id
          (make-id 'composer-chunk))
         (target-var
          (get-target-var all-predicates))
         (target-var-type
          (get-type-of-var target-var all-predicates
                           :primitive-inventory (available-primitives agent)))
         (open-vars
          (get-open-vars all-predicates))
         (open-var-types
          (loop for open-var in open-vars
                for found-bind-statement
                = (find open-var all-bind-statements :key #'third)
                if found-bind-statement
                collect (second found-bind-statement)
                else
                collect (get-type-of-var
                         open-var all-predicates
                         :primitive-inventory
                         (available-primitives agent))))
         (new-chunk
          (make-instance 'chunk :id new-chunk-id
                         :irl-program all-predicates
                         :target-var (cons target-var target-var-type)
                         :open-vars (mapcar #'cons open-vars open-var-types)
                         :score (get-configuration agent :initial-chunk-score)))
         (existing-chunk
          (loop for other-chunk in (composer-chunks agent)
                when (equivalent-chunk? new-chunk other-chunk)
                return other-chunk)))
    (unless existing-chunk
      (push new-chunk (composer-chunks agent)))
    (or existing-chunk new-chunk)))



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