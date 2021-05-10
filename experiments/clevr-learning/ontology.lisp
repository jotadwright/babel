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

(defun add-composer-chunk (agent irl-program)
  ;; add a chunk to the ontology's composer-chunks
  ;; unless when an identical chunk is already there
  ;; only check chunks with more than 1 predicate
  (let* ((program-without-context
          (remove 'get-context irl-program :key #'first))
         (chunk-id (make-id
                    (format nil "~{~a~^+~}"
                            (reverse
                             (mapcar #'first
                                     program-without-context)))))
         (new-chunk
          (create-chunk-from-irl-program program-without-context
                                         :id chunk-id
                                         :target-var (get-target-var irl-program)
                                         :primitive-inventory (available-primitives agent)))
         (add-chunk-p
          (and (> (length (irl-program new-chunk)) 1)
               (loop for other-chunk in (composer-chunks agent)
                     always (or (= (length (irl-program other-chunk)) 1)
                                (not (equivalent-irl-programs? (irl-program new-chunk)
                                                               (irl-program other-chunk))))))))
    (when add-chunk-p
      (push new-chunk (composer-chunks agent)))))

#|
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

(defun add-composer-chunks (agent irl-program)
  (let* ((program-without-context
          (remove 'get-context irl-program :key #'first))
         (all-predicates
          (remove 'bind program-without-context :key #'first))
         (all-possible-chunk-predicates
          (remove-if
           #'(lambda (len) (= len 1))
           (remove-duplicates
            (loop with len = (length all-predicates)
                  for i below len
                  append (list (subseq all-predicates i len)
                               (reverse (subseq (reverse program) i len))))
            :test #'equal)
           :key #'length))
         (all-possible-chunk-programs
          (loop for predicates in all-possible-chunk-predicates
                collect (loop for predicate in predicates
                              for linked-bind = (linked-bind-statement predicate irl-program)
                              if linked-bind
                              append (list predicate
                                           (substitute (make-var 'binding)
                                                       (last-elt linked-bind)
                                                       linked-bind))
                              else collect predicate))))
    (loop for chunk-program in all-possible-chunk-programs
          unless (find chunk-program (composer-chunks agent)
                       :key #'irl-program
                       :test #'equivalent-irl-programs?)
          do (let* ((chunk-id
                     (make-id
                      (list-of-strings->string
                       (loop for predicate in chunk-program
                             for linked-bind = (linked-bind-statement predicate chunk-program)
                             if linked-bind
                             collect (format nil "~a-~a" (first predicate)
                                             (second linked-bind))
                             else collect (mkstr (first predicate)))
                       :separator "+")))
                    (chunk
                     (create-chunk-from-irl-program
                      chunk-program  :id chunk-id
                      :target-var (get-target-var chunk-program)
                      :primitive-inventory (available-primitives agent))))
               (push chunk (composer-chunks agent))))))
|#         