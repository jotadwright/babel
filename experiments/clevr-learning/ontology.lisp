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

(defun incf-chunk-score (chunk &key (delta 0.1)
                               (upper-bound 1.0))
  (incf (score chunk) delta)
  (when (> (score chunk) upper-bound)
    (setf (score chunk) upper-bound)))

(defun decf-chunk-score (chunk &key (delta 0.1)
                               (lower-bound 0.0))
  (decf (score chunk) delta)
  (when (< (score chunk) lower-bound)
    (setf (score chunk) lower-bound)))