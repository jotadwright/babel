;;;; ontology.lisp

(in-package :clevr-learning)

(defparameter *challenge-level-primitive-dict*
  '((1 count! exist filter get-context query unique)
    (2 count! exist filter get-context query unique relate same)
    (3 count! exist filter get-context query unique relate same equal? intersect union! equal-integer less-than greater-than)))

(define-event challenge-level-primitives-set (level number))

(defun set-primitives-for-current-challenge-level (agent)
  (let ((primitive-inventory
         (def-irl-primitives clevr-learning-primitives
           :primitive-inventory *clevr-learning-primitives*))
        (available-primitives
         (rest (assoc (get-configuration agent :current-challenge-level)
                      *challenge-level-primitive-dict*))))
    (loop for p in available-primitives
          do (add-primitive
              (find-primitive p *clevr-primitives*)
              primitive-inventory))
    (setf (available-primitives agent) primitive-inventory)
    (notify challenge-level-primitives-set
            (get-configuration agent :current-challenge-level))))

(defun update-composer-chunks-w-primitive-inventory (agent)
  (loop for p in (irl::primitives (available-primitives agent))
        unless (find (irl::id p) (composer-chunks agent) :key #'id)
        do (push (create-chunk-from-primitive p)
                 (composer-chunks agent))))
    

(defun find-clevr-entity (answer-str ontology)
  (let ((all-categories
         (loop for field in (fields ontology)
               for field-data = (get-data ontology field)
               when (listp field-data)
               append field-data)))
    (cond
     ((stringp answer-str)
      (let ((found (find (internal-symb (upcase (mkstr answer-str)))
                         all-categories :key #'id)))
        (if found found (parse-integer answer-str))))
     ((eql t answer-str)
      (find 'yes all-categories :key #'id))
     ((null answer-str)
      (find 'no all-categories :key #'id)))))

(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

#|
(defun solution->chunk (agent solution &key (initial-score 0.5))
  "Store the irl-program AND the bind statements in a chunk"
  (make-instance 'chunk
                 :irl-program (append (bind-statements solution)
                                      (irl-program (chunk solution)))
                 :target-var (let* ((program (irl-program (chunk solution)))
                                    (target-var (get-target-var program)))
                               (cons target-var
                                     (get-type-of-var target-var program
                                                      :primitive-inventory
                                                      (available-primitives agent))))
                 :open-vars (let* ((program (irl-program (chunk solution)))
                                   (all-vars
                                    (find-all-anywhere-if
                                     #'variable-p
                                     (append (bind-statements solution)
                                             program)))
                                   (open-vars
                                    (set-difference
                                     (get-open-vars program)
                                     all-vars)))
                              (mapcar #'(lambda (var)
                                          (cons var (get-type-of-var var program
                                                                     :primitive-inventory
                                                                     (available-primitives agent))))
                                      open-vars))
                 :score initial-score))
|#

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
      (push new-chunk (composer-chunks agent))
      ;(notify composer-chunk-added composer-chunk)
      )))