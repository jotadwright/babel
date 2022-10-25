(in-package :irl)

(export '(chunk-evaluation-result
          bind-statements))

(defclass chunk-evaluation-result ()
  ((score 
    :type float :initarg :score :initform 0.0 :accessor score
    :documentation "A score that is used to rank results (higher == better)")
   (chunk 
    :type chunk :initarg :chunk :accessor chunk
    :documentation "The evaluated chunk.")
   (pip-node
    :type pip-node :initarg :pip-node :accessor pip-node
    :documentation "The node of the primitive inventory processor")
   (target-entity
    :initarg :target-entity :accessor target-entity
    :documentation "The value of the binding that corresponds to the target var")
   (bind-statements 
    :type list :initarg :bind-statements :accessor bind-statements
    :documentation "IRL representations of those bindings
                    that are target or source variables of the chunk
                    and that are not given in the wrapped chunk")
   (bindings 
    :type list :initarg :bindings :accessor bindings
    :documentation "A list of bindings (one solution of the irl program)")
   (node
    :type chunk-composer-node :initarg :node :accessor node
    :documentation "A pointer to the chunk-composer-node that
      created this chunk-evaluation-result"))
  (:documentation "Represents the one result of a chunk evaluation"))


(defun expand-chunks (irl-program composer 
                      &optional already-expanded-symbols)
  (loop with chunks = (chunks composer)
        for c in irl-program
        for chunk = (find (first c) chunks :key #'id)
        if (and chunk (not (find (id chunk) already-expanded-symbols)))
        append (substitute-variables 
                (expand-chunks (irl-program chunk) composer
                               (cons (id chunk) already-expanded-symbols))
                ;; call-pattern
                (append
                 (list (cons (car (target-var chunk)) (second c)))
                 (loop for open-var in (open-vars chunk)
                       for call-p-var in (cdr (cdr c))
                       collect (cons (car open-var) call-p-var))))
        else 
        append (list c)))


(defmethod evaluate-chunk ((chunk chunk) (composer chunk-composer))
  (let ((irl-program (expand-chunks (irl-program chunk) composer)))
    (multiple-value-bind (list-of-bindings pip-nodes pip)
        ;; evaluating the irl program always in silent mode
        (evaluate-irl-program irl-program (ontology composer) :silent t
                              :primitive-inventory (primitive-inventory composer))
      (declare (ignorable pip))
      (loop with bound-variable-ids-in-irl-program 
            = (mapcar #'third (all-bind-statements (irl-program chunk)))
            with chunk-variable-ids 
            = (mapcar #'car (cons (target-var chunk) (open-vars chunk)))
            with newly-bound-variable-ids
            = (set-difference chunk-variable-ids bound-variable-ids-in-irl-program)
            for bindings in list-of-bindings
            for pip-node in pip-nodes
            for bind-statements = (loop for b in bindings
                                        when (and (find (var b) newly-bound-variable-ids)
                                                  (not (eq (var b) (car (target-var chunk)))))
                                        collect (list 'bind (type-of (value b))
                                                      (var b) (id (value b))))
            for target-entity = (value
                                 (find (car (target-var chunk))
                                       bindings :key #'var))
            for chunk-evaluation-result = (make-instance 'chunk-evaluation-result
                                                         :chunk chunk
                                                         :pip-node pip-node
                                                         :target-entity target-entity
                                                         :bind-statements bind-statements
                                                         :bindings bindings)
            collect chunk-evaluation-result into chunk-evaluation-results
            finally
            (return chunk-evaluation-results)))))

;; #########################################
;; run-chunk-evaluation-result-score
;; -----------------------------------------

(defun run-chunk-evaluation-result-score (result composer)
  (let ((mode (get-configuration composer :chunk-evaluation-score-mode)))
    (score-chunk-evaluation result composer mode)))

(defgeneric score-chunk-evaluation (result composer mode)
  (:documentation "A function that is called after chunk evaluation
    to compute a score for a result (higher == better)."))

(defmethod score-chunk-evaluation ((result chunk-evaluation-result)
                                   (composer chunk-composer)
                                   (mode (eql :chunk-and-binding-score-with-few-duplicates)))
  (average
   (list ;; score of the chunk
         (score (chunk result))
         ;; multiply the binding scores
         (apply #'* (mapcar #'score (bindings result)))
         ;; less duplicate primitives is better
         (let ((primitive-ids (mapcar #'car (irl-program (chunk result)))))
           (/ 1 (1+ (- (length primitive-ids)
                       (length (remove-duplicates primitive-ids)))))))))

(defmethod score-chunk-evaluation ((result chunk-evaluation-result)
                                   (composer chunk-composer)
                                   (mode (eql :uniform)))
  (declare (ignorable result composer))
  0.5)
