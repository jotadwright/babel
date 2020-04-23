(in-package :irl-2)

(defclass chunk-evaluation-result ()
  ((score 
    :type float :initarg :score :initform 0.0 :accessor score
    :documentation "A score that is used to rank results (higher == better)")
   (chunk 
    :type chunk :initarg :chunk :accessor chunk
    :documentation "The evaluated chunk.")
   (evaluation-node
    :type irl-program-processor-node :initarg :evaluation-node
    :accessor evaluation-node
    :documentation "The node of the evaluation search tree")
   (target-entity
    :initarg :target-entity :accessor target-entity
    :documentation "The value of the binding that
                    corresponds to the target var")
   (bind-statements 
    :type list :initarg :bind-statements :accessor bind-statements
    :documentation "IRL representations of those bindings
                     that are target or source variables of the chunk
                     and that are not given in the wrapped chunk")
   (bindings 
    :type list :initarg :bindings :accessor bindings
    :documentation "A list of bindings (one solution of the
                    irl program)")
   (node
    :type chunk-composer-node :initarg :node :accessor node
    :documentation "A pointer to the chunk-composer-node that
      created this chunk-evaluation-result"))
  (:documentation "Represents the one result of a chunk evaluation"))


(defun expand-chunks (irl-program composer 
                      &optional already-expanded-symbols)
  (let ((chunks (chunks composer)))
    (loop for c in irl-program
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
          append (list c))))


(defmethod evaluate-chunk ((chunk chunk) (composer chunk-composer))
  (let ((irl-program (expand-chunks (irl-program chunk) composer)))
    (multiple-value-bind (evaluation-results evaluation-nodes)
        (evaluate-irl-program irl-program
                              :primitive-inventory
                              (primitive-inventory composer))
      (loop with bound-variable-ids-in-irl-program 
            = (mapcar #'third (find-all 'bind (irl-program chunk) :key #'car))
            with chunk-variable-ids 
            = (mapcar #'car (cons (target-var chunk) (open-vars chunk)))
            with newly-bound-variable-ids
            = (set-difference chunk-variable-ids bound-variable-ids-in-irl-program)
            for evaluation-result in evaluation-results
            for evaluation-node in evaluation-nodes
            for bind-statements = (loop for b in evaluation-result
                                        when (and (find (var b) newly-bound-variable-ids)
                                                  (not (eq (var b) (car (target-var chunk)))))
                                        collect (list 'bind (type-of (value b))
                                                      (var b) (id (value b))))
            for target-entity = (value
                                 (find (car (target-var chunk))
                                       evaluation-result 
                                       :key #'var))
            for chunk-evaluation-result = (make-instance 'chunk-evaluation-result
                                                         :chunk chunk
                                                         :evaluation-node evaluation-node
                                                         :target-entity target-entity
                                                         :bind-statements bind-statements
                                                         :bindings evaluation-result)
            collect chunk-evaluation-result into chunk-evaluation-results
            finally
            return chunk-evaluation-results))))

;; #########################################
;; score solution
;; -----------------------------------------

(defun score-solution (result composer)
  (let ((mode (get-configuration composer :chunk-evaluation-result-scoring-mode)))
    (score-chunk-evaluation-result result composer mode)))

(defgeneric score-chunk-evaluation-result (result composer mode)
  (:documentation "A function that is called after chunk evaluation
    to compute a score for a result (higher == better)."))

(defmethod score-chunk-evaluation-result ((result chunk-evaluation-result)
                                          (composer chunk-composer)
                                          (mode (eql :default)))
  (average
   (list ;; score of the chunk
         (score (chunk result))
         ;; multiply the binding scores
         (apply #'* (mapcar #'score (bindings result)))
         ;; less duplicate primitives is better
         (let ((primitive-ids (mapcar #'car (irl-program (chunk result)))))
           (/ 1 (1+ (- (length primitive-ids)
                       (length (remove-duplicates primitive-ids)))))))))

(defmethod score-chunk-evaluation-result ((result chunk-evaluation-result)
                                          (composer chunk-composer)
                                          (mode (eql :uniform)))
  (declare (ignorable result composer))
  0.5)