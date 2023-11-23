(in-package :irl)

(defun nr-connected-components (chunk)
  (multiple-value-bind (connectedp num-components components)
      (irl-program-connected? (irl-program chunk))
    num-components))

(defun nr-repeated-variable (chunk)
  (loop with count = 0
        for predicate in (irl-program chunk)
        unless (length= predicate (remove-duplicates predicate))
        do (incf count (- (length predicate) (length (remove-duplicates predicate))))
        finally (return count)))

(defun nr-open-variables (chunk)
  (length (get-open-vars (irl-program chunk))))

(defun ontological-vector (type vectors)
  (gethash type vectors))

;; cannot check the ontological distance of the bindings
;; as the bindings are not available here!
;; alternative is to check ontological distance as
;; :chunk-evaluation-score-mode
(defun ontological-distance-new-link (chunk composer)
  (if (find-data chunk :new-links)
    (let* ((new-links (find-data chunk :new-links))
           (var-a (caar new-links))
           (type-a (cdar new-links))
           (var-b (cadr new-links))
           (type-b (cddr new-links))
           (ontological-vectors
            (find-data composer :ontological-vectors)))
      (cosine-similarity
       (ontological-vector type-a ontological-vectors)
       (ontological-vector type-b ontological-vectors)))
    0.0))

(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :best-linked-chunk)))
  ;; nodes have a lower cost when
  ;; 1) fewer connected components
  ;; 2) fewer open variables
  ;; 3) less ontological distance
  (+ (nr-connected-components (chunk node))
     ;(nr-repeated-variable (chunk node))  ; filtered by chunk-node-test
     (nr-open-variables (chunk node))
     (- 1 (ontological-distance-new-link (chunk node) composer))))

(defmethod chunk-node-test ((node chunk-composer-node)
                            (composer chunk-composer)
                            (mode (eql :valid-irl-program)))
  (let* ((bind-statements (find-all 'bind (irl-program (chunk node)) :key #'first))
         (primitives (set-difference (irl-program (chunk node)) bind-statements :test #'equal)))
    (and
     ;; primitives cannot have the same argument twice
     (loop for primitive in primitives
           always (length= (rest primitive)
                           (remove-duplicates (rest primitive))))
     ;; the same variable cannot be bound multiple times
     (length= (mapcar #'third bind-statements)
              (remove-duplicates (mapcar #'third bind-statements))))))

(defmethod chunk-evaluation-goal-test ((result chunk-evaluation-result)
                                       (composer chunk-composer)
                                       (mode (eql :duplicate-solutions)))
  ;; a solution is a duplicate when
  ;; 1) the irl programs are equivalent
  ;; 2) the bindings are all equivalent
  (loop for solution in (solutions composer)
        never (loop for solution-binding in (bindings solution)
                        for result-binding in (bindings result)
                        always (and (eql (var solution-binding)
                                         (var result-binding))
                                    (muhai-cookingbot::similar-entities (value solution-binding)
                                                                        (value result-binding))))))