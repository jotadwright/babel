;(ql:quickload :muhai-cookingbot)

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

    
(in-package :muhai-cookingbot)

(activate-monitor trace-irl)

(defparameter *meaning-to-link*
  `((fetch-and-proportion ?bowl-with-butter
                          ?ks-out ?ks-in
                          ?empty-bowl
                          ?butter-concept
                          ?quantity ?unit)
    (bind ingredient ?b1 ,(make-instance 'butter :is-concept t))
    (bind quantity ?b2 ,(make-instance 'quantity :value 230))
    (bind unit ?b3 ,(make-instance 'g))
    (bind kitchen-state ?b4 ,*full-kitchen*)))


(defparameter *composer*
  (let ((ontological-vectors (make-ontology-vectors))
        (composer
         (make-chunk-composer
          :topic nil :meaning nil
          :initial-chunk (create-chunk-from-irl-program *meaning-to-link*)
          :chunks (mapcar #'create-chunk-from-primitive (primitives-list *irl-primitives*))
          :configurations '((:chunk-expansion-modes :link-open-variables)
                            (:node-cost-mode . :best-linked-chunk)
                            (:chunk-evaluation-score-mode . :uniform)
                            (:chunk-node-tests :restrict-search-depth :check-duplicate :valid-irl-program)
                            (:chunk-evaluation-goal-tests :duplicate-solutions))
          :ontology nil
          :primitive-inventory *irl-primitives*)))
    (set-data composer :ontological-vectors ontological-vectors)
    composer))


;(get-next-solutions *composer*)
;(get-all-solutions *composer*)






#|
(defun sort-queue (queue new-nodes)
  (sort (append queue new-nodes)
        #'(lambda (chunk-1 chunk-2)
            (let ((components-1 (nr-connected-components chunk-1))
                  (components-2 (nr-connected-components chunk-2))
                  (repeated-vars-1 (nr-repeated-variable chunk-1))
                  (repeated-vars-2 (nr-repeated-variable chunk-2))
                  (open-vars-1 (nr-open-variables chunk-1))
                  (open-vars-2 (nr-open-variables chunk-2)))
              (if (= components-1 components-2)
                (if (= repeated-vars-1 repeated-vars-2)
                  (<= open-vars-1 open-vars-2)
                  (<= repeated-vars-1 repeated-vars-2))
                (<= components-1 components-2))))))

(defun worse-child-p (parent child)
  (let ((components-parent (nr-connected-components parent))
        (components-child (nr-connected-components child))
        (repeated-vars-parent (nr-repeated-variable parent))
        (repeated-vars-child (nr-repeated-variable child)))
    (or (> components-child components-parent)
        (and (<= components-child components-parent)
             (> repeated-vars-child repeated-vars-parent)))))

(defun link-open-variables-recursively (chunk)
  (loop with queue = (list chunk)
        with solutions = nil
        while queue
        for node = (pop queue)
        for children = (irl::link-open-variables node)
        for good-children = (loop for child in children
                                  unless (worse-child-p node child)
                                  collect child)
        if (null good-children)
        do (unless (find (irl-program node) solutions
                         :test #'equivalent-irl-programs?
                         :key #'irl-program)
             (push node solutions))
        else
        do (setf queue (sort-queue queue good-children))
        finally (return solutions)))

(defparameter *linked-open-variables-rec*
  (link-open-variables-recursively
    (create-chunk-from-irl-program *meaning-to-link*)))

(add-element (make-html (first *linked-open-variables-rec*)))
(add-element (make-html (last-elt *linked-open-variables-rec*)))

(loop for chunk in *linked-open-variables-rec*
      do (format t "~%chunk ~a: ~a components, ~a repeated variables, ~a open variables"
                 (id chunk) (nr-connected-components chunk)
                 (nr-repeated-variable chunk)
                 (length (get-open-vars (irl-program chunk))))
      do (add-element (make-html chunk :expand-initially t)))
|#
