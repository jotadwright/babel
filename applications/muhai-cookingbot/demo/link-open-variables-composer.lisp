;(ql:quickload :muhai-cookingbot)



;;;; manually linking chunks

(in-package :muhai-cookingbot)

(defparameter *meaning-to-link*
  '((fetch-and-proportion ?bowl-with-butter
                          ?ks-out ?ks-in
                          ?empty-bowl
                          ?butter-concept
                          ?quantity ?unit)
    (bind ingredient ?b1 butter)
    (bind quantity ?b2 230)
    (bind unit ?b3 g)))

(defparameter *meaning-to-link*
  `((fetch-and-proportion ?bowl-with-butter
                          ?ks-out ?ks-in
                          ?empty-bowl
                          ?butter-concept
                          ?quantity ?unit)
    (bind kitchen-state ?ks ,(make-instance 'kitchen-state))))

(add-element (make-html (create-chunk-from-irl-program *meaning-to-link*)))

;; add other heuristics:
;; - when making a binding (A . B), consider the path length from A to B in the ontology, shorter paths are better
;; - prefer bindings to input arguments of primitives rather than output arguments
;; => or just use the composer to execute programs and see what happens!

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

#|
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



;;;;
;;;; using the composer
;;;;

(in-package :irl)

(defmethod node-cost ((node chunk-composer-node)
                      (composer chunk-composer)
                      (mode (eql :best-linked-chunk)))
  (+ (muhai-cookingbot::nr-connected-components (chunk node))
     (muhai-cookingbot::nr-repeated-variable (chunk node))
     (muhai-cookingbot::nr-open-variables (chunk node))))

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
  (make-chunk-composer
   :topic nil :meaning nil
   :initial-chunk (create-chunk-from-irl-program *meaning-to-link*)
   :chunks (mapcar #'create-chunk-from-primitive (primitives-list *irl-primitives*))
   :configurations '((:chunk-expansion-modes :link-open-variables)
                     (:node-cost-mode . :best-linked-chunk)
                     (:chunk-evaluation-score-mode . :uniform)
                     (:chunk-node-tests :restrict-search-depth :check-duplicate :valid-irl-program))
   :ontology nil
   :primitive-inventory *irl-primitives*))

(get-next-solutions *composer*)



#|
(defmethod initialize-instance :after ((composer chunk-composer) &key)
  ;; add hashed-nodes to the composer blackboard
  (set-data composer 'hashed-nodes (make-hash-table))
  ;; max depth for composer search process
  (set-configuration composer :max-search-depth 10)
  ;; How to arange the queue
  (set-configuration composer :queue-mode :best-first)
  ;; A list of functions that are called on the chunk
  ;; of each search node to create new chunks.
  (set-configuration composer :chunk-expansion-modes
                     '(:combine-program))
  ;; A list of functions that is called whenever a new
  ;; node is created to filter out bad nodes.
  (set-configuration composer :chunk-node-tests
                     '(:restrict-search-depth
                       :check-duplicate))
  ;; A function that is called whenever a new node is
  ;; created to rate the node. Returns a float.
  ;; lower == better
  (set-configuration composer :node-cost-mode
                     :short-programs-with-few-primitives-and-open-vars)
  ;; Computes a score for a chunk, a float
  ;; between 0 (very bad) and 1 (very good).
  (set-configuration composer :chunk-score-mode
                     :source-chunks-average)
  ;; A function that is called before chunk evaluation
  ;; to add things to the irl program. Gets the chunk
  ;; and returns a new chunk or nil when the wrapping
  ;; failed.
  (set-configuration composer :chunk-wrapper :identity)
  ;; A list of functions that are called after chunk
  ;; evaluation. Returns t when the result is a good  one
  (set-configuration composer :chunk-evaluation-goal-tests '(:identity))
  ;; A function that is called after chunk evaluation
  ;; to compute a score for a result (higher == better).
  (set-configuration composer :chunk-evaluation-score-mode
                     :chunk-and-binding-score-with-few-duplicates))
|#
