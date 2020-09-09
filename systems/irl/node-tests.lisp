(in-package :irl)

;; ############################################################################
;; node-tests
;; ----------------------------------------------------------------------------

(defgeneric run-node-tests (node primitive-inventory)
  (:documentation "Runs all node tests on the given node. A node test should return
                   t or nil. The node test should alter the state of the node if it fails,
                   otherwise, it can simply return t"))

(defmethod run-node-tests ((node irl-program-processor-node)
                           (primitive-inventory primitive-inventory))
  (loop for mode in (get-configuration primitive-inventory :node-tests)
        always (node-test node mode)))


(defgeneric node-test (node mode)
  (:documentation "Runs the node test specified by mode on the node"))

;; ---------------------------------------------------------
;; duplicate detection

(defun duplicate-node-p (node other-node)
  (unless (or (eq node other-node)
              (eq (status other-node) 'initial))
    ;; first check if the number of non-empty bindings is identical
    ;; this is a quick check that avoids having to compare all bindings
    ;; using equal-entity
    (when (= (count nil (bindings node) :key #'value)
             (count nil (bindings other-node) :key #'value))
      ;; only when the number of non-empty bindings is empty,
      ;; actually check the binding values
      (loop for var in (mapcar #'var (bindings node))
            for node-value = (value (find var (bindings node) :key #'var))
            for other-value = (value (find var (bindings other-node) :key #'var))
            always (or (and (null node-value) (null other-value))
                       (and node-value other-value
                            (equal-entity node-value other-value)))))))

(defun find-duplicate (node other-node)
  (unless (eq node other-node)
    (or (duplicate-node-p node other-node)
        (loop for child in (children other-node)
              thereis (find-duplicate node child)))))


(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :check-duplicate)))
  "Walk over the entire tree and check if there is a duplicate node.
   Nodes are duplicates when they have the same bindings."
  (let ((duplicate-p (find-duplicate node (top (processor node)))))
    (if duplicate-p
      (progn (setf (status node) 'duplicate) nil)
      t)))

(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :no-duplicate-solutions)))
  (if (primitives-remaining node) t
    (let ((duplicatep
           ;; this assumes the bindings are always ordered in the same way
           (when (solutions (processor node))
             (loop for solution in (solutions (processor node))
                   thereis (loop for value in (mapcar #'value solution)
                                 for node-value in (mapcar #'value (bindings node))
                                 always (or (and (null value) (null node-value))
                                            (and value node-value
                                                 (equal-entity value node-value))))))))
      (when duplicatep
        (setf (status node) 'duplicate))
      (not duplicatep))))
    

;; ---------------------------------------------------------
;; search depth limit

(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :restrict-search-depth)))
  (let ((max-depth (get-configuration (configuration (processor node)) :max-search-depth)))
    (when max-depth
      (if (> (node-depth node) max-depth)
        (progn (setf (status node) 'max-nr-of-nodes) nil)
        t))))

;; ---------------------------------------------------------
;; limiting the total number of nodes

(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :restrict-nr-of-nodes)))
  (let ((max-nodes (get-configuration (configuration (processor node)) :max-nr-of-nodes)))
    (when max-nodes
      (if (> (node-number node) max-nodes)
        (progn (setf (status node) 'max-nr-of-nodes) nil)
        t))))