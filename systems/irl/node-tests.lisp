(in-package :irl)

;; ############################################################################
;; node-tests
;; ----------------------------------------------------------------------------

(defgeneric pip-run-node-tests (node primitive-inventory-processor)
  (:documentation "Runs all node tests on the given node. A node test should return
                   t or nil. The node test should alter the state of the node if it fails,
                   otherwise, it can simply return t"))

(defmethod pip-run-node-tests ((node pip-node) (pip primitive-inventory-processor))
  (loop for mode in (get-configuration pip :node-tests)
        always (pip-node-test node mode)))


;; If call-next-method returns nil, then set fully-expanded? to true,
;; as we don't want to continue this path + return nil
(defmethod pip-node-test :around ((node pip-node) (mode t))
  (not (setf (fully-expanded? node) (not (call-next-method)))))

;; ---------------------------------------------------------
;; duplicate detection

(defun duplicate-node-p (node other-node)
  (and (not (eq node other-node))
       (not (duplicate other-node))
       ;; first check if the number of non-empty bindings is identical
       ;; this is a quick check that avoids having to compare all bindings
       ;; using equal-entity
       (= (count-if #'(lambda (b) (slot-boundp b 'value)) (bindings node))
          (count-if #'(lambda (b) (slot-boundp b 'value)) (bindings other-node)))
       ;; only when the number of non-empty bindings is the same,
       ;; actually check the binding values
       (loop for var in (mapcar #'var (bindings node))
             for node-binding = (find var (bindings node) :key #'var)
             for other-binding = (find var (bindings other-node) :key #'var)
             always (or (and (not (slot-boundp node-binding 'value))
                             (not (slot-boundp other-binding 'value)))
                        (and (slot-boundp node-binding 'value)
                             (slot-boundp other-binding 'value)
                             (equal-entity (value node-binding) (value other-binding)))))))

(defun find-duplicate (node other-node)
  (unless (eq node other-node)
    (or (duplicate-node-p node other-node)
        (loop for child in (children other-node)
              thereis (find-duplicate node child)))))


(defmethod pip-node-test ((node pip-node) (mode (eql :check-duplicate)))
  "Walk over the entire tree and check if there is a duplicate node.
   Nodes are duplicates when they have the same bindings."
  (let ((duplicate (find-duplicate node (top (pip node)))))
    (if duplicate
      (progn
        (setf (duplicate node) duplicate)
        (push 'duplicate (statuses node))
        nil)
      t)))

;; ---------------------------------------------------------
;; no duplicate solutions

(defmethod pip-node-test ((node pip-node) (mode (eql :no-duplicate-solutions)))
  "When this node has no more remaining primitives (leaf node),
   walk over all solutions thus far and check if this is not a
   duplicate solution."
  (if (primitives-remaining node) t
    (let ((duplicatep
           (when (succeeded-nodes (pip node))
             (loop for succeeded-node in (succeeded-nodes (pip node))
                   never (duplicate-node-p node succeeded-node)))))
      (when duplicatep
        (push 'duplicate (statuses node)))
      (not duplicatep))))
    

;; ---------------------------------------------------------
;; search depth limit

(defmethod pip-node-test ((node pip-node) (mode (eql :restrict-search-depth)))
  (let ((max-depth (get-configuration node :max-search-depth)))
    (when max-depth
      (if (> (depth node) max-depth)
        (progn (push 'max-search-depth-reached (statuses node)) nil)
        t))))

;; ---------------------------------------------------------
;; limiting the total number of nodes

(defmethod pip-node-test ((node pip-node) (mode (eql :restrict-nr-of-nodes)))
  (let ((max-nodes (get-configuration node :max-nr-of-nodes)))
    (when max-nodes
      (if (> (created-at node) max-nodes)
        (progn (push 'max-nr-of-nodes-reached (statuses node)) nil)
        t))))