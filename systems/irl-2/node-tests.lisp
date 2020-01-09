(in-package :irl-2)

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
  (unless (eq node other-node)
    (loop for var in (mapcar #'var (bindings node))
          for node-value = (value (find var (bindings node) :key #'var))
          for other-value = (value (find var (bindings other-node) :key #'var))
          always (or (and (null node-value)
                          (null other-value))
                     (and node-value
                          other-value
                          (equal-entity node-value other-value))))))

(defun find-duplicate (node other-node)
  (unless (eq node other-node)
    (or (duplicate-node-p node other-node)
        (loop for child in (children other-node)
              thereis (duplicate-node-p node child)))))

(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :check-duplicate)))
  (let ((duplicate-p (find-duplicate node (top (processor node)))))
    (if duplicate-p
      (progn (setf (status node) 'duplicate) nil)
      t)))

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
      (if (> (created-at node) max-nodes)
        (progn (setf (status node) 'max-nr-of-nodes) nil)
        t))))