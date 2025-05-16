(in-package :clg)

;; ---------------------------------
;; + Diagnostic: unknown utterance +
;; ---------------------------------

;; problem
(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when the utterance is completely unknown
                      OR when no solution is found using partial utterances."))

;; diagnostic class
(defclass diagnose-unknown-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; diagnose func
(defmethod diagnose ((diagnostic diagnose-unknown-utterance)
                     (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (when (or (unknown-utterance-p node)
              (partial-repairs-failed-p node))
      ;; Within the diagnostic, the agent performs intention reading.
      ;; The reconstructed intention is then used to check following
      ;; repairs: holophrase -> item-based (all variants) and
      ;; add-holophrase.
      (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
             (problem (make-instance 'unknown-utterance-problem)))
        (set-data problem :owner agent)
        problem))))

;; helper functions
(defun unknown-utterance-p (node)
  (and ;; node is fully expanded
       (fully-expanded? node)
       ;; node is the initial node
       (initial-node-p node)
       ;; queue is empty or fully expanded
       (or (null (queue (cip node)))
           (notany #'null
                   (mapcar #'fully-expanded?
                           (cons node (queue (cip node))))))
       ;; no children
       (null (children node))
       ;; no applied constructions
       (null (applied-constructions node))))

(defun partial-repairs-failed-p (node)
  (and ;; node is fully expanded
       (fully-expanded? node)
       ;; node is the initial node
       (initial-node-p node)
       ;; node has some children
       (children node)
       ;; queue is empty or fully expanded
       (or (null (queue (cip node)))
           (notany #'null
                   (mapcar #'fully-expanded?
                           (cons node (queue (cip node))))))
       ;; all nodes in the tree have been diagnosed (and failed)
       (all-diagnostics-tried-in-tree node)))

(defun all-diagnostics-tried-in-tree (node)
  ;; all nodes in the tree (except for the initial node)
  ;; must be diagnosed. For this, we truly need to traverse
  ;; the entire tree...
  (let ((undiagnosed-node-found-p nil))
    (traverse-depth-first
     (initial-node node)
     :do-fn #'(lambda (node)
                (unless (initial-node-p node)
                  (unless (or (find 'fcg::duplicate (fcg::statuses node))
                              (find 'fcg::second-merge-failed (fcg::statuses node)))
                    (unless (find 'fcg::diagnostic-triggered (fcg::statuses node))
                      (setf undiagnosed-node-found-p t))))))
    (null undiagnosed-node-found-p)))