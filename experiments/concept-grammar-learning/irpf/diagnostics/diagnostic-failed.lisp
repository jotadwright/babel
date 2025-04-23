(in-package :cgl)

;; ---------------------------------
;; + Diagnostic: failed utterance +
;; ---------------------------------

;; problem
(defclass failed-interpretation-problem (problem)
  () (:documentation "Problem created when interpretation has failed."))

;; diagnostic class
(defclass diagnose-failed-interpretation (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; diagnostic func
(defmethod diagnose ((diagnostic diagnose-failed-interpretation)
                     (node cip-node) &key &allow-other-keys)
  (when (and (eql (direction (cip node)) '<-)
             (failed-interpretation-p node))
    (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
           (problem (make-instance 'failed-interpretation-problem))
           (intention (compose-program agent (topic agent) (utterance agent)
                                       (get-configuration agent :composer-strategy))))
      ;; Within the diagnostic, the agent performs intention reading.
      ;; The reconstructed intentioln is then used to check following
      ;; repairs: holophrase -> item-based (all variants) and
      ;; add-holophrase.
      (set-data problem :intention intention)
      (set-data problem :owner agent)
      problem)))

(defun interpretation-failed-in-tree (node)
  ;; there must be some node with a failed
  ;; interpretation goal test
  ;; luckily, the goal test writes this on
  ;; the blackboard of the initial node,
  ;; so we don't have to traverse the entire
  ;; tree
  (multiple-value-bind (some-interpretation-failed-p foundp)
      (find-data (initial-node node) :some-interpretation-failed)
    (when foundp some-interpretation-failed-p)))

(defun failed-interpretation-p (node)
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
       ;; there is a node in the tree with a failed
       ;; interpretation goal test
       (interpretation-failed-in-tree node)))