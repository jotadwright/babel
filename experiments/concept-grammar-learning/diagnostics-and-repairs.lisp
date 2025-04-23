;;;; diagnostics-and-repairs.lisp

(in-package :cgl)

;; ------------
;; + Problems +
;; ------------

(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when the utterance is completely unknown
                      OR when no solution is found using partial utterances."))

(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when part of the utterance is known."))

(defclass failed-interpretation-problem (problem)
  () (:documentation "Problem created when interpretation has failed."))

(defclass partial-meaning-problem (problem)
  () (:documentation "Problem created when part of the meaning is known."))

;; ---------------
;; + Diagnostics +
;; ---------------

(defclass diagnose-unknown-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

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
             (problem (make-instance 'unknown-utterance-problem))
             (intention (compose-program agent (topic agent) (utterance agent)
                                         (get-configuration agent :composer-strategy))))
        (set-data problem :intention intention)
        (set-data problem :owner agent)
        problem))))






(defclass diagnose-partial-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree can be added by a repair
  ;; luckily, the handle-fix methods write this
  ;; on the blackboard of the initial node, so
  ;; we don't have to traverse the entire tree
  (multiple-value-bind (some-repair-applied-p foundp)
      (find-data (initial-node node) :some-repair-applied)
    (when foundp some-repair-applied-p)))

(defun push-candidate (node)
  ;; push this node on the candidate list,
  ;; stored in the initial node
  (push-data (initial-node node) :candidates node))

(defun get-candidates (node)
  ;; get the candidate list from the
  ;; initial node
  (find-data (initial-node node) :candidates))

(defun sort-candidates (candidates)
  ;; sort the candidates on number of applied cxns
  ;; and on the order in which they were created
  (sort candidates
        #'(lambda (node-1 node-2)
            (cond ((length= (applied-constructions node-1)
                            (applied-constructions node-2))
                   (< (created-at node-1) (created-at node-2)))
                  ((length> (applied-constructions node-1)
                            (applied-constructions node-2)) t)
                  (t nil)))))

;; Every node that can have a possible 'partial utterance' diagnoses
;; is pushed onto a list (stored in the initial node). This is because
;; a solution may still appear later in the search tree and we want
;; the agent to use routine processing as much as possible. When the
;; search arrives back at the initial node, with all nodes expanded
;; and still no solution, the diagnositcs are triggered manually
;; over the candidate list until one of them succeeds. If all fail,
;; the unknown-utterance diagnostic will take over.

(defmethod diagnose ((diagnostic diagnose-partial-utterance)
                     (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (when (and (fully-expanded? node)
               (get-strings-from-root node)
               (not (some-applied-repair-in-tree node)))
      
      (cond ((applied-constructions node)
             ;; when there are some applied constructions,
             ;; and the queue is not yet empty, then push this
             ;; node on the candidate list.
             ;; if the queue is completely empty, then you've arrived here
             ;; because the diagnostic was manually called on an item from
             ;; the candidate list (see below). Now, the actual problem can
             ;; be created!
             (if (and (null (queue (cip node)))
                      (not (interpretation-failed-in-tree node)))
               (let ((agent (find-data (blackboard (construction-inventory node)) :owner))
                     (problem (make-instance 'partial-utterance-problem)))
                 (set-data problem :owner agent)
                 problem)
               (progn (push-candidate node) nil)))

            ;; when the queue is completely empty and there are no applied
            ;; constructions, you've arrived at the initial node without
            ;; any solution. Now, manually go over the candidate list and
            ;; diagnose each of them until a solution is found. The candidate
            ;; list is sorted with most applied constructions first. As a tie
            ;; breaker, we use the 'created-at' number, so to follow the
            ;; cxn supplier.
            ((or (null (queue (cip node)))
                 (notany #'null
                         (mapcar #'fully-expanded?
                                 (cons node (queue (cip node))))))
             (loop for candidate in (sort-candidates (get-candidates node))
                   for (problems fixes)
                   = (multiple-value-list
                      (notify-learning candidate :trigger 'fcg::new-node))
                   when problems
                   do (progn (loop for problem in problems
                                   do (push (type-of problem) (statuses candidate)))
                        (push 'fcg::diagnostic-triggered (statuses candidate))
                        nil)
                   when fixes return nil))))))








(defclass diagnose-failed-interpretation (diagnostic)
  ((trigger :initform 'fcg::new-node)))

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
       
(defmethod diagnose ((diagnostic diagnose-failed-interpretation)
                     (node cip-node) &key &allow-other-keys)
  (when (and (eql (direction (cip node)) '<-)
             (failed-interpretation-p node))
    (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
           (problem (make-instance 'failed-interpretation-problem))
           (intention (compose-program agent (topic agent) (utterance agent)
                                       (get-configuration agent :composer-strategy))))
      ;; Within the diagnostic, the agent performs intention reading.
      ;; The reconstructed intention is then used to check following
      ;; repairs: holophrase -> item-based (all variants) and
      ;; add-holophrase.
      (set-data problem :intention intention)
      (set-data problem :owner agent)
      problem)))







(defclass diagnose-partial-meaning (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defmethod diagnose ((diagnostic diagnose-partial-meaning)
                     (node cip-node) &key &allow-other-keys)
  (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
         (repair-mode (get-configuration agent :th-link-repair-mode-formulation)))
    (case repair-mode
      (:path-required ;; only diagnose once, at the end of processing
       (diagnose-partial-meaning-at-end diagnostic node))
      (:no-path-required ;; diagnose at every leaf node
       (diagnose-partial-meaning-at-leafs diagnostic node)))))



;; DIAGNOSE AT END
;; ---------------

#|
(defun plausible-links-nodes-p (node)
  (let* ((all-leafs (cip-leafs (cip node)))
         (fmf-leafs (find-all 'fcg::first-merge-failed all-leafs
                              :key #'fcg::statuses :test #'member)))
    (loop for fmf-node in fmf-leafs
          for applied-cxns = (original-applied-constructions fmf-node)
          for applied-lex-cxns = (find-all 'lexical applied-cxns :key #'get-cxn-type)
          for applied-item-based-cxn = (find 'item-based applied-cxns :key #'get-cxn-type)
          thereis (and (not (null applied-lex-cxns))
                       (not (null applied-item-based-cxn))
                       (= (length applied-lex-cxns)
                          (item-based-number-of-slots applied-item-based-cxn))))))
|#

(defun first-merge-failed-leafs-p (node)
  (loop for leaf in (cip-leafs (cip node))
        thereis (find 'fcg::first-merge-failed (fcg::statuses leaf))))
    
(defmethod diagnose-partial-meaning-at-end ((diagnostic diagnose-partial-meaning)
                                            (node cip-node) &key &allow-other-keys)
  ;; you have reached the end in processing in formulation
  ;; and there is no solution
  ;; and there are some first-merge-failed nodes
  (when (and (eql (direction (cip node)) '->)
             (fully-expanded? node)
             (or (null (queue (cip node)))
                 (notany #'null (mapcar #'fully-expanded? (cons node (queue (cip node))))))
             (children node)
             (first-merge-failed-leafs-p node))
    (let ((agent (find-data (blackboard (construction-inventory node)) :owner))
          (problem (make-instance 'partial-meaning-problem)))
      (set-data problem :owner agent)
      problem)))


;; DIAGNOSE AT LEAFS
;; -----------------


(defmethod diagnose-partial-meaning-at-leafs ((diagnostic diagnose-partial-meaning)
                                              (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '->)
    (when (and (fully-expanded? node)
               (get-meaning-from-root node))
      
      (cond ((applied-constructions node)
             ;; when there are some applied constructions,
             ;; and the queue is not yet empty, then push this
             ;; node on the candidate list.
             ;; if the queue is completely empty, then you've arrived here
             ;; because the diagnostic was manually called on an item from
             ;; the candidate list (see below). Now, the actual problem can
             ;; be created!
             (if (null (queue (cip node)))
               (let ((agent (find-data (blackboard (construction-inventory node)) :owner))
                     (problem (make-instance 'partial-meaning-problem)))
                 (set-data problem :owner agent)
                 problem)
               (progn (push-candidate node) nil)))

            ;; when the queue is completely empty and there are no applied
            ;; constructions, you've arrived at the initial node without
            ;; any solution. Now, manually go over the candidate list and
            ;; diagnose each of them until a solution is found. The candidate
            ;; list is sorted with most applied constructions first. As a tie
            ;; breaker, we use the 'created-at' number, so to follow the
            ;; cxn supplier.
            ((or (null (queue (cip node)))
                 (notany #'null
                         (mapcar #'fully-expanded?
                                 (cons node (queue (cip node))))))
             (loop for candidate in (reverse (get-candidates node))
                   for (problems fixes)
                   = (multiple-value-list
                      (notify-learning candidate :trigger 'fcg::new-node))
                   when problems
                   do (progn (loop for problem in problems
                                   do (push (type-of problem) (statuses candidate)))
                        (push 'fcg::diagnostic-triggered (statuses candidate))
                        nil)
                   when fixes return nil))))))
  
