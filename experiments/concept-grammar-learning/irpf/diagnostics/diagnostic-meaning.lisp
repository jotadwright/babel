(in-package :cgl)

;; -------------------------------
;; + Diagnostic: partial meaning +
;; -------------------------------

;; problem
(defclass partial-meaning-problem (problem)
  () (:documentation "Problem created when part of the meaning is known."))

;; diagnostic class
(defclass diagnose-partial-meaning (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; diagnose func
(defmethod diagnose ((diagnostic diagnose-partial-meaning)
                     (node cip-node) &key &allow-other-keys)
  (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
         (repair-mode (get-configuration agent :th-link-repair-mode-formulation)))
    (case repair-mode
      (:path-required ;; only diagnose once, at the end of processing
       (diagnose-partial-meaning-at-end diagnostic node))
      (:no-path-required ;; diagnose at every leaf node
       (diagnose-partial-meaning-at-leafs diagnostic node)))))

;; helper functions
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

(defun first-merge-failed-leafs-p (node)
  (loop for leaf in (cip-leafs (cip node))
        thereis (find 'fcg::first-merge-failed (fcg::statuses leaf))))


;; uncommented by Jens
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