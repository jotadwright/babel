(in-package :pattern-finding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems and Diagnostics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; ------------
;; + Problems +
;; ------------

(defclass non-gold-standard-meaning (problem)
  ())

(defclass non-gold-standard-utterance (problem)
  ())

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

(defun all-nodes-diagnosed-p (node)
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

(defun all-partial-analysis-repairs-failed-p (node)
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
       (all-nodes-diagnosed-p node)))
       
(defmethod diagnose ((diagnostic diagnose-unknown-utterance)
                     (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (when (or (unknown-utterance-p node)
              (all-partial-analysis-repairs-failed-p node))
      (let ((resulting-cfs (car-resulting-cfs (cipn-car node)))
            (problem (make-instance 'non-gold-standard-meaning)))
        (set-data problem :utterance (get-data resulting-cfs :utterance))
        (set-data problem :meaning (get-data resulting-cfs :meaning))
        problem))))






(defclass diagnose-partial-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree can be added by a repair
  ;; to find it, traverse the entire tree
  (let ((added-by-repair-node-found-p nil))
    (traverse-depth-first
     (initial-node node)
     :do-fn #'(lambda (node)
                (unless (initial-node-p node)
                  (unless (or (find 'fcg::duplicate (fcg::statuses node))
                              (find 'fcg::second-merge-failed (fcg::statuses node)))
                    (when (find 'added-by-repair (fcg::statuses node))
                      (setf added-by-repair-node-found-p t))))))
    added-by-repair-node-found-p))

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

(defun au-meaning-diff (gold-standard-meaning partial-meaning)
  ;; anti unify gold standard (pattern) with partial meaning (source)
  ;; remainder of the meaning will be in pattern delta
  (let ((au-result (first (anti-unify-predicate-network gold-standard-meaning partial-meaning))))
    (pattern-delta au-result)))


;; Every node that can have a possible 'partial utterance' diagnoses
;; is pushed onto a list (stored in the initial node). This is because
;; a solution may still appear later in the search tree and we want
;; the agent to use routine processing as much as possible. When the
;; search arrives back at the initial node, with all nodes expanded
;; and still no solution, the diagnostics are triggered manually
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
             (if (null (queue (cip node)))
               (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
                      (constructed-meaning (extract-meanings (left-pole-structure resulting-cfs)))
                      (gold-standard-meaning (get-data resulting-cfs :meaning))
                      (remaining-meaning (au-meaning-diff gold-standard-meaning constructed-meaning))
                      (remaining-utterance (get-strings-from-root node)))
                 (unless (equivalent-irl-programs? constructed-meaning gold-standard-meaning)
                   (let ((problem (make-instance 'non-gold-standard-meaning)))
                     (set-data problem :utterance remaining-utterance)
                     (set-data problem :meaning remaining-meaning)
                     problem)))
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
|#



;; Comprehension ;;
;;;;;;;;;;;;;;;;;;;

(defclass non-gold-standard-meaning (problem)
  ())

(defclass diagnose-non-gold-standard-meaning (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defmethod diagnose ((diagnostic diagnose-non-gold-standard-meaning) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the meaning or form in a fully expanded node does not match the gold standard."
  ;; Node has to be fully expanded and the direction needs to be comprehension
  (when (and (fully-expanded? node)
             (or (null (queue (cip node))) ;the queue is empty or
                 ;; everything in the queue has to be fully expanded
                 (notany #'null (mapcar #'fully-expanded? (append (list node) (queue (cip node))))))
             ;; no solution in the tree so far
             (loop for current-node in (traverse-depth-first (top-node (cip node)) :collect-fn #'identity)
                   never (find 'succeeded (statuses current-node) :test #'string=))
             (eql (direction (cip node)) '<-))
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meaning (get-data resulting-cfs :meaning)))
      (unless (equivalent-irl-programs? gold-standard-meaning meaning)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterance (get-data resulting-cfs :utterance))
          (set-data problem :meaning gold-standard-meaning)
          problem)))))


;; Production   ;;
;;;;;;;;;;;;;;;;;;

(defclass non-gold-standard-utterance (problem)
  ())

(defclass diagnose-non-gold-standard-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

(defmethod diagnose ((diagnostic diagnose-non-gold-standard-utterance) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the meaning or form in a fully expanded node does not match the gold standard."
  ;; Node has to be fully expanded and direction is formulation
  (when (and (fully-expanded? node)
             (or (null (queue (cip node)))
                 (notany #'null (mapcar #'fully-expanded? (append (list node) (queue (cip node))))))
             (eql (direction (cip node)) '->))
    (let* ((render-mode (get-configuration (construction-inventory node) :render-mode))
           (resulting-cfs (car-resulting-cfs (cipn-car node)))
           (utterance (render node render-mode))
           (gold-standard-utterance (render (get-data resulting-cfs :utterance) render-mode)))
      (unless (string= utterance gold-standard-utterance)
        (let ((problem (make-instance 'non-gold-standard-utterance)))
          (set-data problem :meaning (get-data resulting-cfs :meaning))
          (set-data problem :utterance gold-standard-utterance)
          problem)))))


