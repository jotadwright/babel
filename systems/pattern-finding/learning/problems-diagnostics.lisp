(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems and Diagnostics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                   never (succeeded-cipn-p current-node))
                   ;never (find 'succeeded (statuses current-node) :test #'string=))
             (eql (direction (cip node)) '<-))
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meaning (get-data resulting-cfs :meaning)))
      (unless (equivalent-irl-programs? gold-standard-meaning meaning)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterance (get-data resulting-cfs :utterance))
          (set-data problem :meaning gold-standard-meaning)
          problem)))))

#|
(defun unknown-utterance-p (node)
  ;; the utterance is completely unknown when
  (and ;; node is fully expanded
       (fully-expanded? node)
       ;; node is the initial node
       (initial-node-p node)
       ;; node has no children
       (null (children node))
       ;; node has no applied constructions
       (null (applied-constructions node))
       ;; queue is empty or
       ;; all nodes in it are fully expanded
       (or (null (queue (cip node)))
           (notany #'null
                   (mapcar #'fully-expanded?
                           (cons node (queue (cip node))))))))

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree was added by a repair
  ;; to find it, traverse the entire tree
  (let ((added-by-repair-node-found-p nil))
    (traverse-depth-first
     (initial-node node)
     :do-fn #'(lambda (node)
                (when (find 'added-by-repair (fcg::statuses node))
                  (setf added-by-repair-node-found-p t))))
    added-by-repair-node-found-p))

(defun potential-partial-analysis-p (node)
  ;; a node is a potential partial analysis node when
  (and ;; it is fully expanded
       (fully-expanded? node)
       ;; there are some applied cxns
       (applied-constructions node)
       ;; there are no children
       (null (children node))
       ;; no repair has applied yet
       (not (some-applied-repair-in-tree node))))

(defun reached-end-of-search-p (node)
  ;; reached the end of search when
  (and ;; the node is fully expanded
       (fully-expanded? node)
       ;; it is the initial node
       (initial-node-p node)
       ;; the queue is empty or
       ;; all nodes in it are fully expanded
       (or (null (queue (cip node)))
           (notany #'null
                   (mapcar #'fully-expanded?
                           (cons node (queue (cip node))))))))

(defun no-solution-in-tree-p (node)
  (loop for current-node in (traverse-depth-first (top-node (cip node))
                                                  :collect-fn #'identity)
        never (find 'succeeded (statuses current-node) :test #'string=)))

(defun non-gold-standard-meaning-p (node)
  (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
         (meaning (extract-meanings (left-pole-structure resulting-cfs)))
         (gold-standard-meaning (get-data resulting-cfs :meaning)))
    (not (equivalent-irl-programs? gold-standard-meaning meaning))))

(defun all-nodes-diagnosed-p (node)
  ;; all leafs in the tree must be diagnosed.
  ;; For this, we truly need to traverse
  ;; the entire tree...
  (let ((undiagnosed-node-found-p nil))
    (traverse-depth-first
     (initial-node node)
     :do-fn #'(lambda (node)
                (unless (initial-node-p node)
                  (unless (find 'fcg::duplicate (fcg::statuses node))
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

(defun push-candidate (node)
  ;; push this node on the candidate list,
  ;; stored in the initial node
  (push-data (initial-node node) :candidates node))

(defun get-candidates (node)
  ;; get the candidate list from the
  ;; initial node
  (find-data (initial-node node) :candidates))
      
(defmethod diagnose ((diagnostic diagnose-non-gold-standard-meaning) (node cip-node)
                     &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (let ((resulting-cfs (car-resulting-cfs (cipn-car node))))
      (cond (; utterance is unknown, not a single cxn could apply
             (unknown-utterance-p node)
             ; make a problem
             (let ((problem (make-instance 'non-gold-standard-meaning)))
               (set-data problem :utterance (get-data resulting-cfs :utterance))
               (set-data problem :meaning (get-data resulting-cfs :meaning))
               problem))

            (; node is a potential partial analysis node
             ; but the queue is not empty
             (and (potential-partial-analysis-p node)
                  (queue (cip node)))
             ; add it to the candidate list (stored in the initial node)
             (progn (push-candidate node) nil))

            (; when reached the end of search
             (reached-end-of-search-p node)
             ; loop over candidates and try to repair
             (loop for candidate in (reverse (get-candidates node))
                   for (problems fixes)
                   = (multiple-value-list
                      (notify-learning candidate :trigger 'fcg::new-node))
                   when problems
                   do (progn (loop for problem in problems
                                   do (push (type-of problem) (statuses candidate)))
                        (push 'fcg::diagnostic-triggered (statuses candidate))
                        nil)
                   when fixes return nil))

            (; node is a potential partial analysis node
             ; and the queue is empty
             ; and no solution in the tree
             ; and node does not have gold standard meaning
             (and (potential-partial-analysis-p node)
                  (null (queue (cip node)))
                  (no-solution-in-tree-p node)
                  (non-gold-standard-meaning-p node))
             ; you've arrived here because the diagnostic was manually called
             ; on an item from the candidate list (see above). Now, the actual
             ; problem can be created!
             (let ((problem (make-instance 'non-gold-standard-meaning)))
               (set-data problem :utterance (get-data resulting-cfs :utterance))
               (set-data problem :meaning (get-data resulting-cfs :meaning))
               problem))))))
|#


;; Production ;;
;;;;;;;;;;;;;;;;

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


