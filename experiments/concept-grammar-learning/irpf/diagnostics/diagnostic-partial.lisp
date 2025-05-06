(in-package :clg)

;; ---------------------------------
;; + Diagnostic: partial utterance +
;; ---------------------------------

;; problem
(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when part of the utterance is known."))

;; diagnostic class
(defclass diagnose-partial-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; diagnose func
(defmethod diagnose ((diagnostic diagnose-partial-utterance)
                     (node cip-node) &key &allow-other-keys)
    ;; Every node that can have a possible 'partial utterance' diagnoses
    ;; is pushed onto a list (stored in the initial node). This is because
    ;; a solution may still appear later in the search tree and we want
    ;; the agent to use routine processing as much as possible. When the
    ;; search arrives back at the initial node, with all nodes expanded
    ;; and still no solution, the diagnositcs are triggered manually
    ;; over the candidate list until one of them succeeds. If all fail,
    ;; the unknown-utterance diagnostic will take over.
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
             (loop for candidate in (sort-candidates (filter-candidates (get-candidates node)))
                   for (problems fixes)
                   = (multiple-value-list
                      (notify-learning candidate :trigger 'fcg::new-node))
                   when problems
                   do (progn (loop for problem in problems
                                   do (push (type-of problem) (statuses candidate)))
                        (push 'fcg::diagnostic-triggered (statuses candidate))
                        nil)
                   when fixes return nil))))))

;; helper functions
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

(defun is-lexical-cxn-p (cxn)
  (eq 'lexical (assqv :cxn-type (fcg::attributes cxn))))

(defun filter-lexical-cxns (cxns)
  (loop for cxn in cxns
        when (is-lexical-cxn-p cxn)
          collect cxn))

(defun filter-candidates (candidates)
  "Excludes any candidate nodes if they have less lexical constructions than the candidate with the most lexicals.

   Ensures that the composer must work with the entire partial program."
  (loop with max-count = 0
        ;; first pass: find the maximum count
        for candidate in candidates
        for count = (length (filter-lexical-cxns (applied-constructions candidate)))
        do (when (> count max-count)
             (setf max-count count))
        ;; second pass: collect candidates with max count
        finally (return (loop with results = '()
                              for candidate in candidates
                              for count = (length (filter-lexical-cxns (applied-constructions candidate)))
                              when (= count max-count)
                                do (push candidate results)
                              finally (return (nreverse results))))))


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

