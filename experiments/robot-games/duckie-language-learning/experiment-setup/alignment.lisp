(in-package :duckie-language-learning)

;; -------------
;; + Alignment +
;; -------------
         
(defmethod run-alignment ((agent duckie-language-learning-agent) cipn)
  "Aligns the construction inventory.

   Case 1: If no repair applied (or add-categorial-links), use standard lateral inhibition.
   Case 2: If a repair applied:
    2.1 A failed interpretation problem, then punish the cxns for which interpretation failed.
    2.2 For all other repairs, collect cxns for which the repair failed (if any) and punish them."
  (let* ((applied-cxns (mapcar #'get-original-cxn (applied-constructions cipn)))
         (success (find-data (blackboard (grammar agent)) :answer-correct?))
         (cip-node (find-last-node cipn))
         (all-statuses (all-cipn-statuses cip-node))
         (repair-applied-p (when cip-node (find 'fcg::added-by-repair all-statuses)))
         (failed-interpretation-p (when cip-node (find 'failed-interpretation-problem all-statuses))))
    (cond ((not repair-applied-p) (lateral-inhibition agent applied-cxns success))
          (failed-interpretation-p (let ((other-applied-cxns (collect-failed-interpretation-cxns cipn applied-cxns)))
                                     (when other-applied-cxns
                                       (punish-cxns agent other-applied-cxns))))
          (t (let ((failed-repair-cxns (collect-failed-repair-cxns cipn applied-cxns)))
               (when failed-repair-cxns
                 (punish-cxns agent failed-repair-cxns)))))))

;; ----------------------------------------
;; + Utils to find specific constructions +
;; ----------------------------------------
(defun collect-failed-repair-cxns (cipn applied-cxns)
  "Collect constructions for which a repair failed."
  (let ((all-cxns
         (remove-duplicates
          (flatten
           (traverse-depth-first
            (initial-node cipn)
            :collect-fn #'(lambda (node)
                            (when (find 'fcg::repair-failed (statuses node))
                              (original-applied-constructions node))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defun collect-failed-interpretation-cxns (cipn applied-cxns)
  "Collect constructions for which interpretation failed."
  (let ((all-cxns
         (remove-duplicates
          (flatten
           (traverse-depth-first
            (initial-node cipn)
            :collect-fn #'(lambda (node)
                            (multiple-value-bind (success foundp)
                                (find-data (goal-test-data node) :interpretation-success)
                              (when (and foundp (null success))
                                (original-applied-constructions node)))))))))
    (set-difference all-cxns applied-cxns :key #'name)))

(defun all-cipn-statuses (cipn)
  (find 'fcg::added-by-repair (flatten
                               (traverse-depth-first
                                (initial-node cipn)
                                :collect-fn #'statuses))))

(defun find-last-node (cipn)
  "There can be multiple goal-test-failed nodes.
   Take the one that was created last."
  (if (find 'fcg::succeeded (statuses cipn)) 
    cipn
    (the-biggest #'created-at
                 (find-all 'fcg::goal-test-failed
                           (all-cip-nodes (cip cipn))
                           :key #'statuses :test #'member))))
