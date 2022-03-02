(in-package :grammar-learning)

(define-event constructions-chosen (constructions list))
(define-event cipn-statuses (cipn-statuses list))

(defgeneric run-learner-comprehension-task (agent)
  (:documentation "Entry point for the learner's comprehension task"))

(defmethod run-learner-comprehension-task (agent)
  (set-data (blackboard (grammar agent)) :add-th-links-repair-failed nil)
    (multiple-value-bind (comprehended-meaning cipn)
      (comprehend (utterance agent) :cxn-inventory (grammar agent) :gold-standard-meaning (meaning agent))
    (let* ((applied-cxns (all-applied-cxns cipn)))
      ;; notify the logging monitor
      ;; notify which cxns will be used
      (notify constructions-chosen applied-cxns)
      (notify cipn-statuses (statuses cipn))

      ;; do alignment
      (run-alignment agent cipn (get-configuration (experiment agent) :alignment-strategy))
      ;(notify-learning process-result :trigger 'alignment-finished)
      
      ;; update the :last-used property of the cxns
      (loop for cxn in applied-cxns
            do (set-cxn-last-used agent cxn))
    (values comprehended-meaning cipn))))

(defun all-applied-cxns (cipn)
  (cond (;initial node
         (and (null (parent cipn))
              (null (children cipn)))
         (values nil nil))
        (;success node
         (find 'fcg::succeeded (fcg::statuses cipn))
         (values (mapcar #'get-original-cxn
                         (applied-constructions cipn))
                 cipn))
        (t ;otherwise, take all non-duplicate leaf nodes
         (let ((all-leaf-nodes
                (get-all-non-duplicate-leaf-nodes (cip cipn))))
           (if (length= all-leaf-nodes 1)
             ;; if there is only one, return that one
             (values (mapcar #'get-original-cxn
                             (applied-constructions (first all-leaf-nodes)))
                     (first all-leaf-nodes))
             ;; else, ...
             (flet ((avg-cxn-scores (node)
                      (average (mapcar #'cxn-score
                                       (mapcar #'get-original-cxn
                                               (applied-constructions node)))))
                    (sum-cxn-scores (node)
                      (reduce #'+ (mapcar #'cxn-score
                                       (mapcar #'get-original-cxn
                                               (applied-constructions node))))))
               (multiple-value-bind (possible-nodes impossible-nodes)
                   (separate-nodes all-leaf-nodes)
                 (let* ((set-to-consider
                         (if possible-nodes possible-nodes impossible-nodes))
                        (high-score-node
                         (the-biggest #'sum-cxn-scores set-to-consider)))
                   (values (mapcar #'get-original-cxn
                                   (applied-constructions high-score-node))
                           high-score-node)))))))))

(defun get-all-non-duplicate-leaf-nodes (cip)
  (remove-if #'(lambda (node) (find 'fcg::duplicate (fcg::statuses node)))
             (remove nil (traverse-depth-first
                          cip  :collect-fn #'(lambda (node)
                                               (when (null (children node))
                                                 node))))))
(defun separate-nodes (leaf-nodes)
  ;; remove node if item-based can never cover the current utterance
  ;; look at the number of slots, the number of applied lex cxns
  ;; and the number of strings in root
  ;; when nr-of-slots < applied lex + strings in root, skip this node
  ;; when nr-of-slots > applied lex + 1, skip this node
  (loop for node in leaf-nodes
        for applied-cxns = (mapcar #'get-original-cxn
                                   (applied-constructions node))
        for applied-lex-cxns = (find-all 'lexical applied-cxns :key #'get-cxn-type)
        for applied-item-based-cxn = (find 'item-based applied-cxns :key #'get-cxn-type)
        for strings-in-root = (get-strings-from-root node)
        when (and applied-item-based-cxn
                  (or (< (item-based-number-of-slots applied-item-based-cxn)
                         (+ (length applied-lex-cxns)
                            (length strings-in-root)))
                      (> (item-based-number-of-slots applied-item-based-cxn)
                         (1+ (length applied-lex-cxns)))))
        collect node into impossible-nodes
        else collect node into possible-nodes
        finally (return (values possible-nodes impossible-nodes))))
         
(define-event constructions-chosen (constructions list))
(define-event log-parsing-finished
  (agent clevr-learning-agent)
  (process-result-data list))

