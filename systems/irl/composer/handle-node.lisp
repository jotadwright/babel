(in-package :irl)

;; #########################################
;; handle node methods
;; -----------------------------------------

(define-event chunk-composer-node-changed-status
  (node chunk-composer-node))

(defgeneric handle-node (node handler composer)
  (:documentation "Handle the node according to handler.
   The handle-node methods are responsible for setting
   the next-handler of the node. Always return two values:
   solutions and new nodes."))

;;;; Match
(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'match))
                        (composer chunk-composer))
  (if (meaning composer)
    ;; if the composer has meaning, try to match it
    ;; to the current node
    ;; else, skip to evaluation
    (let ((matched-chunks
           (match-chunk (chunk node) (meaning composer))))
      (if matched-chunks
        (progn (set-data node 'matched-chunks matched-chunks)
          (setf (next-handler node) 'evaluate)
          (push 'match-chunk-succeeded (statuses node))
          (notify chunk-composer-node-changed-status node))
        (progn (setf (next-handler node) 'expand)
          (push 'match-chunk-failed (statuses node))
          (notify chunk-composer-node-changed-status node))))
    (setf (next-handler node) 'evaluate))
  (values nil nil))


;;;; Evaluate
(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'evaluate))
                        (composer chunk-composer))
  (setf (next-handler node) 'expand)
  (let* ((chunks-for-evaluation
          (if (find-data node 'matched-chunks)
            (get-data node 'matched-chunks)
            (list (chunk node))))
         (wrapped-chunks
          (mapcar #'(lambda (chunk)
                      (run-chunk-wrapper chunk composer))
                  chunks-for-evaluation))
         (chunk-evaluation-results
          ;; evaluate the chunks
          (loop for chunk in wrapped-chunks
                append (loop for result in (evaluate-chunk chunk composer)
                             do (setf (node result) node)
                             collect result))))
    ;; filter out good solutions and push them to
    ;; the results of the node
    (loop for result in chunk-evaluation-results
          if (and (or (null (topic composer))
                      (equal-entity (topic composer)
                                    (target-entity result)))
                  (run-chunk-evaluation-goal-tests result composer))
          do (push result (chunk-evaluation-results node))
          else do (push-data node 'bad-evaluation-results result))
    ;; change the status accordingly
    (cond
     ((chunk-evaluation-results node)
      (setf (next-handler node) nil)
      (push 'solution (statuses node))
      (notify chunk-composer-node-changed-status node))
     ((find-data node 'bad-evaluation-results)
      (push 'bad-evaluation-results (statuses node))
      (notify chunk-composer-node-changed-status node))
     (t (push 'no-evaluation-results (statuses node))
        (notify chunk-composer-node-changed-status node)))
    ;; return the solutions
    (values (chunk-evaluation-results node) nil)))
          
          

;;;; Expand
(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'expand))
                        (composer chunk-composer))
  (setf (next-handler node) nil)
  (let ((children
         (loop for (new-chunk . source-chunks) in (run-expand-chunk (chunk node) composer)
               collect (make-instance 'chunk-composer-node :composer composer
                                      :source-chunks (append source-chunks (source-chunks node))
                                      :chunk new-chunk :depth (1+ (depth node))))))
    (notify chunk-composer-node-changed-status node)
    (loop for child in children
          ;; assign a score to the chunk of the child
          do (setf (score (chunk child))
                   (run-chunk-score child composer))
          ;; assign a cost to the new node (uses chunk score)
          do (setf (cost child)
                   (run-node-cost child composer))
          ;; run the node tests
          if (not (run-chunk-node-tests child composer))
          ;; if not ok, set next handler to nil
          do (setf (next-handler child) nil)
          ;; otherwise, add to hash for duplicate detection
          ;; and assign a node number
          else do (progn (add-to-hash child composer)
                    (setf (created-at child)
                          (incf (node-counter composer)))))
    (let ((valid-children
           (remove-if #'null children :key #'next-handler)))
      (when valid-children
        (push 'expanded (statuses node)))
      (values nil valid-children))))
          
    