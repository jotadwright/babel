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
          (mapcar #'(lambda (chunk)
                      (run-chunk-wrapper-fn chunk composer))
                  (if (find-data node 'matched-chunks)
                    (get-data node 'matched-chunks)
                    (list (chunk node)))))
         (chunk-evaluation-results
          ;; evaluate the chunks
          (loop for chunk in chunks-for-evaluation
                append (loop for result in (evaluate-chunk chunk composer)
                             do (setf (node result) node)
                             collect result))))
    ;; filter out good solutions and push them to
    ;; the results of the node
    (loop for result in chunk-evaluation-results
          if (and (or (null (topic composer))
                      (equal-entity (topic composer)
                                    (target-entity result)))
                  (run-check-chunk-evaluation-result-fns result composer))
          do (push result (cers node))
          else do (push-data node 'bad-evaluation-results result))
    ;; change the status accordingly
    (cond
     ((cers node)
      (setf (next-handler node) nil)
      (push 'solution (statuses node))
      (notify chunk-composer-node-changed-status node))
     ((find-data node 'bad-evaluation-results)
      (push 'bad-evaluation-results (statuses node))
      (notify chunk-composer-node-changed-status node))
     (t (push 'no-evaluation-results (statuses node))
        (notify chunk-composer-node-changed-status node)))
    ;; return the solutions
    (values (cers node) nil)))
          
          

;;;; Expand
(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'expand))
                        (composer chunk-composer))
  (setf (next-handler node) nil)
  ;; as long as max depth is not reached, expand the current node
  ;; using the expand-chunk-fns
  (let ((children
         (when (> (get-configuration composer :max-search-depth)
                  (node-depth node))
           (loop for (new-chunk . source-chunks) in (run-expand-chunk-fns (chunk node) composer)
                 collect (make-instance 'chunk-composer-node :composer composer
                                       :source-chunks (append source-chunks (source-chunks node))
                                       :chunk new-chunk
                                       :node-number (incf (node-counter composer))
                                       :node-depth (1+ (node-depth node)))))))
    (when (and (null children)
               (<= (get-configuration composer :max-search-depth)
                   (node-depth node)))
      (push 'max-depth-reached (statuses node)))
    (notify chunk-composer-node-changed-status node)
    (loop for child in children
          ;; run the check node functions
          ;; these can alter the status of the child node
          if (not (run-check-node-fns child composer))
          do (setf (next-handler child) nil)
          ;; for the good nodes, give a rating
          ;; add to hash table and give a score
          ;; to the new chunk
          else
          do (progn
               (add-to-hash child composer)
               (setf (node-rating child)
                     (run-node-rating-fn child composer))
               (setf (score (chunk child))
                     (run-chunk-scoring-fn child composer))))
    (let ((valid-children
           (remove-if #'null children :key #'next-handler)))
      (when valid-children
        (push 'expanded (statuses node)))
      (values nil valid-children))))
          
    