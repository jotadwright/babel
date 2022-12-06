(in-package :irl)

;; ---------------------------------------------------------
;; single path test

;; TO DO Jens 6/12/22
;; This node test no longer works due to changes in IRL.
;; Needs to be fixed at some point...
;; Sorry, Liesbet

(defmethod node-test ((node irl-program-processor-node)
                      (mode (eql :single-path)))
  ;; is there another node that has already evaluated
  ;; this combination of primitive operations?
  ;; this can only be the case when that node has
  ;; an equal amount of more evaluated primitives.
  (let* ((all-nodes (nodes (processor node)))
         (all-evaluated-nodes
          (remove node (find-all 'evaluated all-nodes :key #'status)))
         (node-primitives
          (cons (primitive-under-evaluation node)
                (primitives-evaluated node)))
         (test-result t))
    (loop for n in all-evaluated-nodes
          for np = (cons (primitive-under-evaluation n)
                         (primitives-evaluated n))
          when (and (>= (length np) (length node-primitives))
                    (subsetp node-primitives np :test #'equal))
          do (setf test-result nil) (return))
    test-result))