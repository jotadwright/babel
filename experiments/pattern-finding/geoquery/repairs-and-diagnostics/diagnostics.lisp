(in-package :pf-for-sql)

;;---------------;;
;; Comprehension ;;
;;---------------;;

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
                 (notany #'null (mapcar #'fully-expanded? (append (list node) (queue (cip node))))));; everything in the queue has to be fully expanded
             ;; no solution in the tree so far
             (loop for current-node in (traverse-depth-first (top-node (cip node)) :collect-fn #'identity)
                   never (find 'succeeded (statuses current-node) :test #'string=))
             (eql (direction (cip node)) '<-))
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (unless (find meaning gold-standard-meanings :test #'irl:equivalent-irl-programs?)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterances (get-data resulting-cfs :utterances))
          (set-data problem :meanings gold-standard-meanings)
          problem)))))