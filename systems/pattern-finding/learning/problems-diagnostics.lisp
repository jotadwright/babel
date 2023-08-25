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


