(in-package :grammar-learning)

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
             (or (null (queue (cip node)))
                 (notany #'null (mapcar #'fully-expanded? (append (list node) (queue (cip node))))))
             (eql (direction (cip node)) '<-))
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (unless (find meaning gold-standard-meanings :test #'irl:equivalent-irl-programs?)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterances (get-data resulting-cfs :utterances))
          (set-data problem :meanings gold-standard-meanings)
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
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (utterance (render node (get-configuration (construction-inventory node) :render-mode)))
           (gold-standard-utterances (get-data resulting-cfs :utterances)))
        (unless (find (format nil "~{~a~^ ~}" utterance) gold-standard-utterances :test #'equalp)
          (let ((problem (make-instance 'non-gold-standard-utterance)))
            (set-data problem :meanings (get-data resulting-cfs :meanings))
            (set-data problem :utterances gold-standard-utterances)
            problem)))))



