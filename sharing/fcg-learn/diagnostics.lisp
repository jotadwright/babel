(in-package :fcg)

;;;;;;;;;;;;;;;;;
;;             ;;
;; Diagnostics ;;
;;             ;;
;;;;;;;;;;;;;;;;;


(defclass gold-standard-not-in-search-space (problem)
  ())

(defclass gold-standard-not-preferred-solution (problem)
  ())

(defclass diagnose-gold-standard-not-in-search-space (diagnostic)
  ((trigger :initform 'routine-processing-finished)))


(defun non-gold-standard-solution-p (node speech-act direction)
  "Checks whether the the transient structure holds the gold standard meaning or form, depending on the direction of processing."
  (case direction
    ('<-
     (not (equivalent-predicate-networks (extract-meanings (left-pole-structure (fcg-get-transient-structure node)))
                                         (meaning speech-act)))) ;;TO DO: Variablify!
    ('->
     (not (string= (render node (get-configuration (construction-inventory (cip node)) :render-mode))
                   (form speech-act))))))

(defmethod diagnose ((diagnostic diagnose-gold-standard-not-in-search-space) (cip construction-inventory-processor)
                     &key &allow-other-keys)
  "Diagnose that the gold standard is not in the search space after comprehend-all or produce-all."
  (loop with speech-act = (get-data (blackboard (construction-inventory cip)) :speech-act)
        for node in (traverse-depth-first (top-node cip) :collect-fn #'identity)
        when (and (fully-expanded? node)
                  (non-gold-standard-solution-p node speech-act (direction cip)))
          do (return (make-instance 'gold-standard-not-in-search-space))))


#|  
    (let ((processing-direction (processing-direction node))
          (speech-act (get-data node :speech-act))
          (resulting-cfs (car-resulting-cfs (cipn-car node))))
      (case processing-direction
        ('<-
         (unless (irl:equivalent-irl-programs? (extract-meanings (left-pole-structure resulting-cfs))
                                               (meaning speech-act))
           (make-instance 'gold-standard-not-in-search-space))
         ('->
          (unless (string= (extract-forms (left-pole-structure resulting-cfs))
                                               (meaning speech-act))
           (make-instance 'gold-standard-not-in-search-space))

          )
             
    
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (unless (find meaning gold-standard-meanings :test #'irl:equivalent-irl-programs?)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterances (get-data resulting-cfs :utterances))
          (set-data problem :meanings gold-standard-meanings)
          problem)))))
|#
