(in-package :fcg)

;;;;;;;;;;;;;;;;;
;;             ;;
;; Diagnostics ;;
;;             ;;
;;;;;;;;;;;;;;;;;


(defclass gold-standard-not-in-search-space (problem)
  ())

(defclass gold-standard-not-in-search-space (problem)
  ())

(defclass diagnose-gold-standard-not-in-search-space (diagnostic)
  ((trigger :initform 'routine-processing-finished)))


 

(defmethod diagnose ((diagnostic diagnose-gold-standard-not-in-search-space) (cip construction-inventory-processor)
                     &key &allow-other-keys)
  "Diagnose that the gold standard is not in the search space after comprehend-all or produce-all."
  (loop with speech-act = (get-data (construction-inventory cip) :speech-act)
        for node in (traverse-depth-first (top-node cip) :collect-fn #'identity)
        when (fully-expanded? node)
        
        
  
             ;; no solution in the tree so far
             )
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
           


           
           (let ((problem (make-instance 'gold-standard-not-in-search-space)))
             (set-data problem :utterances (get-data resulting-cfs :utterances))
             (set-data problem :meanings gold-standard-meanings)
             problem)
           

           
                 )
         )
        ('->
         )
    
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (unless (find meaning gold-standard-meanings :test #'irl:equivalent-irl-programs?)
        (let ((problem (make-instance 'non-gold-standard-meaning)))
          (set-data problem :utterances (get-data resulting-cfs :utterances))
          (set-data problem :meanings gold-standard-meanings)
          problem)))))

(irl::variablify-