(in-package :fcg)

;;;;;;;;;;;;;;;;;
;;             ;;
;; Diagnostics ;;
;;             ;;
;;;;;;;;;;;;;;;;;


(defmethod diagnose ((diagnostic diagnose-cip-against-gold-standard) (cip construction-inventory-processor)
                     &key &allow-other-keys)
  "Examine cip and check whether the best-solution node or any other nodes match the gold standard."
  (let* ((cxn-inventory (construction-inventory cip))
         (gold-standard-speech-act (get-data (blackboard cxn-inventory) :speech-act))
         (best-solution-node (get-data cip :best-solution-node)))
    (if (and best-solution-node
             (gold-standard-solution-p (fcg-get-transient-structure best-solution-node)
                                       gold-standard-speech-act
                                       (direction cip)
                                       (configuration cxn-inventory)))
      nil ;; return nil of best solution matches gold standard (no problems)
      (loop for node in (traverse-depth-first (top-node cip) :collect-fn #'identity)
            if (and (fully-expanded? node)
                    (gold-standard-solution-p (fcg-get-transient-structure node)
                                              gold-standard-speech-act
                                              (direction cip)
                                              (configuration cxn-inventory)))
              do ;; return problem stating that gold standard is in search space but not best solution
                (return (make-instance 'gold-standard-elsewhere-in-search-space))
            finally
              ;; else return problem stating that gold standard is not in search space
              (return (make-instance 'gold-standard-not-in-search-space))))))