(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;; Construction suppliers for integrating seq2seq heuristics ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass cxn-supplier-with-seq2seq-heuristics ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try.")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :seq2seq-heuristic)))
  "Creating the construction supplier and querying the seq2seq model."
  (let* ((cxn-inventory (construction-inventory node))
         (utterance (get-data (blackboard cxn-inventory) :input))
         (endpoint (get-configuration cxn-inventory :seq2seq-endpoint))
         (model (if (eq (direction (cip node)) '<-)
                  (get-configuration cxn-inventory :seq2seq-model-comprehension)
                  (get-configuration cxn-inventory :seq2seq-model-formulation)))
         (number-cutoff (or (get-configuration cxn-inventory :seq2seq-number-cutoff) -1))
         (probability-cutoff (or (get-configuration cxn-inventory :seq2seq-probability-cutoff) -1))
         (distribution (seq2seq-next-cxn utterance (applied-constructions node) model endpoint
                                         :number-cutoff number-cutoff :probability-cutoff probability-cutoff)))
    (set-data node :seq2seq-prediction distribution)
    (make-instance
     'cxn-supplier-with-seq2seq-heuristics
     :remaining-constructions (loop for cxn-and-prob in distribution
                                    if (find (internal-symb (car cxn-and-prob))
                                             (constructions-list cxn-inventory)
                                             :test #'equal :key #'name )
                                    collect it))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-seq2seq-heuristics)
                     (node cip-node))
  "Return all constructions at once."
  (let ((next-constructions (remaining-constructions cxn-supplier)))
    (setf (remaining-constructions cxn-supplier) nil)
    next-constructions))
