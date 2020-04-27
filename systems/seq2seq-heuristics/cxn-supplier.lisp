(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;; Construction suppliers for integrating seq2seq heuristics ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Seq2seq-heuristic ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; This construction supplier calls the seq2seq model and returns all
;; constructions at once.
;; (For hashed construction set, use :hashed+seq2seq-heuristic, which is more efficient)

(defclass cxn-supplier-with-seq2seq-heuristics ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try.")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :seq2seq-heuristic)))
  "Creating the construction supplier and querying the seq2seq model."
  (let ((cxn-inventory (construction-inventory node))
        (distribution (seq2seq-distribution-for-node node)))
    ;; Set distribution in blackboard of node
    (set-data node :seq2seq-prediction distribution)
    ;; Return all constructions for which a probability was returned.
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashed+Seq2seq-heuristic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This construction supplier calls the seq2seq model and returns all
;; constructions at once, but not the hashed ones if they are incomptatible.

(defclass cxn-supplier-hashed+seq2seq-heuristics ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try.")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed+seq2seq-heuristic)))
  "Creating the construction supplier and querying the seq2seq model, removing incompatibel cxns."
  (let* ((distribution (seq2seq-distribution-for-node node))
         (hash-compatible-cxns (all-cxns-except-incompatible-hashed-cxns node))
         (comptatible-cxns (loop for cxn-and-prob in distribution
                                 if (find (internal-symb (car cxn-and-prob)) hash-compatible-cxns
                                          :test #'equal :key #'name)
                                 collect it)))
    (set-data node :seq2seq-prediction distribution)
    (make-instance
     'cxn-supplier-with-seq2seq-heuristics
     :remaining-constructions comptatible-cxns)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed+seq2seq-heuristics)
                     (node cip-node))
  (let ((next-constructions (remaining-constructions cxn-supplier)))
    (setf (remaining-constructions cxn-supplier) nil)
    next-constructions))

;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun seq2seq-distribution-for-node (node)
  "For a given node, computes a distribution over all constructions for heuristically expanding it."
  (let* ((cxn-inventory (construction-inventory node))
         (utterance/meaning (get-data (blackboard cxn-inventory) :input))
         (endpoint (get-configuration cxn-inventory :seq2seq-endpoint))
         (model (if (eq (direction (cip node)) '<-)
                  (get-configuration cxn-inventory :seq2seq-model-comprehension)
                  (get-configuration cxn-inventory :seq2seq-model-formulation)))
         (number-cutoff (get-configuration cxn-inventory :seq2seq-number-cutoff))
         (probability-cutoff (get-configuration cxn-inventory :seq2seq-probability-cutoff))
         (distribution (seq2seq-next-cxn utterance/meaning (reverse (applied-constructions node)) model endpoint
                                         :number-cutoff number-cutoff :probability-cutoff probability-cutoff)))
    distribution))