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
                                    if (find (intern (symbol-name (car cxn-and-prob)))
                                              (constructions-list cxn-inventory)
                                              :test #'eq :key (compose #'intern #'symbol-name #'name))
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
                              if (find (intern (mkstr (car cxn-and-prob)) :clevr-grammar-v1)
                                       hash-compatible-cxns
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



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashed+Seq2seq-beam ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-seq2seq-beam ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try.")))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed+seq2seq-beam)))
  "Creating the construction supplier and querying the seq2seq model, removing incompatibel cxns."
  (let* ((distribution (seq2seq-distribution-for-node node))
         (hash-compatible-cxns (all-cxns-except-incompatible-hashed-cxns node))
         (comptatible-cxns (loop for cxn-and-prob in distribution
                                 for cxn = (find (intern (mkstr (car cxn-and-prob)) :clevr-grammar-v1)
                                                 hash-compatible-cxns
                                                 :test #'equal :key #'name)
                                 if cxn
                                 collect (cons cxn (cdr cxn-and-prob))))
         (cxn-inventory (construction-inventory node))
         (beam-width (get-configuration cxn-inventory :beam-width))
         ;(beam-cxns (mapcar #'car (the-x-highest comptatible-cxns beam-width :key #'cdr))))
         (beam (the-x-highest comptatible-cxns beam-width :key #'cdr))
         (beam-cxns (mapcar #'car beam))
         (beam-probs (mapcar #'cdr beam)))
    ;(print beam-probs)
    (set-data node :seq2seq-prediction distribution)
    (make-instance
     'cxn-supplier-with-seq2seq-heuristics
     :remaining-constructions beam-cxns)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-seq2seq-beam)
                     (node cip-node))
  (let ((next-constructions (remaining-constructions cxn-supplier)))
    (setf (remaining-constructions cxn-supplier) nil)
    next-constructions))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ordered-by-label-hashed+seq2seq ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-supplier-with-ordered-labels-hashed+seq2seq
          (cxn-supplier-with-ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order, supports
    hashing (for lex and morph cxns) and consults the
    seq2seq model for heuristics (for cxn cxns)"))


(defun seq2seq-predicted-cxns-with-label-and-sorted (node label)
  "Get the predicted cxns from the seq2seq model and filter
   out those that have the correct label. Also, store the
   seq2seq prediction in the blackboard."
  (let* ((distribution (seq2seq-distribution-for-node node))
         (cxns-with-label (all-constructions-of-label node label))
         (compatible-cxns-with-probabilities
          (loop for (cxn-name . prob) in distribution
                for found-cxn = (find (intern (mkstr cxn-name) :clevr-grammar-v1)
                                      cxns-with-label :test #'equal :key #'name)
                if found-cxn
                collect (cons found-cxn prob)))
         (sorted-compatible-cxns
          (mapcar #'car (sort compatible-cxns-with-probabilities #'> :key #'cdr))))
    (set-data node :seq2seq-prediction distribution)
    sorted-compatible-cxns))
    

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-hashed+seq2seq)))
  (let ((parent (parent node)))
    (if parent
      ;; there is a parent node, keep using the same label
      ;; and recompute(!) the compatible cxns, either through
      ;; the hash or through the seq2seq model
      (make-instance 
       'cxn-supplier-with-ordered-labels-hashed+seq2seq
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label
       (if (eq (intern (mkstr (current-label (cxn-supplier parent))) :fcg) 'cxn)
         (seq2seq-predicted-cxns-with-label-and-sorted node (current-label (cxn-supplier parent)))
         (all-constructions-of-label-hashed node (current-label (cxn-supplier parent)))))
      ;; there is no parent, start from first label and compute
      ;; the compatible cxns through the hash or the seq2seq model
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        
        (make-instance 
         'cxn-supplier-with-ordered-labels-hashed+seq2seq
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (if (eq (intern (mkstr (car labels)) :fcg) 'cxn)
           (seq2seq-predicted-cxns-with-label-and-sorted node (car labels))
           (all-constructions-of-label-hashed node (car labels))))))))


(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels-hashed+seq2seq)
                     (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (if (eq (intern (mkstr (current-label cxn-supplier)) :fcg) 'cxn)
                 (seq2seq-predicted-cxns-with-label-and-sorted node (current-label cxn-supplier))
                 (all-constructions-of-label-hashed node (current-label cxn-supplier))))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))


;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun seq2seq-distribution-for-node (node)
  "For a given node, computes a distribution over all constructions for heuristically expanding it.
   When in formulation, need to transform the input (irl-network) to RPN notation.
   For the moment, this is a configuration. A general IRL->RPN package could be made."
  (let* ((cxn-inventory (construction-inventory node))
         (utterance/meaning (if (eq (direction (cip node)) '<-)
                              (get-data (blackboard cxn-inventory) :input)
                              ;; if rpn is available, give it already in the blackboard
                              (if (find-data (blackboard cxn-inventory) :rpn-input)
                                (get-data (blackboard cxn-inventory) :rpn-input)
                                (funcall (get-configuration cxn-inventory :seq2seq-rpn-fn)
                                         (get-data (blackboard cxn-inventory) :input)))))
         (endpoint (get-configuration cxn-inventory :seq2seq-endpoint))
         (model (if (eq (direction (cip node)) '<-)
                  (get-configuration cxn-inventory :seq2seq-model-comprehension)
                  (get-configuration cxn-inventory :seq2seq-model-formulation)))
         (number-cutoff (get-configuration cxn-inventory :seq2seq-number-cutoff))
         (probability-cutoff (get-configuration cxn-inventory :seq2seq-probability-cutoff))
         (distribution (seq2seq-next-cxn utterance/meaning (reverse (applied-constructions node)) model endpoint
                                         :number-cutoff number-cutoff :probability-cutoff probability-cutoff)))
    distribution))

