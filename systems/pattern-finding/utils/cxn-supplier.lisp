(in-package :fcg)

(export '(cxn-supplier-hashed-labeled-and-positive-scores))

(defclass cxn-supplier-hashed-labeled-and-positive-scores (cxn-supplier-hashed-scored-labeled)
  ()
  (:documentation "Construction supplier that returns all constructions except
                   incompatible hashed ones, of a given label, sorted by score,
                   and with only positive scores."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-labeled-positive-scores)))
  "Create an instance of the cxn supplier"
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-hashed-labeled-and-positive-scores
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :remaining-constructions (all-constructions-of-current-label (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let* ((labels (get-configuration (construction-inventory (cip node))
                                        (if (eq (direction (cip node)) '->)
                                          :production-order :parse-order)))
             (all-constructions-of-current-label
              (all-constructions-of-label-by-hash-and-score node (car labels)))
             (all-constructions-of-current-label-with-positive-scores
              (remove-if #'(lambda (cxn) (< (attr-val cxn :score) 0))
                         all-constructions-of-current-label)))
        (make-instance 
         'cxn-supplier-hashed-labeled-and-positive-scores
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :remaining-constructions all-constructions-of-current-label-with-positive-scores
         :all-constructions-of-current-label all-constructions-of-current-label-with-positive-scores)))))

;; uses next-cxn of the superclass



;; hash mode = :hash-sequence-meaning
;; ---------------------------------------------------------
(defmethod hash ((construction construction)
                 (mode (eql :hash-sequence-meaning))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :form)
            (attr-val construction :meaning))
    (remove-duplicates
     (append (attr-val construction :form)
             (list (attr-val construction :meaning))))))

(defun extract-sequences (unit &optional (ignore-tags nil))
  (loop for form-value in (unit-feature-value unit 'form ignore-tags)
        when (and (consp form-value) (eq (first form-value) 'sequence))
        collect form-value))
          
(defmethod hash ((node cip-node)
                 (mode (eql :hash-sequence-meaning))
                 &key &allow-other-keys)
  "Checks the root and returns entities (for IRL meanings) or predicates."
  (let* ((units (fcg-get-transient-unit-structure node))
         (sequences (mapcar #'second (extract-sequences (get-root units))))
         (meanings (loop for meaning in (extract-meaning (get-root units))
                         collect (if (and (= 4 (length meaning)) (eql 'bind (first meaning)))
                                     (fourth meaning)
                                     (first meaning)))))
    (if (eql (car-direction (cipn-car node)) '<-)
      sequences
      meanings)))


;; constructions for application hashed
;; ---------------------------------------------------------
(defun constructions-for-application-hashed (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let* ((node-hashes (hash node (get-configuration node :hash-mode)))
         (constructions
          (remove-duplicates
           (append
            (loop for cxn-hash being the hash-keys of (constructions-hash-table (construction-inventory node))
                  for cxns being the hash-values of (constructions-hash-table (construction-inventory node))
                  when (loop for node-hash in node-hashes
                             thereis (or (search node-hash cxn-hash)
                                         (search cxn-hash node-hash)))
                    append cxns)
            (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions (shuffle constructions)))
    ;; sort if requested
    (when (get-configuration node :sort-cxns-before-application)
      (setq constructions
            (funcall (get-configuration node :sort-cxns-before-application)
                     constructions :node node)))
    ;; return constructions
    constructions))

(defun all-constructions-of-label-by-hash-and-score (node label)
  "returns all constructions that of label 'label'"
  (let* ((constructions (constructions-for-application-hashed node))
         (constructions-of-label
          (loop for cxn in constructions
                for cxn-label = (attr-val cxn :label)
                when (or (and (symbolp cxn-label) (equalp (symbol-name label) (symbol-name cxn-label)))
                         (and (listp cxn-label) (member label cxn-label)))
                  collect cxn)))
    ;; sort 
    (setf constructions-of-label
          (sort constructions-of-label
                #'(lambda (cxn-1 cxn-2)
                    (cond ((>= (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                          ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                           nil)
                          ((>= (attr-val cxn-1 :frequency) (attr-val cxn-2 :frequency)))))))
    ;; return constructions
    constructions-of-label))