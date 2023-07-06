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
      (let* ((ignore-nil (get-configuration (construction-inventory (cip node)) :ignore-nil-hashes))
             (labels (get-configuration (construction-inventory (cip node))
                                        (if (eq (direction (cip node)) '->)
                                          :production-order :parse-order)))
             (all-constructions-of-current-label
              (all-constructions-of-label-by-hash-and-score node (car labels) :ignore-nil ignore-nil))
             (all-constructions-of-current-label-with-positive-scores
              (remove-if #'(lambda (cxn) (< (attr-val cxn :score) 0))
                         all-constructions-of-current-label)))
        (make-instance 
         'cxn-supplier-hashed-labeled-and-positive-scores
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :remaining-constructions all-constructions-of-current-label-with-positive-scores
         :all-constructions-of-current-label all-constructions-of-current-label-with-positive-scores)))))