(in-package :fcg)

(export '(cxn-supplier-hashed-labeled-and-positive-scores
          cxn-supplier-ordered-by-label-and-positive-score))

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

(defclass cxn-supplier-ordered-by-label-and-positive-score (cxn-supplier-ordered-by-label-and-score)
  ()
  (:documentation "A construction pool that applies constructions of
                   different labels sorted by score, but removes
                   negative scores."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-and-positive-score)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-ordered-by-label-and-score
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let* ((labels (get-configuration (construction-inventory (cip node))
                                        (if (eq (direction (cip node)) '->)
                                          :production-order :parse-order)))
             (cxns (all-constructions-of-label-by-score node (car labels)))
             (positive-cxns (remove-if #'(lambda (cxn) (< (attr-val cxn :score) 0)) cxns)))
        (make-instance 
         'cxn-supplier-ordered-by-label-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label positive-cxns)))))



(defclass cxn-supplier-cxn-sets-hashed-positive-score (cxn-supplier-cxn-sets-hashed)
  () (:documentation "cxn sets + hashing + positive scores"))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cxn-sets-hashed-positive-scores)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (make-instance 'cxn-supplier-cxn-sets-hashed-positive-score
                 :cxn-sets (get-configuration (construction-inventory (cip node))
                                              (if (eq (direction (cip node)) '->)
                                                :production-order :parse-order))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-cxn-sets-hashed-positive-score) (node cip-node))
  "Returns all constructions that satisfy the hash of the node and are in the current or a later set
   and have a positive score!"
  (if (initial-node-p node)
    ;; return cxns of the first set
    (loop with first-set = (first (cxn-sets cxn-supplier))
          for construction in (constructions-for-application-hashed node)
          when (and (equalp (symbol-name (attr-val construction :label))
                            (symbol-name first-set))
                    (> (attr-val construction :score) 0))
          collect construction)
    ;; return cxns of the same or later sets
    (loop with current-set = (attr-val (first (applied-constructions node)) :label)
          with current-set-index = (position (symbol-name current-set) (cxn-sets cxn-supplier) :test #'equalp :key #'symbol-name)
          with remaining-sets = (subseq (cxn-sets cxn-supplier) current-set-index)
          for construction in (constructions-for-application-hashed node)
          when (and (member (symbol-name (attr-val construction :label)) remaining-sets :test #'equalp :key #'symbol-name)
                    (> (attr-val construction :score) 0))
          collect construction)))