(in-package :fcg)

;;;; custom cxn-supplier for clevr-grammar
;;;; -------------------------------------
;;;; This one is only _slightly_ different from the
;;;; cxn-supplier-with-hash+ordered-labels in that
;;;; the hash-compatible cxns are recomputed at
;;;; every node, even within the same label.


(defclass cxn-supplier-with-ordered-labels-hashed
          (cxn-supplier-with-ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order and supports hashing"))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-hashed)))
  (let ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      ;; !! recompute the hash-compatible cxns (with label) every time !!
      (make-instance 
       'cxn-supplier-with-ordered-labels-hashed
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-label-hashed node (current-label (cxn-supplier parent))))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-ordered-labels-hashed
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-hashed node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels-hashed)
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
               (all-constructions-of-label-hashed node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))