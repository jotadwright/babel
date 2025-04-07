(in-package :fcg)


(defmethod make-json ((thing cip-node) &optional objects-processed)
  "Creates a serialised lisp object for a given cip-node."
  (multiple-value-bind (json-cxns objects-processed-extended)
      (make-json (original-applied-constructions thing))
    `((:applied-cxns . ,json-cxns)
      (:statuses . ,(make-json (statuses thing))))))

(defmethod make-json ((thing fcg-construction) &optional objects-processed)
  "Create a serialised lisp object for an fcg-construction"
  `((:name . ,(name thing))
    (:class . , (type-of thing))
    (:grammar-id . ,(fcg::name (cxn-inventory thing)))
    (:attributes . ,(make-json (attributes thing) objects-processed))))