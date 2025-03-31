(in-package :fcg)


(defmethod make-json ((thing cip-node))
  "Creates a serialised lisp object for a given cip-node."
  `((:applied-cxns . ,(list (mapcar #'make-json (original-applied-constructions thing))))))

(defmethod make-json ((thing fcg-construction))
  "Create a serialised lisp object for an fcg-construction"
  `((:name . ,(name thing))
    (:class . , (type-of thing))
    (:grammar . ,(make-json (cxn-inventory thing)))
    (:contributing-pole . ,(make-json (contributing-part thing)))
    (:conditional-pole . ,(make-json (conditional-part thing)))
    (:attributes . ,(make-json (attributes thing)))))
