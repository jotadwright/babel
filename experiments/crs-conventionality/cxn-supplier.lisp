(in-package :fcg)


;; First try cxns that are entrenched, later the one that have an entrenchment score of 0
(defclass cxn-supplier-cascading-entrenchment (cxn-supplier-cxn-sets)
  ((current-level-cxns
    :initarg :current-level-cxns :accessor current-level-cxns
    :documentation "All the entrenched cxns")
   (remaining-level-cxns
    :type list :initarg :remaining-level-cxns :accessor remaining-level-cxns :initform nil
    :documentation "Constructions that have a score of 0"))
  (:documentation "Construction supplier that ranks cxns based on similarity from categorial links."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cascading-entrenchment)))
  "Creates an instance of the cxn-supplier and sets the cxn-sets for the applicable direction."
  (let* ((cxns (constructions-list (construction-inventory node)))
         (cxns-entrenched-not-entrenched (multiple-value-list 
                                          (loop for cxn in cxns
                                                for score = (attr-val cxn :score)
                                                if (< score 0.7)
                                                  collect cxn into not-entrenched-cxns
                                                else
                                                  collect cxn into entrenched-cxns
                                                finally (return (values entrenched-cxns not-entrenched-cxns))
                                                  ))))
    (make-instance 'cxn-supplier-cascading-entrenchment
                   :current-level-cxns (first cxns-entrenched-not-entrenched)
                   :remaining-level-cxns (list (second cxns-entrenched-not-entrenched)))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-cascading-entrenchment) (node cip-node))
  "Returns all constructions that satisfy the hash of the node or are
direct neighbours of the categories present in the node."
  (let ((cxns-of-current-level (current-level-cxns cxn-supplier)))
    (setf (current-level-cxns cxn-supplier) (pop (remaining-level-cxns cxn-supplier)))
  cxns-of-current-level))
