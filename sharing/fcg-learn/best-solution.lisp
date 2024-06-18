(in-package :fcg)

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; Best solution ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric best-solution (cip mode)
  (:documentation "Return solutions sorted according to a given criterion."))

(defmethod best-solution (cip (mode (eql :highest-average-entrenchment-score)))
  "Returns the succeeded cip-node with the highest average entrenchment score over the applied constructions."
  (loop for node in (succeeded-nodes cip)
        collect (cons node (average (mapcar #'(lambda (cxn) (attr-val cxn :entrenchment-score))
                                            (applied-constructions node))))
          into nodes-with-scores
        finally (return (car (first (sort nodes-with-scores #'> :key #'cdr))))))

(defmethod best-solution (cip (mode (eql :highest-average-link-weight)))
  "Returns the succeeded cip-node with the highest average link weight over the branch."
  (loop with category-mapping = (find-data cip :category-mapping)
        for node in (succeeded-nodes cip)
        for mapped-categorial-links = (loop for (cat-1 cat-2 link-type) in (used-categorial-links node)
                                            collect (list (or (cdr (assoc cat-1 category-mapping))
                                                              cat-1)
                                                          (or (cdr (assoc cat-2 category-mapping))
                                                              cat-2)
                                                          link-type))
        if mapped-categorial-links
          collect (cons node (/ (loop for (cat-1 cat-2 link-type) in mapped-categorial-links
                                      sum (link-weight cat-1 cat-2 (original-cxn-set (construction-inventory cip)) :link-type link-type))
                                (length mapped-categorial-links)))
            into nodes-with-scores
        else
          collect (cons node 1)
            into nodes-with-scores
        finally (return (car (first (sort nodes-with-scores #'> :key #'cdr))))))