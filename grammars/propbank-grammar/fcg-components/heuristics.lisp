(in-package :propbank-grammar)


(defmethod apply-heuristic ((node cip-node) (mode (eql :frequency)))
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (/ (attr-val applied-cxn :score) 100)))


(defmethod apply-heuristic ((node cip-node) (mode (eql :edge-weight)))
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (cond ((attr-val applied-cxn :gram-category)
           (let ((lex-category-used (cdr (assoc (attr-val applied-cxn :gram-category) (car-match-bindings (cipn-car node))))))
             (link-weight (attr-val applied-cxn :gram-category) 
                          lex-category-used (original-cxn-set (construction-inventory node)))))
          ((attr-val applied-cxn :sense-category)
           (let ((gram-category-used (cdr (assoc (attr-val applied-cxn :sense-category) (car-match-bindings (cipn-car node))))))
             (link-weight (attr-val applied-cxn :sense-category) 
                          gram-category-used (original-cxn-set (construction-inventory node)))))
          (t
           0))))

(defmethod apply-heuristic ((node cip-node) (mode (eql :nr-of-units-matched)))
  "Returns a normalisation of the number of units matched by the cxn."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (length (conditional-part applied-cxn))))
  
       
(defun find-highest-edge-weight (category-list cxn node)
  (loop with cxn-category = (or (attr-val cxn :gram-category)
                                (attr-val cxn :sense-category))
        for cat in category-list
        if (link-exists-p cat cxn-category (original-cxn-set (construction-inventory node)))
        maximize (link-weight cat cxn-category (original-cxn-set (construction-inventory node)))))

