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
  
        
  

#|(defun sort-cxns-by-frequency-and-categorial-edge-weight (constructions &key node)
  "Sorts a list of constructions based on their frequency and the
weight of the edge linking the categories present in the transient
structure and those of the constructions."
  (sort constructions #'(lambda (cxn-1 cxn-2)
                          (cond ((>= (find-highest-edge-weight (lex-categories node) cxn-1 node)
                                     (find-highest-edge-weight (lex-categories node) cxn-2 node)))
                                ((>= (find-highest-edge-weight (gram-categories node) cxn-1 node)
                                     (find-highest-edge-weight (gram-categories node) cxn-2 node)))
                                ((> (attr-val cxn-1 :score) (attr-val cxn-2 :score)))
                                ((< (attr-val cxn-1 :score) (attr-val cxn-2 :score))
                                 nil)
                                (t
                                 nil)))))
|#

(defun find-highest-edge-weight (category-list cxn node)
  (loop with cxn-category = (or (attr-val cxn :gram-category)
                                (attr-val cxn :sense-category))
        for cat in category-list
        if (link-exists-p cat cxn-category (original-cxn-set (construction-inventory node)))
        maximize (link-weight cat cxn-category (original-cxn-set (construction-inventory node)))))

