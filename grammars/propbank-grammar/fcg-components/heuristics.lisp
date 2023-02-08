(in-package :propbank-grammar)


(defmethod apply-heuristic ((node cip-node) (mode (eql :frequency)))
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (/ (attr-val applied-cxn :score) 100)))


(defmethod apply-heuristic ((node cip-node) (mode (eql :edge-weight)))
  (if (field? (blackboard (construction-inventory node)) :matched-categorial-links)
    (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
      (cond ((attr-val applied-cxn :gram-category)
             (let* ((matched-neighbours
                     (loop for links-and-score in (get-data  (blackboard (construction-inventory node)) :matched-categorial-links)
                           when (eq (caar links-and-score) (attr-val applied-cxn :gram-category))
                             collect (cdar links-and-score)))
                    (lex-cat-used 
                     (loop for matched-neighbour-cat in matched-neighbours
                           when (neighbouring-categories-p (attr-val applied-cxn :gram-category)
                                                      matched-neighbour-cat
                                                      (categorial-network (construction-inventory node))
                                                      :link-type 'lex-gram)
                             do (return matched-neighbour-cat))))
               (cdr (find (cons (attr-val applied-cxn :gram-category) lex-cat-used)
                          (get-data (blackboard (construction-inventory node)) :matched-categorial-links)
                          :key #'car :test #'equalp))))
            ((attr-val applied-cxn :sense-category)
             (let* ((matched-neighbours
                     (loop for links-and-score in (get-data  (blackboard (construction-inventory node)) :matched-categorial-links)
                           when (eq (caar links-and-score) (attr-val applied-cxn :sense-category))
                             collect (cdar links-and-score)))
                    (gram-cat-used 
                     (loop for matched-neighbour-cat in matched-neighbours
                           when (neighbouring-categories-p (attr-val applied-cxn :sense-category)
                                                      matched-neighbour-cat
                                                      (categorial-network (construction-inventory node))
                                                      :link-type 'gram-sense)
                             do (return matched-neighbour-cat))))
               (cdr (find (cons (attr-val applied-cxn :sense-category) gram-cat-used)
                          (get-data (blackboard (construction-inventory node)) :matched-categorial-links)
                          :key #'car :test #'equalp))))
            (t
             0)))
    0))


(defmethod apply-heuristic ((node cip-node) (mode (eql :nr-of-units-matched)))
  "Returns a normalisation of the number of units matched by the cxn."
  (let ((applied-cxn (get-original-cxn (car-applied-cxn (cipn-car node)))))
    (* (length (conditional-part applied-cxn)) 2)))
  
       
(defun find-highest-edge-weight (category-list cxn node)
  (loop with cxn-category = (or (attr-val cxn :gram-category)
                                (attr-val cxn :sense-category))
        for cat in category-list
        if (link-exists-p cat cxn-category (original-cxn-set (construction-inventory node)))
        maximize (link-weight cat cxn-category (original-cxn-set (construction-inventory node)))))

