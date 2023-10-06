(in-package :fcg)

(defun transform-feature-value-with-feature-types (v feature-name feature-types)
  (if (and (consp v) (not (merge-fn (first v))))
      (let ((so
             (case (first (rest (assoc feature-name feature-types)))
               (set '==p)
               (set-of-predicates '==p)
               (sequence nil))))
        (if so (cons so v) v))
      v))

(defun transform-structure-with-feature-types (s feature-types)
  (loop for unit in s
        collect (make-unit 
                 :name (unit-name unit)
                 :features
                 (loop for feature in (unit-features unit) collect
                         (if (tag-p feature)
                           feature
                           (make-feature (feature-name feature)
                                         (transform-feature-value-with-feature-types
                                          (feature-value feature) (feature-name feature)
                                          feature-types)))))))

(defun equivalent-coupled-feature-structures-strict-aux (cfs-1 cfs-2 &key cxn-inventory)
  (multiple-value-bind (instantiated-2-left instantiations)
      (instantiate-expression (left-pole-structure cfs-2))
    (let* ((instantiated-2-right
            (instantiate-expression 
             (right-pole-structure cfs-2) instantiations))
           (unit-name-renamings
            (when (left-pole-structure cfs-1)
              (unless (variable-p (unit-name (first (left-pole-structure cfs-1))))
                (create-vars
                 (union (mapcar #'unit-name (left-pole-structure cfs-1))
                        (mapcar #'unit-name (right-pole-structure cfs-1)))))))
           (rcfs (sublis unit-name-renamings (left-pole-structure cfs-1)))
           (bsl (progn  
                  (unify-structures
                   (transform-structure-with-feature-types
                    rcfs (feature-types (original-cxn-set cxn-inventory)))
                   instantiated-2-left
                   (list +no-bindings+)
                   :cxn-inventory cxn-inventory))))
	(when bsl
	  (unify-structures
	   (transform-structure (sublis unit-name-renamings (right-pole-structure cfs-1)))
	   instantiated-2-right
	   bsl
           :cxn-inventory cxn-inventory)))))

(defun equivalent-coupled-feature-structures-strict (cfs-1 cfs-2 cxn-inventory)
  (and cfs-1 cfs-2
       (length= (left-pole-structure cfs-1)
                (left-pole-structure cfs-2))
       (length= (right-pole-structure cfs-1)
                (right-pole-structure cfs-2))
       (equivalent-coupled-feature-structures-strict-aux
        cfs-1 cfs-2 :cxn-inventory cxn-inventory)
       (equivalent-coupled-feature-structures-strict-aux
        cfs-2 cfs-1 :cxn-inventory cxn-inventory)))

(defun strict-duplicate-nodes? (node other-node cxn-inventory)
  (and (not (eq node other-node))
       (not (duplicate other-node))
       (permutation-of? (applied-constructions node)
                        (applied-constructions other-node)
                        :key #'name)
       (equivalent-coupled-feature-structures-strict 
        (car-resulting-cfs (cipn-car node))
        (car-resulting-cfs (cipn-car other-node))
        cxn-inventory)))

(defun find-strict-duplicate (node other-node)
  (or (when (strict-duplicate-nodes? node other-node (construction-inventory node))
        other-node)
      (loop for child in (children other-node)
            for duplicate = (find-strict-duplicate node child)
            when duplicate do (return duplicate))))

(defmethod cip-node-test ((node cip-node) (mode (eql :check-duplicate-strict)))
  "Checks whether the node is a duplicate of another one in the tree"
  (let ((duplicate (find-strict-duplicate node (top-node (cip node)))))
    (if duplicate
      (progn
        (setf (duplicate node) duplicate)
        (push 'duplicate (statuses node))
        nil)
      t)))