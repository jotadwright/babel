(in-package :clg)

(defun construction-category (cxn)
  (attr-val cxn :construction-category))

(defun find-meaning (cxn)
  (attr-val cxn :meaning))

(defun cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun find-category-per-binding (node)
  (loop for applied-cxn in (applied-constructions node)
        when (eq (cxn-type applied-cxn) 'clg::lexical)
          collect (cons (find-meaning applied-cxn) (construction-category applied-cxn))))


(defun find-competing-candidate-categories (agent category)
  ;; in beginning of experiment, there are no categories in categorial network
  (if (find category (hash-keys (graph-utils::nodes (fcg::graph (categorial-network (grammar agent))))))
    (loop with categorial-network = (categorial-network (grammar agent))
          for candidate in (reduce #'union
                                   (loop for slot in (neighbouring-categories category categorial-network)
                                         for neighbours-of-slot = (neighbouring-categories slot categorial-network)
                                         collect neighbours-of-slot))
          for similarity = (graph-utils::my-weighted-graph-cosine-similarity category candidate (fcg::graph categorial-network))
          collect (cons candidate similarity) into results
          finally (return results))
    (list (cons category 1.0))))

(defun filter-entries-above-threshold (entries threshold)
  (loop for entry in entries
        when (> (cdr entry) threshold)
          collect entry))

(in-package :graph-utils)

(defun my-similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop for node-similarity = (my-weighted-graph-cosine-similarity other-node node graph)
        when (>= node-similarity 0)
          collect (cons other-node node-similarity)
            into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))


(defun my-weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2))
         (neighbors-of-1 (remove-duplicates (graph-utils::my-neighbors graph node-id-1)))
         (neighbors-of-2 (remove-duplicates (graph-utils::my-neighbors graph node-id-2))))
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for neighbor-id in neighbors-of-1 ;; take the edge weight of all neighbouring edges
                                                   collect (expt (graph-utils:edge-weight graph neighbor-id node-id-1) 2))) ;; take the square of the weight, and sum it
                                  (reduce '+ (loop for neighbor-id in neighbors-of-2
                                                   collect (expt (graph-utils:edge-weight graph neighbor-id node-id-2) 2)))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (remove-duplicates (intersection neighbors-of-1 neighbors-of-2))
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0)))))


;; todo: edgetype fixen!
(defmethod my-neighbors ((graph undirected-typed-graph) (node integer)
                      &key edge-type (return-ids? t))
  "Return a list of cons-cells of the neighbors of the node.
   The car is the edge type and the cdr is the id of the neighbor."
  (let ((neighbors nil))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash (lambda (etype matrix)
                     (push (hash-values matrix node) neighbors))
                   (matrix graph)))
      ;; return 
      (if return-ids?
          (nreverse (flatten neighbors))
          (mapcar (lambda (pair)
                    (lookup-node graph (cdr pair)))
                  (nreverse neighbors)))))

(defun hash-values (matrix node)
  (let ((ht (gethash node (matrix matrix))))
    (when ht
      (loop for node-id being the hash-keys of ht
        collect node-id))))


;; utilities

