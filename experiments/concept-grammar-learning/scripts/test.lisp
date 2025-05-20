

(in-package :clg)

(get-similar-lex-categories 'tiny-98
 (categorial-network (grammar (second (agents *experiment*)))))

(neighbouring-categories (first (neighbouring-categories 'fcg::tiny-2 (categorial-network (grammar (second (agents *experiment*))))))
                         (categorial-network (grammar (second (agents *experiment*)))))


(loop for category in (reduce #'union
                              (loop for slot in (neighbouring-categories 'fcg::TINY-2 (categorial-network (grammar (second (agents *experiment*)))))
                                    for neighbours-of-slot = (neighbouring-categories slot (categorial-network (grammar (second (agents *experiment*)))))
                                    collect neighbours-of-slot))
      for similarity = (graph-utils::my-weighted-graph-cosine-similarity
                        'fcg::tiny-2
                        category
                        (fcg::graph (categorial-network (grammar (second (agents *experiment*))))))
      collect (cons category similarity) into results
      finally (let ((sorted-list (sort results #'> :key #'cdr)))
                (loop for el in sorted-list
                      do (format t "~%~a" el))))

(reduce #'union
        (loop for slot in (neighbouring-categories 'fcg::TINY-2 (categorial-network (grammar (second (agents *experiment*)))))
              for neighbours-of-slot = (neighbouring-categories slot (categorial-network (grammar (second (agents *experiment*)))))
              collect neighbours-of-slot))

(loop for category in (list 'fcg::BIG-2 'fcg::TINY-2 'fcg::BROWN-2 'fcg::BLUE-2 'fcg::LARGE-2 'fcg::CYAN-2)
      for similarity = (graph-utils::my-weighted-graph-cosine-similarity
                        'fcg::tiny-2
                        category
                        (fcg::graph (categorial-network (grammar (second (agents *experiment*))))))
      do (format t "~%~a" (cons category similarity)))


(defun competing-categories (slot-filler)
  (loop for (sl . el) in (loop for slot in (neighbouring-categories slot-filler (categorial-network (grammar (second (agents *experiment*)))))
                               for neighbours-of-slot = (neighbouring-categories slot (categorial-network (grammar (second (agents *experiment*)))))
                               collect (cons slot neighbours-of-slot))
        do (format t "~%~a                    => [~a]" el sl))
  )


(competing-categories 'fcg::tiny-2)

(defun competing-categories2 (slot-filler
(loop for neighbour-filler in (neighbouring-categories slot-filler (categorial-network (grammar (second (agents *experiment*))))))



(graph-utils::my-weighted-graph-cosine-similarity 'fcg::tiny-2 'fcg::big-2 (fcg::graph (categorial-network (grammar (second (agents *experiment*))))))



(defun get-similar-lex-categories (lex-category categorial-network)
  (let* ((graph (fcg::graph categorial-network))
         ;(similar-lex-categories (gethash lex-category (graph-utils::node-similarities graph))))
         (similar-lex-categories (graph-utils::my-similar-nodes-weighted-cosine lex-category graph))
         )
    ;(when (not similar-lex-categories)
    ;(setf similar-lex-categories (graph-utils::my-similar-nodes-weighted-cosine-same-node-type lex-category graph))
    ;(setf (gethash lex-category (graph-utils::node-similarities graph)) similar-lex-categories))
    similar-lex-categories))



(in-package :graph-utils)

(defun my-similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop ;with node-type = (get-node-type (lookup-node graph node) graph)
       ; for other-node-id in (remove-duplicates (gethash node-type (node-types graph)))
        ;for other-node = (lookup-node graph other-node-id)
        for node-similarity = (my-weighted-graph-cosine-similarity other-node node graph)
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

