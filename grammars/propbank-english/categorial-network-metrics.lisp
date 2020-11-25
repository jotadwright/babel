;(ql:quickload :grammar-learning)

;(in-package :grammar-learning)

(in-package :propbank-english)

(defun euclidean-distance (node-1 node-2 graph)
  "Euclidean distance is equal to the number of neighbors that differ between two vertices"
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2))
         (degree-node-1 (length (remove-duplicates (graph-utils::neighbors graph node-id-1))))
         (degree-node-2 (length (remove-duplicates (graph-utils::neighbors graph node-id-2))))
         (neighbours-node-1 (remove-duplicates (graph-utils::neighbors graph node-id-1)))
         (neighbours-node-2 (remove-duplicates (graph-utils::neighbors graph node-id-2))))
    (float (/ (length (set-difference neighbours-node-1 neighbours-node-2)) (+ degree-node-1 degree-node-2)))))

(defun common-neighbours (node-1 node-2 graph)
  (let ((neighbours-node-1 (graph-utils::neighbors graph node-1))
        (neighbours-node-2 (graph-utils::neighbors graph node-2)))
    (remove-duplicates (intersection neighbours-node-1 neighbours-node-2))))

(defun graph-cosine-similarity (node-1 node-2 graph)
  "(GCS i j) = (/ (number-common-neighbours i j) (sqrt (* (degree i) (degree j))))"
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
        (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let* ((neighbours-node-1 (remove-duplicates (graph-utils::neighbors graph node-id-1)))
             (neighbours-node-2 (remove-duplicates (graph-utils::neighbors graph node-id-2)))
             (common-neighbours (intersection neighbours-node-1 neighbours-node-2))
             (degree-node-1 (length neighbours-node-1))
             (degree-node-2 (length neighbours-node-2))
             (denominator (sqrt (* degree-node-1 degree-node-2))))
        (if (> denominator 0)
          (/ (length common-neighbours) denominator) 0)))))

(defun weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for degree-1 in (remove-duplicates (graph-utils::neighbors graph node-id-1))
                                                   collect (square (graph-utils:edge-weight graph  degree-1 node-id-1))))
                                  (reduce '+ (loop for degree-2 in (remove-duplicates (graph-utils::neighbors graph node-id-2))
                                                   collect (square (graph-utils:edge-weight graph  degree-2 node-id-2))))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0)))))

(defun square (x)
  (* x x))

(defun similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (weighted-graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))

(defun similar-nodes-cosine (node graph)
  "loop through all nodes in graph, sort by cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))

(defun similar-nodes-euclidean (node graph)
  "loop through all nodes in graph, sort by cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (euclidean-distance other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'< :key #'cdr))))

(defun neighbour-categories (node graph)
  "loop through neighbours, sort them by edge weight"
  (loop with node-id = (graph-utils:lookup-node graph node)
        for neighbour-id in (remove-duplicates (graph-utils::neighbors graph node))
        for edge-weight = (graph-utils:edge-weight graph node-id neighbour-id)
        collect (cons (graph-utils:lookup-node graph neighbour-id) edge-weight)
        into neighbour-categories
        finally (return (sort neighbour-categories #'> :key #'cdr))))



(make-array '(5 5))


(graph-utils::saref (graph-utils::matrix (graph-utils::graph *th*)) (graph-utils::indices (graph-utils::graph *th*)))



(incf (gethash 0 (graph-utils::matrix (graph-utils::matrix (graph-utils::graph *th*)))))

(graph-utils::saref (graph-utils::matrix (graph-utils::graph *th*)) 0 2)


(saref (matrix graph) n1 n2)

(defun node-cosine-similarity (node-1 node-2 graph)
  ""
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2))
         (vector-1 )
         (vector-2))
    (cosine-similarity vector-1 vector-2)))



  
  (let ((neighbors nil))
    (map-sarray-col #'(lambda (row-id value)
                        (when (> value 0)
                          (push row-id neighbors)))
                    (matrix graph) node)
    (when (directed? graph)
      (map-sarray-row #'(lambda (col-id value)
                          (when (> value 0)
                            (push col-id neighbors)))
                      (matrix graph) node))
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'(lambda (id)
                    (lookup-node graph id))
                (nreverse neighbors)))))

graph-utils:map-nodes


;;given a node, return the most similar nodes in the graph
(defparameter *th* (get-type-hierarchy *propbank-learned-cxn-inventory*))



(pprint (similar-nodes-cosine 'SEND.01-898
                      (graph-utils::graph *th*)))

(node-p 'propbank-eng::SEND.01-446 *th*)
fcg::unify-atom

(loop for node in (graph-utils:list-nodes (graph-utils::graph *th*))
      for i from 1
      do (format t "~a~%" i))