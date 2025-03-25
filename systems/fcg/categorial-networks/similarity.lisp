(in-package :graph-utils)

(export '(weighted-graph-cosine-similarity
          graph-cosine-similarity
          euclidean-distance
          common-neighbours
          similar-nodes-weighted-cosine
          similar-nodes-cosine
          similar-nodes-euclidean
          neighbour-categories))


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
  (let ((neighbours-node-1 (mapcar #'cdr (graph-utils::neighbors graph node-1)))
        (neighbours-node-2 (mapcar #'cdr (graph-utils::neighbors graph node-2))))
    (remove-duplicates (intersection neighbours-node-1 neighbours-node-2))))

(defun graph-cosine-similarity (node-1 node-2 graph)
  "(GCS i j) = (/ (number-common-neighbours i j) (sqrt (* (degree i) (degree j))))"
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let*
          ((common-neighbours (common-neighbours node-id-1 node-id-2 graph))
           (degree-node-1 (length (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-1))))) ;; degree is how many unique neighbours the node has in the graph, here we take the number of neighbours
           (degree-node-2 (length (remove-duplicates (mapcar #'cdr  (graph-utils::neighbors graph node-id-2)))))
           (denominator (sqrt (* degree-node-1 degree-node-2))))
        (if (> denominator 0)
          (/ (length common-neighbours) denominator) 0)))))

(defun weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for degree-1 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-1))) ;; take the edge weight of all neighbouring edges
                                                   collect (square (graph-utils:edge-weight graph  degree-1 node-id-1)))) ;; take the square of the weight, and sum it
                                  (reduce '+ (loop for degree-2 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-2)))
                                                   collect (square (graph-utils:edge-weight graph  degree-2 node-id-2))))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0)))))

(defun weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for degree-1 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-1))) ;; take the edge weight of all neighbouring edges
                                                   collect (square (graph-utils:edge-weight graph  degree-1 node-id-1)))) ;; take the square of the weight, and sum it
                                  (reduce '+ (loop for degree-2 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-2)))
                                                   collect (square (graph-utils:edge-weight graph  degree-2 node-id-2))))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0)))))


(defun weighted-graph-cosine-similarity-from-node-ids (node-id-1 node-id-2 graph)
    (when (and node-id-1 node-id-2)
      (let ((denominator (sqrt (* (reduce '+ (loop for degree-1 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-1))) ;; take the edge weight of all neighbouring edges
                                                   collect (square (graph-utils:edge-weight graph  degree-1 node-id-1)))) ;; take the square of the weight, and sum it
                                  (reduce '+ (loop for degree-2 in (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-2)))
                                                   collect (square (graph-utils:edge-weight graph  degree-2 node-id-2))))))))
        (if (> denominator 0)
          (/ (reduce '+ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                              collect (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                                         (graph-utils:edge-weight graph node-id-2 common-neighbour))))
             denominator)
          0))))

(defun square (x)
  (* x x))

(defun get-node-type (node-id graph)
  "look up the type of the node in the matrix"
  
  (loop for node-type being the hash-keys of (node-types graph)
          using (hash-value node-ids)
        when (and node-type (find node-id node-ids))
          return node-type))

(defun similar-neighbour-nodes-weighted-cosine (source-node target-node graph)
  "loop through neighbours of source-node, sort them similarity with target-node, filter on type of target node"
  (loop with source-node-id = (graph-utils:lookup-node graph source-node)
        with target-node-id = (graph-utils:lookup-node graph target-node)
        with target-node-type = (get-node-type target-node-id graph)
        for neighbour-id in (remove-duplicates (neighbours-by-node-type graph source-node-id :node-type target-node-type))
        for weighted-node-similarity = (weighted-graph-cosine-similarity-from-node-ids neighbour-id target-node-id graph)
        collect (cons (graph-utils:lookup-node graph neighbour-id) weighted-node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))



(defmethod neighbours-by-node-type ((graph undirected-node-and-edge-typed-graph) (node integer)
                               &key node-type &allow-other-keys)
  "Returns neigbours by node type."
  (let ((neighbours nil))
    (flet ((find-neighbours (matrix etype)
             (map-sarray-col (lambda (row-id value)
                               (when (and
                                      etype ;;edge type needs to be non-zero!!!!
                                      (equal (get-node-type row-id graph) node-type)
                                      (>= value 0)) ;; edge type value needs to be larger than 0
                                 (push row-id neighbours)))
                             matrix node)))
      (maphash (lambda (etype matrix)
                 (find-neighbours matrix etype))
               (matrix graph))
      neighbours)))


(defun similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (weighted-graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))

(defun similar-nodes-weighted-cosine-same-node-type (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop with node-type = (get-node-type (lookup-node graph node) graph)
        for other-node-id in (remove-duplicates (gethash node-type (node-types graph)))
        for other-node = (lookup-node graph other-node-id)
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

(defun pre-compute-cosine-similarities (graph node-type)
  (loop for node-id in (remove-duplicates (gethash node-type (node-types graph)))
        for node = (lookup-node graph node-id)
        for similarities = (graph-utils::similar-nodes-weighted-cosine-same-node-type node graph) ;; todo: skip permutations?
        do (setf (gethash node (graph-utils::node-similarities graph)) similarities)))
