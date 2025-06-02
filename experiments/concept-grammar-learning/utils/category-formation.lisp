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

(defun make-categories (agent)
  (let* ((ontology (ontology agent))
         (similarity-table (calculate-all-similarities-of-concepts agent))
         (nr-of-cats (get-configuration (experiment agent) :nr-of-categories))
         (all-combinations (loop for concepts-and-similarities in (hash-values similarity-table)
                                 for possible-concepts = (loop for (concept-id . similarity) in concepts-and-similarities
                                                               for concept = (gethash concept-id (get-data ontology 'concepts))
                                                               when (> similarity (get-configuration (experiment agent) :category-strategy-threshold))
                                                                 collect concept)
                                 collect possible-concepts))
         (all-relevant-combinations (mapcar #'(lambda (x) (when (> (length x) 1) x)) all-combinations))
         (all-relevant-combinations-no-duplicates (remove nil (remove-duplicates all-relevant-combinations :test #'equal)))
         (categories (loop for concepts in all-relevant-combinations-no-duplicates 
                           collect (make-instance 'category :concepts concepts))))
    categories))

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



(in-package :fcg)

(defmethod categorial-network-based-on-neighbours-of-node ((categorial-network categorial-network)
                                                           &key nodes included-nodes)
  (let* ((new-categorial-network (make-instance 'categorial-network
                                                :graph (graph-utils:make-undirected-typed-graph)))
         (new-graph (graph new-categorial-network))
         (graph (graph categorial-network)))
    (loop for node in nodes
          do (let* ((neighbours (graph-utils::neighbors graph node :return-ids? nil))
                    (neighbour-ids (loop for neighbour in neighbours
                                         for id = (gethash neighbour (graph-utils::nodes graph))
                                         collect id))
                    (all-edges (graph-utils::list-edges graph :nodes-as-ids t)))
               (graph-utils:add-node new-graph node)
               (loop for neighbour in neighbours
                     for neighbour-id = (gethash neighbour (graph-utils::nodes graph)) 
                     for edges-of-neighbour = (loop for edge in all-edges
                                                    when (= (first edge) neighbour-id)
                                                      collect (second edge))
                       when (if included-nodes
                              (find neighbour included-nodes)
                              t)
                       do (graph-utils:add-node new-graph neighbour)
                        and do (graph-utils:add-edge new-graph node neighbour)
                        and do (loop for edge-for-neighbour in edges-of-neighbour
                                    for edge-name = (graph-utils:lookup-node graph edge-for-neighbour)
                                    when (find edge-name neighbours)
                                      do (graph-utils:add-edge new-graph edge-name neighbour)))))
    new-categorial-network))

(defun draw-categorial-network-node-and-neighbours (cxn-inventory lexicals &key (included nil) (render-program "dot"))
  (let* ((categorial-network (categorial-network cxn-inventory))
         (small-categorial-network (categorial-network-based-on-neighbours-of-node categorial-network
                                                                                   :nodes lexicals
                                                                                   :included-nodes included)))
    (add-element (make-html small-categorial-network :render-program render-program))))




#|
(fcg::draw-categorial-network-node-and-neighbours
 (clg::grammar (clg::learner clg::*experiment*))
 (list 'FCG::red-2
       'FCG::purple-2
       'FCG::gray-2
       'FCG::yellow-2
       'FCG::green-2
       'FCG::blue-2
       'FCG::metal-2
       'FCG::rubber-2
       'FCG::large-2
       'FCG::small-2
       'FCG::cubes-2
       'FCG::spheres-2
       'FCG::cylinders-2)
 :included (list 'fcg::how-many-?a-?z-?y-?x-are-there-\(?a\)-1
       'fcg::how-many-?a-?z-?y-?x-are-there-\(?x\)-1
       'fcg::how-many-?a-?z-?y-?x-are-there-\(?z\)-1
       'fcg::how-many-?a-?z-?y-?x-are-there-\(?y\)-1
       'fcg::how-many-?z-?y-?x-are-there-\(?y\)-1
       'fcg::how-many-?z-?y-?x-are-there-\(?z\)-1
       'fcg::how-many-?z-?y-?x-are-there-\(?x\)-1
       'fcg::how-many-?y-?x-are-there-\(?y\)-1
       'fcg::how-many-?y-?x-are-there-\(?x\)-1)

 :render-program "fdp"
 )



  |#
