(ql:quickload :grammar-learning)

(in-package :grammar-learning)

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
      (let*
        ((common-neighbours (common-neighbours node-id-1 node-id-2 graph))
        (degree-node-1 (length (remove-duplicates (graph-utils::neighbors graph node-id-1))))
        (degree-node-2 (length (remove-duplicates (graph-utils::neighbors graph node-id-2))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Load Constructions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(setf *fcg-constructions* (cl-store:restore (babel-pathname :directory '("systems" "grammar-learning" "storage") :name "fcg-cxns-10k" :type "store")))
;;
;; (wi:add-element (make-html (get-type-hierarchy *fcg-constructions*) :weights? t :colored-edges-0-1 t))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Graph Cosine Similarity ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                         'TYPE-HIERARCHIES::RED-59008
                         (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                                  'TYPE-HIERARCHIES::RED-59008
                                  (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                         'TYPE-HIERARCHIES::PURPLE-29431
                         (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                                  'TYPE-HIERARCHIES::PURPLE-29431
                                  (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                         'TYPE-HIERARCHIES::RUBBER-43779
                          (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                                  'TYPE-HIERARCHIES::RUBBER-43779
                                  (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                         'TYPE-HIERARCHIES::SIZE-56511
                         (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::RED-59008
                            'TYPE-HIERARCHIES::SIZE-56511
                            (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(graph-cosine-similarity 'TYPE-HIERARCHIES::LARGE-39123
                         'TYPE-HIERARCHIES::TINY-69108
                         (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::LARGE-39123
                            'TYPE-HIERARCHIES::TINY-69108
                            (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(graph-cosine-similarity 'TYPE-HIERARCHIES::BIG-90007
                         'TYPE-HIERARCHIES::LARGE-39123
                         (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

(weighted-graph-cosine-similarity 'TYPE-HIERARCHIES::BIG-90007
                             'TYPE-HIERARCHIES::LARGE-39123
                             (graph-utils:graph (get-type-hierarchy *fcg-constructions*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Finding Similar Nodes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pprint (similar-nodes-cosine 'TYPE-HIERARCHIES::RUBBER-43779 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

(pprint (similar-nodes-weighted-cosine 'TYPE-HIERARCHIES::RUBBER-43779 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

(pprint (similar-nodes-cosine 'TYPE-HIERARCHIES::COLOR-56873 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

(pprint (similar-nodes-weighted-cosine 'TYPE-HIERARCHIES::COLOR-56873 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

(pprint (similar-nodes-cosine 'TYPE-HIERARCHIES::RED-59008 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

(pprint (similar-nodes-weighted-cosine 'TYPE-HIERARCHIES::RED-59008 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Finding Most Visited Neigbors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pprint (neighbour-categories 'TYPE-HIERARCHIES::RED-59008 (graph-utils:graph (get-type-hierarchy *fcg-constructions*))))

;; als we gaan van what is the color of the X Y Z en what is the color of the A, hoe detecteren we dat Z gelijk is aan A?
;; probeer eens de cosine similarity tussen what is the color of the X Y Z en what is the color of the X, deze zou heel hoog moeten zijn



;; 