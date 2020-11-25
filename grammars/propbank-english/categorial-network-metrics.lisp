(in-package :graph-utils)

(defmethod cosine ((node-1 symbol) (node-2 symbol) (graph graph))
  "Returns the cosine between two sparse vectors."
  (cosine (lookup-node graph node-1) (lookup-node graph node-2) graph))

(defmethod cosine ((node-1 integer) (node-2 integer) (graph graph))
  "Returns the cosine between two sparse vectors."
  (let ((denominator (* (norm node-1 graph) (norm node-2 graph))))
    (if (= 0 denominator)
      0
      (/ (dot-product node-1 node-2 graph)
         denominator))))

;(cosine '4  '4 (graph propbank-english::*th*))


(defmethod dot-product ((node-1 symbol) (node-2 symbol) (graph graph))
  "Returns the dot product of two sparse vectors."
  (dot-product (lookup-node graph node-1) (lookup-node graph node-2) graph))

(defmethod dot-product ((node-1 integer) (node-2 integer) (graph graph))
  (let* ((vector-node-1 (node-vector node-1 graph))
         (vector-node-2 (node-vector node-2 graph))
         (count-vector-node-1 (hash-table-count vector-node-1))
         (count-vector-node-2 (hash-table-count vector-node-2))
         (longest-vector (if (> count-vector-node-1 count-vector-node-2) vector-node-1 vector-node-2))
         (shortest-vector (if (> count-vector-node-1 count-vector-node-2) vector-node-2 vector-node-1)))
    
    (loop for id being the hash-keys in shortest-vector using (hash-value v-1)
          for v-2 = (gethash id longest-vector)
          when v-2
          sum (* v-1 v-2))))

;(dot-product 'propbank-english::abridge.01-3  'propbank-english::abridge.01-3 (graph propbank-english::*th*))

  
(defmethod norm ((node symbol) (graph graph))
  "Returns the eucledean norm of a sparse vector."
  (norm (lookup-node graph node) graph))

(defmethod norm ((node integer) (graph graph))
  "Returns the eucledean norm of a sparse vector."
  (sqrt (sum-square-sarray-row (matrix graph) node)))

;; (dot-product 0 8 (graph propbank-english::*th*))

(defmethod node-vector ((node symbol) (graph graph) (edge-type t))
  "Returns the sparse vector for a given node."
  (node-vector (lookup-node graph node) graph edge-type))

(defmethod node-vector ((node integer) (graph graph) (edge-type t))
  "Returns the sparse vector for a given node."
    (gethash node (matrix (gethash edge-type (matrix graph)))))

(node-vector 'propbank-english::send.01-20 (graph *th*) nil)

(defun closest-nodes (node graph)
  (loop for other-node being the hash-keys in (nodes graph)
        collect (cons other-node (cosine node other-node graph)) into nodes-with-sim
        finally return (sort nodes-with-sim #'> :key #'cdr)))

;; (defparameter *th* (type-hierarchies:get-type-hierarchy propbank-english::*propbank-learned-cxn-inventory*))
;; (pprint (closest-nodes 'propbank-english::send.01-20 *th*))
