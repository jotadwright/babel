;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additions to graph-utils ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :graph-utils)

(export '(find-cheapest-path find-undirected-path
          find-weight-of-path coherence
          connected?
          neighbours?))

(defgeneric find-cheapest-path (graph node-1 node-2)
  (:documentation "Dijkstra's algorithm for finding the shortest path between two nodes."))
            
(defmethod find-cheapest-path ((graph undirected-typed-graph) (n1 integer) (n2 integer))
  (let ((nodes (node-ids graph)))
    (let ((distances (mapcar (lambda (n) (cons n most-positive-fixnum)) nodes))
          (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
            (setf distances (sort distances '< :key 'cdr))
            (let ((next (first (remove-if-not (lambda (d)
                                                (member (car d) nodes))
                                              distances))))
              (when (= (cdr next) most-positive-fixnum)
                (return nil))
              (when (= (car next) n2)
                (return-from find-cheapest-path
                  (nreverse (reconstruct-path previous n2))))
              (setq nodes (remove (car next) nodes))
              (dolist (edge-type-and-neighbor
                       (if (directed? graph)
                         (outbound-neighbors graph (car next))
                         (neighbors graph (car next))))
                (let* ((neighbor (cdr edge-type-and-neighbor))
                       (distance (+ (cdr (assoc (car next) distances)) (edge-weight graph (car next) neighbor))))
                  (when (< distance (cdr (assoc neighbor distances)))
                    (setf (cdr (assoc neighbor distances)) distance
                          (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod find-cheapest-path ((graph undirected-typed-graph) n1 n2)
  (find-cheapest-path graph
		      (gethash n1 (nodes graph))
		      (gethash n2 (nodes graph))))

(defgeneric find-weight-of-path (graph path)
  (:documentation "Returns the total weight of a path."))

(defmethod find-weight-of-path ((graph undirected-typed-graph) path)
  (loop for (n1 n2) in path summing (edge-weight graph n1 n2)))

(defgeneric find-undirected-path (graph node1 node-2)
  (:documentation "Returns any path between two nodes, doesn't take into account direction of links"))

(defmethod find-undirected-path ((graph undirected-typed-graph) (n1 integer) (n2 integer))
  (let* ((nodes (node-ids graph)))
    (let ((distances (mapcar (lambda (n) (cons n most-positive-fixnum)) nodes))
	  (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
            (setf distances (sort distances '< :key 'cdr))
            (let ((next (first (remove-if-not (lambda (d)
                                                (member (car d) nodes))
                                              distances))))
              (when (= (cdr next) most-positive-fixnum)
                (return nil))
              (when (= (car next) n2)
                (return-from find-undirected-path
                  (nreverse (reconstruct-path previous n2))))
              (setq nodes (remove (car next) nodes))
              (dolist (edge-type-and-neighbor (neighbors graph (car next)))
                (let* ((neighbor (cdr edge-type-and-neighbor))
                       (distance (1+ (cdr (assoc (car next) distances)))))
                  (when (< distance (cdr (assoc neighbor distances)))
                    (setf (cdr (assoc neighbor distances)) distance
                          (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod find-undirected-path ((graph undirected-typed-graph) (n1 symbol) (n2 symbol))
  "finds nodes by node-ids and calls method for that."
  (find-undirected-path graph
                        (gethash n1 (nodes graph))
                        (gethash n2 (nodes graph))))

(defmethod coherence ((graph undirected-typed-graph))
  (let ((in-node-counter 0)
        (out-node-counter 0))
    (loop with matrix = (matrix (matrix graph))
          for node being the hash-keys of matrix
          do
          (incf in-node-counter)
          (loop with outgoing-edges =  (gethash node matrix)
                for out-node being the hash-keys of outgoing-edges
                when (< (gethash out-node outgoing-edges) 1.0)
                do
                (incf out-node-counter)))
    (if (> out-node-counter 0)
      (/ out-node-counter in-node-counter)
      1)))

(defmethod connected? ((graph undirected-typed-graph) (n1 integer) (n2 integer))
  ;; Check if there is a path from n1 to n2 using BFS
  (let* ((start-node n1)
         (dest-node n2)
         (queue (list start-node))
         (visited (list start-node)))
    (loop while queue
          for node = (pop queue)
          if (eq node dest-node)
          do (return t)
          else do
          (let ((neighbor-nodes (neighbors graph node)))
            (loop for (nil . neighbor-node) in neighbor-nodes
                  unless (member neighbor-node visited)
                  do
                  (setf queue (append queue (list neighbor-node)))
                  (setf visited (append visited (list neighbor-node))))))))

(defmethod connected? ((graph undirected-typed-graph) n1 n2)
  (connected? graph
              (gethash n1 (nodes graph))
              (gethash n2 (nodes graph))))


(defmethod neighbours? ((graph undirected-typed-graph) (n1 integer) (n2 integer) &key edge-type)
  (let ((matrix (gethash edge-type (matrix graph))))
    (when (and (sparse-array? matrix)
               (numberp (saref matrix n1 n2)))
      (second
       (multiple-value-list
        (saref matrix n1 n2))))))

(defmethod neighbours? ((graph undirected-typed-graph) n1 n2 &key edge-type)
  (neighbours? graph
               (gethash n1 (nodes graph))
               (gethash n2 (nodes graph))
               :edge-type edge-type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Hierarchy -> S-dot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(categorial-network->s-dot))

(defun rgb->rgbhex (rgb)
  "Converts a RGB [0,255] value to an 8-bit hexadecimal string."
  (format nil "#~{~2,'0X~}"
	  (mapcar #'(lambda (x) (round x)) rgb)))

(defun shade-of-gray (edge-weight)
  "Determine the shade of gray for the edge,
   depending on the edge weight"
  (let* ((rgb-val (* edge-weight 211))
         (rgb (loop repeat 3 collect rgb-val))
         (hex (rgb->rgbhex rgb)))
    hex))

(defun categorial-network-node->s-dot (node-name node-id &key colors sizes)
  "A node in the type hierarchy -> s-dot"
  `(s-dot::node
    ,(append `((s-dot::id ,(format nil "~a" node-id))
               (s-dot::label ,(format nil "~(~a~)" node-name))
               (s-dot::fontname "Helvetica")
               (s-dot::fontsize "10.0")
               (s-dot::fillcolor ,(if (hash-table-p colors)
                                    (gethash node-name colors)
                                    "#FFFF00"))
               (s-dot::shape ,(if (hash-table-p sizes)
                                "box" "ellipse"))
               (s-dot::style "filled"))
             (when (hash-table-p sizes)
               `((s-dot::width ,(format nil "~F"
                                        (gethash node-name sizes)))
                 (s-dot::fixedsize "true")
                 (s-dot::fontsize ,(format nil "~D"
                                           (truncate
                                            (* 10 (gethash node-name sizes))))))))))

(defun categorial-network-edge->s-dot (from to &key weight directedp colored-edges-0-1)
  "An edge in the type hierarchy -> s-dot"
  `(s-dot::edge
    ((s-dot::from ,(format nil "~a" from))
     (s-dot::to ,(format nil "~a" to))
     (s-dot::dir ,(if directedp "forward" "none"))
     (s-dot::fontsize "10.0")
     (s-dot::label ,(if weight (format nil "~,2f" weight) ""))
     (s-dot::fontcolor ,(if (and colored-edges-0-1 weight)
                          (shade-of-gray weight); scale RGB gray between (0,0,0) and (211, 211, 211)
                          "#000000"))
     (s-dot::color ,(if (and colored-edges-0-1 weight)
                      (shade-of-gray weight)
                      "#000000"))
     (s-dot::style ,(if (and colored-edges-0-1 weight)
                      (cond ((and (< weight 0.1) (>= weight 0.0)) "bold")
                            ((and (<= weight 1) (> weight 0.9)) "dotted")
                            (t "filled"))
                      "filled")))))

(defmethod categorial-network->s-dot ((graph undirected-typed-graph)
                                  &key weights? colors sizes colored-edges-0-1)
  "Make a dot representation of the graph using s-dot.
   weights? : show the weights on the edges or not.
   colors : a hash table that stores a color for each node
   sizes : a hash table that stores a size for each node
   colored-edges-0-1 : use shades of gray and edge style to indicate
   if the weight of the edge is close to 0 or 1"
   (let* ((graph-properties '((s-dot::fontcolor "#000000")
                              (s-dot::fontsize "10.0")
                              (s-dot::fontname "Helvetica")
                              (s-dot::rankdir "LR")))
         (all-node-names (graph-utils::list-nodes graph))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes graph))
                collect id))
         (all-edges (graph-utils::list-edges graph :nodes-as-ids t))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (categorial-network-node->s-dot node-name node-id
                                                    :colors colors
                                                    :sizes sizes)))
         (s-dot-edges
          ;; we are in an undirected graph
          (loop for (from-id to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight graph from-id to-id)
                when (> from-id to-id)
                collect (categorial-network-edge->s-dot from-id to-id
                                                    :weight (if weights? edge-weight nil)
                                                    :directedp (graph-utils::directed? graph)
                                                    :colored-edges-0-1 colored-edges-0-1))))
     `(s-dot::graph ,graph-properties
                    ,@s-dot-nodes
                    ,@s-dot-edges)))